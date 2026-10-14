(ns dk.simongray.drop-in-search.segment
  "The segments of an index, on both platforms: the postings of some of
  its documents in sorted arrays, as a segment of Lucene's index holds
  them, https://lucene.apache.org/core/9_11_1/core/"
  (:require [dk.simongray.drop-in-search.analysis :as analysis]
            [dk.simongray.drop-in-search.postings :as postings
             #?@(:cljs [:refer [Cursor Out Writer]])])
  #?(:clj (:import [dk.simongray.drop_in_search.postings Cursor Out Writer]
                   [java.util Arrays])))

(defn ^:no-doc make-ints
  "A new array of `n` ints, all 0."
  [n]
  #?(:clj  (int-array n)
     :cljs (js/Int32Array. n)))

(defn ^:no-doc make-doubles
  "A new array of `n` doubles, all 0."
  [n]
  #?(:clj  (double-array n)
     :cljs (js/Float64Array. n)))

(defn ^:no-doc as-ints
  "The ints `xs`, a vector or an array, as an array."
  [xs]
  (if (vector? xs)
    #?(:clj  (int-array xs)
       :cljs (js/Int32Array. (into-array xs)))
    xs))

(defn ^:no-doc as-doubles
  "The numbers of the vector `xs` as an array of doubles."
  [xs]
  #?(:clj  (double-array xs)
     :cljs (js/Float64Array. (into-array xs))))

;; RFC 4648, section 4, which takes a third of the room of a vector of the
;; numbers of the bytes in EDN, and reads back without a number for each
(defn- base64
  "The bytes `bs` as base64 text."
  [bs]
  #?(:clj  (.encodeToString (java.util.Base64/getEncoder) ^bytes bs)
     :cljs (->> (range 0 (.-length bs) 8192)
                (map #(.apply js/String.fromCharCode
                              nil
                              (.subarray bs % (+ % 8192))))
                (apply str)
                js/btoa)))

(defn- as-bytes
  "The bytes `x`, an array, or base64 text of them, as an array."
  [x]
  (if (string? x)
    #?(:clj  (.decode (java.util.Base64/getDecoder) ^String x)
       :cljs (let [binary (js/atob x)
                   bs     (js/Uint8Array. (.-length binary))]
               (dotimes [i (.-length binary)]
                 (aset bs i (.charCodeAt binary i)))
               bs))
    x))

(defn- as-objects
  "The values `xs`, a vector or an array, as an array."
  [xs]
  (if (vector? xs)
    (to-array xs)
    xs))

(defn- as-vector
  "The values of the array `arr` as a vector."
  [arr]
  #?(:clj  (vec arr)
     :cljs (vec (js/Array.from arr))))

(defn ^:no-doc slice
  "The ints of the array `arr` from `from` to `to`, as an array of their
  own."
  [arr from to]
  #?(:clj  (Arrays/copyOfRange ^ints arr (int from) (int to))
     :cljs (.slice arr from to)))

(defn ^:no-doc trimmed-doubles
  "The first `n` doubles of the array `arr`, as an array of their own."
  [arr n]
  #?(:clj  (Arrays/copyOf ^doubles arr (int n))
     :cljs (.slice arr 0 n)))

(defn ^:no-doc copy-ints!
  "Copy `n` ints of the array `src` from `from` into the array `dst` at
  `to`."
  [src from dst to n]
  #?(:clj  (System/arraycopy src (int from) dst (int to) (int n))
     :cljs (.set dst (.subarray src from (+ from n)) to)))

(defn ^:no-doc lower-bound
  "The first index from `from` to `to` of the sorted ints `arr` that holds
  at least `x`, or `to`."
  ^long [^ints arr ^long from ^long to ^long x]
  (loop [lo from hi to]
    (if (< lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (if (< (aget arr mid) x)
          (recur (inc mid) hi)
          (recur lo mid)))
      lo)))

;; Exponential search, Bentley and Yao 1976, so that a short list of
;; documents walks through a long one in few steps
(defn ^:no-doc seek
  "The first index from `from` to `to` of the sorted ints `arr` that holds
  at least `x`, or `to`, found in steps that double from `from`."
  ^long [^ints arr ^long from ^long to ^long x]
  (loop [lo from step 1]
    (let [hi (+ lo step)]
      (if (and (< hi to)
               (< (aget arr hi) x))
        (recur hi (* 2 step))
        (lower-bound arr lo (min hi to) x)))))

(defn ^:no-doc term-bound
  "The first index of the sorted array `terms` whose term is at least
  `term`, or the count of `terms`."
  ^long [terms term]
  (let [^objects terms terms]
    (loop [lo 0 hi (alength terms)]
      (if (< lo hi)
        (let [mid (quot (+ lo hi) 2)]
          (if (neg? (compare (aget terms mid) term))
            (recur (inc mid) hi)
            (recur lo mid)))
        lo))))

(defn ^:no-doc term-number
  "The index of `term` in the sorted array `terms`, or -1."
  ^long [terms term]
  (let [^objects terms terms
        i              (term-bound terms term)]
    (if (and (< i (alength terms))
             (= term (aget terms i)))
      i
      -1)))

;; A segment holds the postings of some of the documents of an index in
;; arrays, which take far less memory than maps:
;;
;; - ids and stored, those of each document by its number
;; - terms, in order, and doc-start, where the postings of each start in
;;   the count of them all, with the end of the last
;; - postings, those of each term in bytes, as the postings namespace has
;;   them, and term-start, where those of each term start, with the end
;;   of the last
;; - lengths, the positions of each field of each document, width of them
;;   to a document, and field-stats, the documents with each field and
;;   their positions in all, by field number
;; - unpositioned, the numbers of the fields without positions, and
;;   positioned, an array of a 1 for each field with them and a 0 for
;;   each without
;; - deleted, the numbers of the documents that are removed
;; - name, that of its file in a store, which an equal segment can have
;;   another of
(declare contents segment-data)

(deftype Segment [ids stored terms doc-start term-start postings lengths
                  ^long width field-stats unpositioned positioned deleted
                  name]
  #?@(:clj
      [Object
       (equals [this other]
         (and (instance? Segment other)
              (= (contents this) (contents other))))
       (hashCode [_]
         (hash [ids deleted]))]

      :cljs
      [IEquiv
       (-equiv [this other]
         (and (instance? Segment other)
              (= (contents this) (contents other))))

       IHash
       (-hash [_]
         (hash [ids deleted]))

       IPrintWithWriter
       (-pr-writer [this writer opts]
         (-pr-writer (segment-data this) writer opts))]))

#?(:clj
   (defmethod print-method Segment
     [segment ^java.io.Writer writer]
     (print-method (segment-data segment) writer)))

(defn- parts
  "The fields of `segment` as a map, with its :name as it is, a delay
  until something needs it."
  [^Segment segment]
  {:ids          (.-ids segment)
   :stored       (.-stored segment)
   :terms        (.-terms segment)
   :doc-start    (.-doc-start segment)
   :term-start   (.-term-start segment)
   :postings     (.-postings segment)
   :lengths      (.-lengths segment)
   :field-stats  (.-field-stats segment)
   :unpositioned (.-unpositioned segment)
   :deleted      (.-deleted segment)
   :name         (.-name segment)})

(defn- contents
  "What `segment` holds as plain data, a map of its fields but its name,
  with its arrays as vectors, and its postings as base64 text."
  [segment]
  (reduce #(update %1 %2 as-vector)
          (-> (parts segment)
              (dissoc :name)
              (update :postings base64))
          [:terms :doc-start :term-start :lengths]))

(defn ^:no-doc segment-name
  "The name of the file of `segment` in a store."
  [^Segment segment]
  (force (.-name segment)))

(defn- segment-data
  "The `segment` as plain data, as it prints: its contents and its name."
  [segment]
  (assoc (contents segment) :name (segment-name segment)))

;; FNV-1a of Fowler, Noll and Vo, over the ints of arrays rather than
;; bytes, in two lanes that read them in opposite orders
(defn- fnv
  "The 32-bit FNV-1a hash `h` with the int `x` mixed in."
  ^long [^long h ^long x]
  #?(:clj  (bit-and (* (bit-xor h (bit-and x 0xFFFFFFFF)) 16777619)
                    0xFFFFFFFF)
     :cljs (unsigned-bit-shift-right (js/Math.imul (bit-xor h x) 16777619)
                                     0)))

(defn- fnv-ints
  "The hash `h` with the count and the ints of the array `arr` mixed in,
  in order, or the other way round when `reversed?`."
  ^long [^long h ^ints arr reversed?]
  (let [n (alength arr)]
    (loop [i 0 h (fnv h n)]
      (if (< i n)
        (recur (inc i) (fnv h (aget arr (if reversed? (- n i 1) i))))
        h))))

(defn- fnv-bytes
  "The hash `h` with the count and the bytes `bs` mixed in, in order, or
  the other way round when `reversed?`."
  ^long [^long h bs reversed?]
  (let [n (postings/byte-count bs)]
    (loop [i 0 h (fnv h n)]
      (if (< i n)
        (recur (inc i) (fnv h (postings/byte-at bs (if reversed? (- n i 1) i))))
        h))))

(defn- hex
  "The number `h`, below 2 to the power of 32, as eight hex digits."
  [h]
  #?(:clj  (format "%08x" h)
     :cljs (.padStart (.toString h 16) 8 "0")))

(defn- content-name
  "The name of the file of a segment with the parts in `m`, from a hash of
  all that it holds, so that one that holds anything else has another."
  [{:keys [ids stored terms doc-start term-start postings lengths
           unpositioned]}]
  (let [arrays [doc-start term-start lengths
                (as-ints (mapv hash terms))
                (as-ints [(hash ids) (hash stored) (hash unpositioned)])]
        lane   (fn [reversed?]
                 (fnv-bytes (reduce #(fnv-ints %1 %2 reversed?)
                                    2166136261
                                    arrays)
                            postings
                            reversed?))]
    (str "segment-" (hex (lane false)) (hex (lane true)) ".ciff")))

(defn ^:no-doc positioned-fields
  "An array of a 1 for each of `width` fields, and a 0 for those numbered
  in `unpositioned`, which have no positions."
  [width unpositioned]
  (let [^ints flags (make-ints width)]
    (dotimes [no width]
      (aset flags no (if (contains? unpositioned no) 0 1)))
    flags))

(defn ^:no-doc segment-of
  "The segment of `m`, a map of its fields, whose arrays can be vectors
  and its postings base64 text. Without a :name, its name is a hash of
  what it holds, worked out once it's needed."
  [{:keys [ids stored terms doc-start term-start postings lengths
           field-stats unpositioned deleted name]
    :as   m}]
  (when-not postings
    (throw (ex-info (str "A segment has no postings, e.g. one printed by an"
                         " earlier version, so index its documents again")
                    {:keys (set (keys m))})))
  (let [arrays {:ids          (vec ids)
                :stored       (vec stored)
                :terms        (as-objects terms)
                :doc-start    (as-ints doc-start)
                :term-start   (as-ints term-start)
                :postings     (as-bytes postings)
                :lengths      (as-ints lengths)
                :unpositioned (set unpositioned)}]
    (->Segment (:ids arrays)
               (:stored arrays)
               (:terms arrays)
               (:doc-start arrays)
               (:term-start arrays)
               (:postings arrays)
               (:lengths arrays)
               (count field-stats)
               (vec field-stats)
               (:unpositioned arrays)
               (positioned-fields (count field-stats) (:unpositioned arrays))
               (set deleted)
               (or name (delay (content-name arrays))))))

(defn ^:no-doc size
  "The number of documents in `segment`, those removed too."
  [^Segment segment]
  (count (.-ids segment)))

(defn ^:no-doc live
  "The number of documents in `segment` that aren't removed."
  [^Segment segment]
  (- (size segment) (count (.-deleted segment))))

(defn ^:no-doc term-numbers
  "The numbers in `segment` of those of `terms` that it holds."
  [^Segment segment terms]
  (into []
        (comp (map #(term-number (.-terms segment) %))
              (remove neg?))
        terms))

(defn ^:no-doc doc-frequency
  "The number of documents of `segment` that hold the term numbered `t`."
  ^long [^Segment segment ^long t]
  (let [^ints starts (.-doc-start segment)]
    (- (aget starts (inc t)) (aget starts t))))

(defn ^:no-doc postings-count
  "The number of postings in `segment` of the terms numbered `ts`."
  [segment ts]
  (reduce + 0 (map #(doc-frequency segment %) ts)))

(defn ^:no-doc cursor
  "A cursor before the first posting in `segment` of the term numbered
  `t`."
  [^Segment segment t]
  (postings/cursor (.-postings segment)
                   (aget ^ints (.-term-start segment) (int t))
                   (doc-frequency segment t)
                   (.-width segment)
                   (.-positioned segment)))

;; Lucene's ConjunctionDISI: the postings of a term and the candidates are
;; both in order, so they meet in one walk, each side skipping ahead
(defn ^:no-doc each-held!
  "Call `f` with the index of each of the `candidates`, a sorted array of
  numbers of documents, whose document the cursor `c` holds, with the
  cursor at its posting."
  [^Cursor c ^ints candidates f]
  (let [n (alength candidates)]
    (loop [j 0]
      (when (< j n)
        (let [d (aget candidates j)
              e (.advance c d)]
          (cond
            (== d e)                (do (f j)
                                        (recur (inc j)))
            (== e postings/no-more) nil
            :else                   (recur (seek candidates j n e))))))))

(defn ^:no-doc with-removed
  "The `segment` with the documents numbered `nos` removed."
  [segment nos]
  (segment-of (update (parts segment) :deleted into nos)))

(defn- analyze
  "The terms of the field value `v` by position: a string tokenized, a
  vector taken as terms already, the text of each value of any other
  collection tokenized, or else the text of `v`, e.g. of a number, a
  keyword or an instant. The `term-fn`, if any, maps each term of a text."
  [v term-fn]
  (cond
    (string? v) (analysis/mapped term-fn (analysis/tokens v))
    (vector? v) v
    (coll? v)   (into [] (mapcat #(analysis/mapped
                                   term-fn
                                   (analysis/tokens (analysis/text %))))
                      v)
    :else       (analysis/mapped term-fn (analysis/tokens (analysis/text v)))))

(defn ^:no-doc numbered
  "The `numbers` of the fields of an index, a map of field to number, with
  a number for each new field of `analyzed`."
  [numbers analyzed]
  (reduce (fn [numbers field]
            (cond
              (contains? numbers field)
              numbers

              ;; the positions of a field start at its number times
              ;; positions-per-field, in an int
              (< (count numbers)
                 (quot 2147483647 postings/positions-per-field))
              (assoc numbers field (count numbers))

              :else
              (throw (ex-info "An index holds no more fields"
                              {:field field}))))
          numbers
          (keys analyzed)))

(defn- placed
  "The transient map `acc` of terms to positions with `term` at `at`. A
  term has one position, or a vector of them."
  [acc term at]
  (let [seen (get acc term)]
    (assoc! acc term (cond
                       (nil? seen)    at
                       (vector? seen) (conj seen at)
                       :else          [seen at]))))

(defn- positions
  "The positions of each term in the `analyzed` fields, by term, as one
  position or a vector of them in order. Each is offset into the block of
  its field by the field `numbers`, and the terms of a vector share a
  position."
  [numbers analyzed]
  (persistent!
   (reduce (fn [acc field]
             (let [terms (get analyzed field)
                   base  (long (* postings/positions-per-field
                                  (get numbers field)))
                   n     (min postings/positions-per-field (count terms))]
               (loop [i 0 acc acc]
                 (if (< i n)
                   (let [term (nth terms i)
                         at   (+ base i)
                         acc  (if (vector? term)
                                (reduce #(placed %1 %2 at) acc term)
                                (placed acc term at))]
                     (recur (inc i) acc))
                   acc))))
           (transient {})
           (sort-by numbers (keys analyzed)))))

(defn ^:no-doc analyzed-fields
  "The terms by position of each of the `fields` of a document that has
  any, with `term-fn` applied to the terms of a text, if it's given."
  [fields term-fn]
  (into {}
        (keep (fn [[field v]]
                (let [terms (analyze v term-fn)]
                  (when (seq terms)
                    [field terms]))))
        fields))

(defn- posted
  "The transient map `acc` of each term to a transient vector of its
  postings, with the document numbered `no` at the positions `at` of
  `term`."
  [acc no term at]
  (let [postings (or (get acc term) (transient []))]
    (assoc! acc term (-> postings (conj! no) (conj! at)))))

(defn- postings-of
  "The postings of the analyzed documents `entries`, numbered in order,
  whose fields have the `numbers` of an index: a map of each term to a
  vector of the numbers of its documents, each followed by its positions
  there."
  [numbers entries]
  (let [postings (reduce-kv (fn [acc no {:keys [fields]}]
                              (reduce-kv #(posted %1 no %2 %3)
                                         acc
                                         (positions numbers fields)))
                            (transient {})
                            entries)]
    (update-vals (persistent! postings) persistent!)))

;; V8 keeps a substring of 13 characters or more as a slice of the string
;; it's from, so a term would keep the whole text of the field it's from
(defn- detached
  "The term `s` as a string of its own."
  [s]
  #?(:clj  s
     :cljs (if (and (string? s) (<= 13 (count s)))
             (js/JSON.parse (js/JSON.stringify s))
             s)))

(defn- write-term!
  "Write the `postings` of a term with the writer `w`, a vector of the
  numbers of documents, each followed by its positions there, one or a
  vector of them."
  [^Writer w postings]
  (loop [i 0 ^ints ats (make-ints 16)]
    (if (< i (count postings))
      (let [at        (nth postings (inc i))
            n         (if (vector? at) (count at) 1)
            ^ints ats (if (< (alength ats) n) (make-ints (* 2 n)) ats)]
        (if (vector? at)
          (dotimes [j n]
            (aset ats j (int (nth at j))))
          (aset ats 0 (int at)))
        (.add w (nth postings i) ats 0 n)
        (recur (+ i 2) ats))
      (.endTerm w))))

(defn ^:no-doc field-stats
  "The number of documents with each field and their positions in all, as
  pairs by field number, from the `lengths` of `n` documents of `width`
  fields each."
  [^ints lengths n width]
  (let [n     (long n)
        width (long width)]
    (mapv (fn [no]
            (loop [d 0 docs 0 length 0]
              (if (< d n)
                (let [l (aget lengths (+ (* d width) (long no)))]
                  (recur (inc d)
                         (if (pos? l) (inc docs) docs)
                         (+ length l)))
                [docs length])))
          (range width))))

;; A field's length counts its positions, so a term that shares one adds
;; nothing, as discountOverlaps does in Lucene's BM25Similarity
(defn ^:no-doc built
  "A segment of the analyzed documents `entries`, maps of :id, :stored and
  :fields of terms by position, whose fields have the `numbers` of an
  index, and of which those numbered in `unpositioned` have no
  positions."
  [numbers unpositioned entries]
  (let [postings         (postings-of numbers entries)
        terms            (to-array (map detached (sort (keys postings))))
        n                (count entries)
        width            (count numbers)
        ^Out out         (postings/out)
        w                (postings/writer out width
                                          (positioned-fields width
                                                             unpositioned))
        ^ints lengths    (make-ints (* n width))
        ^ints doc-start  (make-ints (inc (alength terms)))
        ^ints term-start (make-ints (inc (alength terms)))]
    (dotimes [t (alength terms)]
      (let [term-postings (get postings (aget terms t))]
        (write-term! w term-postings)
        (aset doc-start (inc t) (int (+ (aget doc-start t)
                                        (quot (count term-postings) 2))))
        (aset term-start (inc t) (int (.size out)))))
    (doseq [[no {:keys [fields]}] (map-indexed vector entries)
            [field terms]         fields]
      (aset lengths (+ (* no width) (get numbers field)) (int (count terms))))
    (segment-of {:ids          (mapv :id entries)
                 :stored       (mapv :stored entries)
                 :terms        terms
                 :doc-start    doc-start
                 :term-start   term-start
                 :postings     (.toBytes out)
                 :lengths      lengths
                 :field-stats  (field-stats lengths n width)
                 :unpositioned unpositioned
                 :deleted      #{}})))

(defn- numbers-from
  "An array of the new number of each document of `segment` in a merged
  segment, counting from `base` and skipping those removed, which get
  -1."
  [^Segment segment base]
  (let [n                 (long (size segment))
        deleted           (.-deleted segment)
        ^ints new-numbers (make-ints n)]
    (loop [d 0 j (long base)]
      (when (< d n)
        (if (contains? deleted d)
          (do (aset new-numbers d -1)
              (recur (inc d) j))
          (do (aset new-numbers d (int j))
              (recur (inc d) (inc j))))))
    new-numbers))

(defn- renumbered
  "For each of the `segments`, an array of the new number of each of its
  documents in one segment of them all in order, or -1 for one that is
  removed."
  [segments]
  (mapv numbers-from segments (reductions + 0 (map live segments))))

(defn- without-removed
  "The values `vs` of the documents of a segment by number, without those
  of the numbers in `deleted`."
  [vs deleted]
  (if (empty? deleted)
    vs
    (into []
          (comp (map-indexed vector)
                (remove #(contains? deleted (first %)))
                (map second))
          vs)))

(defn- copy-postings!
  "Write with the writer `w` the postings in `segment` of the term
  numbered `t` of the documents that `new-numbers` keep, as they number
  them, and give how many."
  ^long [segment ^ints new-numbers ^Writer w ^long t]
  (let [^Cursor c (cursor segment t)]
    (loop [k 0]
      (let [d (.nextDoc c)]
        (cond
          (== d postings/no-more)
          k

          (neg? (aget new-numbers d))
          (recur k)

          :else
          (do (.copyTo c w (aget new-numbers d))
              (recur (inc k))))))))

(defn- least-term
  "The least of the terms of the `segments` at their `cursors`, an array of
  the index of the next term of each, or nil when none is left."
  [segments ^ints cursors]
  (reduce (fn [least i]
            (let [^objects terms (.-terms ^Segment (nth segments i))
                  c              (aget cursors i)]
              (if (< c (alength terms))
                (let [term (aget terms c)]
                  (if (or (nil? least)
                          (neg? (compare term least)))
                    term
                    least))
                least)))
          nil
          (range (count segments))))

(defn- merged-lengths
  "The lengths of the fields of the `n` documents of the `segments` that
  their `new-numbers` keep, `width` of them to a document, as an array."
  [segments new-numbers n width]
  (let [^ints lengths (make-ints (* (long n) (long width)))]
    (doseq [[^Segment segment ^ints nos] (map vector segments new-numbers)
            :let  [w (.-width segment)]
            d     (range (size segment))
            :let  [j (aget nos d)]
            :when (<= 0 j)]
      (copy-ints! (.-lengths segment) (* d w) lengths (* j (long width)) w))
    lengths))

;; A merge of sorted lists: each segment has a cursor at its next term,
;; and the least of those terms comes next. The fields without positions
;; are those of the index, so they're the same in every segment of it,
;; but for the fields that a segment doesn't have
(defn- merged-postings
  "The terms of the `segments` and their postings of the documents that
  their `new-numbers` keep, as the parts of one segment of `width`
  fields, those numbered in `unpositioned` without positions."
  [segments new-numbers width unpositioned]
  (let [^Out out      (postings/out)
        w             (postings/writer out width
                                       (positioned-fields width unpositioned))
        ^ints cursors (make-ints (count segments))
        copy-term!    (fn [term k i]
                        (let [^Segment segment (nth segments i)
                              ^objects terms   (.-terms segment)
                              c                (aget cursors i)]
                          (if (and (< c (alength terms))
                                   (= term (aget terms c)))
                            (do (aset cursors i (inc c))
                                (+ (long k)
                                   (copy-postings! segment (nth new-numbers i)
                                                   w c)))
                            k)))]
    (loop [k 0 terms (transient []) doc-start (transient [0])
           term-start (transient [0])]
      (if-let [term (least-term segments cursors)]
        (let [next-k (long (reduce #(copy-term! term %1 %2)
                                   k
                                   (range (count segments))))]
          (.endTerm ^Writer w)
          ;; a term whose documents are all removed goes
          (if (== next-k k)
            (recur k terms doc-start term-start)
            (recur next-k
                   (conj! terms term)
                   (conj! doc-start next-k)
                   (conj! term-start (.size out)))))
        {:terms      (persistent! terms)
         :doc-start  (persistent! doc-start)
         :term-start (persistent! term-start)
         :postings   (.toBytes out)}))))

(defn ^:no-doc combined
  "One segment of the `segments` in order, without their documents that
  are removed."
  [segments]
  (let [segments     (vec segments)
        new-numbers  (renumbered segments)
        width        (reduce max 0 (map #(.-width ^Segment %) segments))
        unpositioned (into #{} (mapcat #(.-unpositioned ^Segment %)) segments)
        kept         (fn [k]
                       (into []
                             (mapcat #(without-removed (k (parts %))
                                                       (.-deleted ^Segment %)))
                             segments))
        ids          (kept :ids)
        lengths      (merged-lengths segments new-numbers (count ids) width)]
    (segment-of (assoc (merged-postings segments new-numbers width
                                        unpositioned)
                       :ids          ids
                       :stored       (kept :stored)
                       :lengths      lengths
                       :field-stats  (field-stats lengths (count ids) width)
                       :unpositioned unpositioned
                       :deleted      #{}))))

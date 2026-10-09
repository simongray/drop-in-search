(ns dk.simongray.drop-in-search.segment
  "The segments of an index, on both platforms: the postings of some of
  its documents in sorted arrays of ints, as a segment of Lucene's index
  holds them, https://lucene.apache.org/core/9_11_1/core/"
  (:require [dk.simongray.drop-in-search.analysis :as analysis])
  #?(:clj (:import [java.io Writer]
                   [java.util Arrays])))

(def ^{:no-doc true :const true} positions-per-field
  "The block of positions that each field of a document gets.

  Each position of a term in a document is offset by its field's number
  times this. Two positions one apart are then always in the same field.
  A longer field is indexed up to this length."
  1000000)

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
;; sorted arrays of ints, which take far less memory than maps:
;;
;; - ids and stored, those of each document by its number
;; - terms, in order, and doc-start, where the postings of each start in
;;   docs, with the end of the last
;; - docs, the document of each posting, in order for each term, and
;;   pos-start, where its positions start in positions, with the end of
;;   the last
;; - positions, those of each posting in order, each in the block of its
;;   field, as positions-per-field says
;; - lengths, the positions of each field of each document, width of them
;;   to a document, and field-stats, the documents with each field and
;;   their positions in all, by field number
;; - deleted, the numbers of the documents that are removed
;; - name, that of its file in a store, which an equal segment can have
;;   another of
(declare contents segment-data)

(deftype Segment [ids stored terms doc-start docs pos-start positions lengths
                  ^long width field-stats deleted name]
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
     [segment ^Writer writer]
     (print-method (segment-data segment) writer)))

(defn- parts
  "The fields of `segment` as a map, with its :name as it is, a delay
  until something needs it."
  [^Segment segment]
  {:ids         (.-ids segment)
   :stored      (.-stored segment)
   :terms       (.-terms segment)
   :doc-start   (.-doc-start segment)
   :docs        (.-docs segment)
   :pos-start   (.-pos-start segment)
   :positions   (.-positions segment)
   :lengths     (.-lengths segment)
   :field-stats (.-field-stats segment)
   :deleted     (.-deleted segment)
   :name        (.-name segment)})

(defn- contents
  "What `segment` holds as plain data, a map of its fields but its name,
  with its arrays as vectors."
  [segment]
  (reduce #(update %1 %2 as-vector)
          (dissoc (parts segment) :name)
          [:terms :doc-start :docs :pos-start :positions :lengths]))

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

(defn- hex
  "The number `h`, below 2 to the power of 32, as eight hex digits."
  [h]
  #?(:clj  (format "%08x" h)
     :cljs (.padStart (.toString h 16) 8 "0")))

(defn- content-name
  "The name of the file of a segment with the parts in `m`, from a hash of
  all that it holds, so that one that holds anything else has another."
  [{:keys [ids stored terms doc-start docs pos-start positions lengths]}]
  (let [arrays [doc-start docs pos-start positions lengths
                (as-ints (mapv hash terms))
                (as-ints [(hash ids) (hash stored)])]
        lane   #(reduce (fn [h arr] (fnv-ints h arr %)) 2166136261 arrays)]
    (str "segment-" (hex (lane false)) (hex (lane true)) ".ciff")))

(defn ^:no-doc segment-of
  "The segment of `m`, a map of its fields, whose arrays can be vectors.
  Without a :name, its name is a hash of what it holds, worked out once
  it's needed."
  [{:keys [ids stored terms doc-start docs pos-start positions lengths
           field-stats deleted name]
    :as   m}]
  (let [arrays {:ids       (vec ids)
                :stored    (vec stored)
                :terms     (as-objects terms)
                :doc-start (as-ints doc-start)
                :docs      (as-ints docs)
                :pos-start (as-ints pos-start)
                :positions (as-ints positions)
                :lengths   (as-ints lengths)}]
    (->Segment (:ids arrays)
               (:stored arrays)
               (:terms arrays)
               (:doc-start arrays)
               (:docs arrays)
               (:pos-start arrays)
               (:positions arrays)
               (:lengths arrays)
               (count field-stats)
               (vec field-stats)
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

(defn ^:no-doc with-removed
  "The `segment` with the documents numbered `nos` removed."
  [segment nos]
  (segment-of (update (parts segment) :deleted into nos)))

(defn- analyze
  "The terms of the field value `v` by position: a string tokenized, a
  vector taken as terms already, the text of each value of any other
  collection tokenized, or else the text of `v`, e.g. of a number or a
  keyword."
  [v]
  (cond
    (string? v) (analysis/tokens v)
    (vector? v) v
    (coll? v)   (into [] (mapcat #(analysis/tokens (str %))) v)
    :else       (analysis/tokens (str v))))

(defn ^:no-doc numbered
  "The `numbers` of the fields of an index, a map of field to number, with
  a number for each new field of `analyzed`, in order of appearance."
  [numbers analyzed]
  (reduce (fn [numbers field]
            (cond
              (contains? numbers field)
              numbers

              ;; the positions of a field start at its number times
              ;; positions-per-field, in an int
              (< (count numbers) (quot 2147483647 positions-per-field))
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
                   base  (long (* positions-per-field (get numbers field)))
                   n     (min positions-per-field (count terms))]
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
  any."
  [fields]
  (into {}
        (keep (fn [[field v]]
                (let [terms (analyze v)]
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

(defn- fill!
  "Fill the arrays `doc-start`, `docs`, `pos-start` and `positions` of a
  segment with the postings `lists` of its terms in order, each a vector
  of the numbers of documents, each followed by its positions there."
  [^ints doc-start ^ints docs ^ints pos-start ^ints positions lists]
  (loop [t 0 i 0 k 0 p 0]
    (cond
      (= t (count lists))
      (aset pos-start k (int p))

      (= i (count (nth lists t)))
      (do (aset doc-start (inc t) (int k))
          (recur (inc t) 0 k p))

      :else
      (let [postings (nth lists t)
            at       (nth postings (inc i))
            ats      (if (vector? at) at [at])]
        (aset docs k (int (nth postings i)))
        (aset pos-start k (int p))
        (dotimes [j (count ats)]
          (aset positions (+ p j) (int (nth ats j))))
        (recur t (+ i 2) (inc k) (+ p (count ats)))))))

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
  index."
  [numbers entries]
  (let [postings      (postings-of numbers entries)
        terms         (to-array (sort (keys postings)))
        lists         (mapv #(get postings %) terms)
        n             (count entries)
        width         (count numbers)
        positions-of  (fn [postings]
                        (transduce (comp (drop 1)
                                         (take-nth 2)
                                         (map #(if (vector? %) (count %) 1)))
                                   +
                                   postings))
        n-postings    (reduce + 0 (map #(quot (count %) 2) lists))
        ^ints lengths (make-ints (* n width))
        doc-start     (make-ints (inc (count lists)))
        docs          (make-ints n-postings)
        pos-start     (make-ints (inc n-postings))
        positions     (make-ints (reduce + 0 (map positions-of lists)))]
    (fill! doc-start docs pos-start positions lists)
    (doseq [[no {:keys [fields]}] (map-indexed vector entries)
            [field terms]         fields]
      (aset lengths (+ (* no width) (get numbers field)) (int (count terms))))
    (segment-of {:ids         (mapv :id entries)
                 :stored      (mapv :stored entries)
                 :terms       terms
                 :doc-start   doc-start
                 :docs        docs
                 :pos-start   pos-start
                 :positions   positions
                 :lengths     lengths
                 :field-stats (field-stats lengths n width)
                 :deleted     #{}})))

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
  "Copy the postings of `segment` from `from` to `to` of the documents that
  `new-numbers` keep into the arrays `docs`, `pos-start` and `positions`
  of a merged segment, from the posting `k` and the position `p` on, and
  give where the copies end, as a pair."
  [^Segment segment ^ints new-numbers from to
   ^ints docs ^ints pos-start ^ints positions k p]
  (let [^ints old-docs      (.-docs segment)
        ^ints old-pos-start (.-pos-start segment)
        old-positions       (.-positions segment)
        to                  (long to)]
    (loop [i (long from) k (long k) p (long p)]
      (if (< i to)
        (let [d (aget new-numbers (aget old-docs i))]
          (if (neg? d)
            (recur (inc i) k p)
            (let [start (aget old-pos-start i)
                  n     (- (aget old-pos-start (inc i)) start)]
              (aset docs k d)
              (aset pos-start k (int p))
              (copy-ints! old-positions start positions p n)
              (recur (inc i) (inc k) (+ p n)))))
        [k p]))))

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
;; and the least of those terms comes next
(defn- merged-postings
  "The terms of the `segments` and their postings of the documents that
  their `new-numbers` keep, as the parts of one segment."
  [segments new-numbers]
  (let [total           (fn [f]
                          (reduce + 0 (map #(alength ^ints (f %)) segments)))
        ^ints docs      (make-ints (total #(.-docs ^Segment %)))
        ^ints pos-start (make-ints (inc (alength docs)))
        positions       (make-ints (total #(.-positions ^Segment %)))
        ^ints cursors   (make-ints (count segments))
        copy-term!      (fn [term [k p] i]
                          (let [^Segment segment (nth segments i)
                                ^objects terms   (.-terms segment)
                                ^ints starts     (.-doc-start segment)
                                c                (aget cursors i)]
                            (if (and (< c (alength terms))
                                     (= term (aget terms c)))
                              (do (aset cursors i (inc c))
                                  (copy-postings! segment (nth new-numbers i)
                                                  (aget starts c)
                                                  (aget starts (inc c))
                                                  docs pos-start positions
                                                  k p))
                              [k p])))]
    (loop [k 0 p 0 terms (transient []) doc-start (transient [0])]
      (if-let [term (least-term segments cursors)]
        (let [[next-k next-p] (reduce #(copy-term! term %1 %2)
                                      [k p]
                                      (range (count segments)))]
          ;; a term whose documents are all removed goes
          (if (= next-k k)
            (recur k p terms doc-start)
            (recur (long next-k)
                   (long next-p)
                   (conj! terms term)
                   (conj! doc-start next-k))))
        (do (aset pos-start k (int p))
            {:terms     (persistent! terms)
             :doc-start (persistent! doc-start)
             :docs      (slice docs 0 k)
             :pos-start (slice pos-start 0 (inc k))
             :positions (slice positions 0 p)})))))

(defn ^:no-doc combined
  "One segment of the `segments` in order, without their documents that
  are removed."
  [segments]
  (let [segments    (vec segments)
        new-numbers (renumbered segments)
        width       (reduce max 0 (map #(.-width ^Segment %) segments))
        kept        (fn [k]
                      (into []
                            (mapcat #(without-removed (k (parts %))
                                                      (.-deleted ^Segment %)))
                            segments))
        ids         (kept :ids)
        lengths     (merged-lengths segments new-numbers (count ids) width)]
    (segment-of (assoc (merged-postings segments new-numbers)
                       :ids         ids
                       :stored      (kept :stored)
                       :lengths     lengths
                       :field-stats (field-stats lengths (count ids) width)
                       :deleted     #{}))))

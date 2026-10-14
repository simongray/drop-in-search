(ns dk.simongray.drop-in-search.ciff
  "An index kept as files in a store: a manifest in EDN, and a file for
  each segment in the Common Index File Format, CIFF, which other search
  engines can read.

  CIFF is that of https://github.com/osirrc/ciff, its README and its
  CommonIndexFileFormat.proto of 2020-03. What CIFF lacks has fields of
  its own, which a reader of CIFF skips, as the encoding of protobuf has
  it, https://protobuf.dev/programming-guides/encoding/"
  (:require #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [dk.simongray.drop-in-search :as search]
            [dk.simongray.drop-in-search.indexing :as indexing]
            [dk.simongray.drop-in-search.postings :as postings
             #?@(:cljs [:refer [Cursor Out Writer]])]
            [dk.simongray.drop-in-search.queries :as search.queries]
            [dk.simongray.drop-in-search.segment :as segment
             #?@(:cljs [:refer [Segment]])])
  #?(:clj (:import [dk.simongray.drop_in_search.postings Cursor Out Writer]
                   [dk.simongray.drop_in_search.segment Segment]
                   [java.nio.charset StandardCharsets])))

(def ^:no-doc version
  "The newest version of the format of the files of an index that this
  reads, which changes only when the meaning of a part changes: 2 for a
  segment with fields without positions, and 1 for any other."
  2)

(defn- segment-version
  "The version of the format of the file of `segment`."
  [^Segment segment]
  (if (seq (.-unpositioned segment)) 2 1))

(def ^:no-doc manifest-name
  "The name of the file that lists the segments of an index."
  "index.edn")

(defn- put-byte!
  "Put the byte `x` into the bytes `bs` at `at`, and give where the next
  one goes."
  ^long [bs ^long at ^long x]
  #?(:clj  (aset ^bytes bs at (unchecked-byte x))
     :cljs (aset bs at x))
  (inc at))

(defn- put-bytes!
  "Put the bytes `src` into the bytes `bs` at `at`, and give where the next
  one goes."
  ^long [src bs ^long at]
  #?(:clj  (System/arraycopy src 0 bs (int at) (alength ^bytes src))
     :cljs (.set bs src at))
  (+ at (postings/byte-count src)))

(defn- utf8
  "The string `s` as UTF-8 bytes."
  [s]
  #?(:clj  (.getBytes ^String s StandardCharsets/UTF_8)
     :cljs (.encode (js/TextEncoder.) s)))

(defn- utf8-text
  "The text of the UTF-8 bytes `bs` from `from` to `to`."
  [bs from to]
  #?(:clj  (String. ^bytes bs
                    (int from)
                    (int (- to from))
                    StandardCharsets/UTF_8)
     :cljs (.decode (js/TextDecoder.) (.subarray bs from to))))

(defn- put-double!
  "Put the double `x` into the bytes `bs` at `at`, eight bytes in little
  endian order, and give where the next one goes."
  ^long [bs ^long at ^double x]
  #?(:clj  (let [bits (Double/doubleToRawLongBits x)]
             (dotimes [i 8]
               (put-byte! bs (+ at i) (bit-and (bit-shift-right bits (* 8 i))
                                               0xFF))))
     :cljs (.setFloat64 (js/DataView. (.-buffer bs) (.-byteOffset bs))
                        at
                        x
                        true))
  (+ at 8))

;; protobuf's Base 128 varints: seven bits to a byte, the lowest first, with
;; the high bit set on all but the last
(defn- varint-size
  ^long [^long x]
  (loop [x x n 1]
    (if (< x 128)
      n
      (recur (quot x 128) (inc n)))))

(defn- put-varint!
  "Put the number `x` into the bytes `bs` at `at` as a varint, and give
  where the next one goes."
  ^long [bs ^long at ^long x]
  (if (< x 128)
    (put-byte! bs at x)
    (recur bs (put-byte! bs at (+ 128 (rem x 128))) (quot x 128))))

;; The tag of a record of protobuf is its field number * 8 + its wire type
(defn- tag
  ^long [^long field wire]
  (+ (* 8 field) (case wire :varint 0 :i64 1 :len 2)))

(defn- field-size
  "The bytes of the field `fld`, a vector of its number, its kind
  (:varint, :double, :bytes or :packed, a vector of varints) and its
  value."
  ^long [fld]
  (let [[field kind v] fld
        body           (case kind
                         :varint (varint-size v)
                         :double 8
                         :bytes  (postings/byte-count v)
                         :packed (reduce + 0 (map varint-size v)))
        wire           (case kind :varint :varint :double :i64 :len)]
    (+ (varint-size (tag field wire))
       (if (#{:bytes :packed} kind) (varint-size body) 0)
       body)))

(defn- put-field!
  "Put the field `fld`, a vector of its number, kind and value, into the
  bytes `bs` at `at`, and give where the next one goes."
  ^long [bs ^long at fld]
  (let [[field kind v] fld]
    (case kind
      :varint (put-varint! bs (put-varint! bs at (tag field :varint)) v)
      :double (put-double! bs (put-varint! bs at (tag field :i64)) v)
      :bytes  (let [at (put-varint! bs at (tag field :len))]
                (put-bytes! v bs (put-varint! bs at (postings/byte-count v))))
      :packed (let [at (put-varint! bs at (tag field :len))
                    at (put-varint! bs at (reduce + 0 (map varint-size v)))]
                (reduce #(put-varint! bs %1 %2) at v)))))

(defn- delimited-size
  "The bytes of a message of the `fields`, with its size before it, as
  writeDelimitedTo writes it."
  ^long [fields]
  (let [body (reduce + 0 (map field-size fields))]
    (+ (varint-size body) body)))

(defn- put-message!
  "Put a message of the `fields` into the bytes `bs` at `at`, with its size
  before it, and give where the next one goes."
  ^long [bs ^long at fields]
  (reduce #(put-field! bs %1 %2)
          (put-varint! bs at (reduce + 0 (map field-size fields)))
          fields))

;; The CIFF messages, with these fields of their own:
;;
;; - a PostingsList has the positions of its postings as field 15,
;;   packed, each posting's first whole and the rest as the gap after the
;;   one before, and the runs of its postings in the fields without
;;   positions as field 14, packed: for each posting, the number of them,
;;   and the field and the count of each
;; - a DocRecord has the length of each field as field 14, packed, and
;;   what is stored as EDN as field 15
;; - the Header has the numbers of the fields without positions as field
;;   14, packed, and the version of this format as field 15
(defn- header-fields
  [^Segment segment]
  (let [n-terms (alength ^objects (.-terms segment))
        n-docs  (segment/size segment)
        total   (areduce ^ints (.-lengths segment) i n 0
                         (+ n (aget ^ints (.-lengths segment) i)))]
    (cond-> [[1 :varint 1]
             [2 :varint n-terms]
             [3 :varint n-docs]
             [4 :varint n-terms]
             [5 :varint n-docs]
             [6 :varint total]
             [7 :double (if (pos? n-docs) (/ (double total) n-docs) 0.0)]
             [8 :bytes (utf8 (str "A segment of an index of"
                                  " dk.simongray.drop-in-search"))]]
      (seq (.-unpositioned segment))
      (conj [14 :packed (vec (sort (.-unpositioned segment)))])

      :always
      (conj [15 :varint (segment-version segment)]))))

(defn- doc-fields
  "The fields of the DocRecord of the document `d` of `segment`."
  [^Segment segment d]
  (let [width   (.-width segment)
        lengths (mapv #(aget ^ints (.-lengths segment) (+ (* d width) %))
                      (range width))
        stored  (nth (.-stored segment) d)]
    (cond-> [[1 :varint d]
             [2 :bytes (utf8 (pr-str (nth (.-ids segment) d)))]
             [3 :varint (reduce + 0 lengths)]
             [14 :packed lengths]]
      (some? stored) (conj [15 :bytes (utf8 (pr-str stored))]))))

;; The postings are read twice, first for the count of their positions
(defn- conj-unplaced!
  "The transient `acc` with the runs of the posting at the cursor `c` in
  the fields without positions, those with a 0 in `positioned`: their
  number, and the field and the count of each."
  [acc ^Cursor c ^ints positioned]
  (let [runs (filterv #(zero? (aget positioned (.field c %)))
                      (range (.runs c)))]
    (reduce #(-> %1 (conj! (.field c %2)) (conj! (.freq c %2)))
            (conj! acc (count runs))
            runs)))

;; The postings are read twice, first for the count of their positions
(defn- term-postings
  "The postings in `segment` of the term numbered `t`, as a map of arrays
  of their :docs, their :tfs, the :placed of each, its positions, and
  their :positions one after the other, and of their :unplaced runs, those
  in the fields without positions, if it has any."
  [^Segment segment t]
  (let [n               (segment/doc-frequency segment t)
        unplaced?       (seq (.-unpositioned segment))
        ^ints docs      (segment/make-ints n)
        ^ints tfs       (segment/make-ints n)
        ^ints placed    (segment/make-ints n)
        ^ints flags     (.-positioned segment)
        ^Cursor c       (segment/cursor segment t)
        unplaced        (loop [k 0 acc (transient [])]
                          (if (< k n)
                            (do (aset docs k (int (.nextDoc c)))
                                (aset tfs k (int (.tf c)))
                                (recur (inc k) (cond-> acc
                                                 unplaced?
                                                 (conj-unplaced! c flags))))
                            (persistent! acc)))
        ;; as many as the occurrences, of which some may have none
        ^ints positions (segment/make-ints (areduce tfs k m 0
                                                    (+ m (aget tfs k))))
        ^Cursor c       (segment/cursor segment t)
        at              (loop [k 0 at 0]
                          (if (< k n)
                            (do (.nextDoc c)
                                (let [end (.readPositions c positions at)]
                                  (aset placed k (int (- end at)))
                                  (recur (inc k) end)))
                            at))]
    (cond-> {:docs      docs
             :tfs       tfs
             :placed    placed
             :positions (if (== at (alength positions))
                          positions
                          (segment/slice positions 0 at))}
      unplaced? (assoc :unplaced unplaced))))

(defn- reduce-positions
  "Reduce the `positions` of postings with the counts `placed`, both
  arrays, with `f` from `init`, as they're packed: each posting's first
  whole and the rest as the gap after the one before."
  [f init ^ints placed ^ints positions]
  (loop [k 0 i 0 start 0 acc init]
    (cond
      (== i (alength positions))
      acc

      (and (== i start) (zero? (aget placed k)))
      (recur (inc k) i start acc)

      (== i start)
      (recur (inc k) (inc i) (+ i (aget placed k)) (f acc (aget positions i)))

      :else
      (recur k (inc i) start (f acc (- (aget positions i)
                                       (aget positions (dec i))))))))

(defn- list-sizes
  "The bytes of the body of the PostingsList of the term whose UTF-8 is
  `term-bytes`, with the `postings-list` of term-postings, and of its
  packed positions, as a pair."
  [{:keys [^ints docs ^ints tfs ^ints placed ^ints positions unplaced]
    :as   postings-list}
   term-bytes]
  (let [packed   (reduce-positions #(+ (long %1) (varint-size %2))
                                   0 placed positions)
        postings (loop [k 0 n 0]
                   (if (< k (alength docs))
                     (let [gap  (- (aget docs k)
                                   (if (zero? k) 0 (aget docs (dec k))))
                           body (+ 2 (varint-size gap)
                                   (varint-size (aget tfs k)))]
                       (recur (inc k) (+ n 1 (varint-size body) body)))
                     n))
        n        (postings/byte-count term-bytes)]
    [(+ 1 (varint-size n) n
        1 (varint-size (alength docs))
        1 (varint-size (areduce tfs k m 0 (+ m (aget tfs k))))
        postings
        (if unplaced (field-size [14 :packed unplaced]) 0)
        1 (varint-size packed) packed)
     packed]))

(defn- put-list!
  "Put the PostingsList of the term whose UTF-8 is `term-bytes` into the
  bytes `bs` at `at`, with the `postings-list` of term-postings and the
  `sizes` of its body and its packed positions, and give where the next
  one goes."
  [bs at {:keys [^ints docs ^ints tfs ^ints placed ^ints positions unplaced]
          :as   postings-list}
   term-bytes [body packed :as sizes]]
  (let [put         (fn [at field kind v] (put-field! bs at [field kind v]))
        at          (-> (put-varint! bs at body)
                        (put 1 :bytes term-bytes)
                        (put 2 :varint (alength docs))
                        (put 3 :varint (areduce tfs k m 0
                                                (+ m (aget tfs k)))))
        posting-tag (tag 4 :len)
        docid-tag   (tag 1 :varint)
        tf-tag      (tag 2 :varint)
        at          (loop [k 0 at (long at)]
                      (if (< k (alength docs))
                        (let [gap  (- (aget docs k)
                                      (if (zero? k) 0 (aget docs (dec k))))
                              tf   (aget tfs k)
                              body (+ 2 (varint-size gap) (varint-size tf))
                              at   (put-varint! bs at posting-tag)
                              at   (put-varint! bs at body)
                              at   (put-varint! bs at docid-tag)
                              at   (put-varint! bs at gap)
                              at   (put-varint! bs at tf-tag)]
                          (recur (inc k) (put-varint! bs at tf)))
                        at))
        at          (cond-> at
                      unplaced (put 14 :packed unplaced))
        at          (put-varint! bs at (tag 15 :len))]
    (reduce-positions #(put-varint! bs %1 %2)
                      (put-varint! bs at packed)
                      placed
                      positions)))

;; The postings of each term are read twice, once to size the file and
;; once to write it, so that only one term's are read at a time
(defn- segment-body
  "The CIFF file of `segment`, as bytes."
  [^Segment segment]
  (binding [*print-length* nil
            *print-level*  nil]
    (let [terms   (mapv utf8 (.-terms segment))
          sizes   (mapv #(list-sizes (term-postings segment %) (terms %))
                        (range (count terms)))
          records (mapv #(doc-fields segment %)
                        (range (segment/size segment)))
          header  (header-fields segment)
          total   (+ (delimited-size header)
                     (reduce + 0 (map (fn [[body]]
                                        (+ (varint-size body) body))
                                      sizes))
                     (reduce + 0 (map delimited-size records)))
          bs      (postings/make-bytes total)
          at      (put-message! bs 0 header)
          at      (reduce #(put-list! bs %1 (term-postings segment %2)
                                      (terms %2) (sizes %2))
                          at
                          (range (count terms)))]
      (reduce #(put-message! bs %1 %2) at records)
      bs)))

(defn- manifest
  "The manifest of `index` as EDN: its segments by the names of their
  files, with the numbers of their removed documents, and its fields and
  defaults."
  [index]
  (binding [*print-length* nil
            *print-level*  nil]
    (pr-str (cond-> {:version  (if (seq (:without-positions (:defaults index)))
                                 2
                                 1)
                     :fields   (:fields index)
                     :segments (mapv (fn [^Segment segment]
                                       {:name    (segment/segment-name segment)
                                        :deleted (.-deleted segment)})
                                     (:segments index))}
              (:defaults index) (assoc :defaults (:defaults index))))))

(defn- segment-file?
  [name]
  (boolean (re-matches #"segment-[0-9a-f]{16}\.ciff" (str name))))

(defn plan
  "The files to put into a store that holds the files of the `names`, and
  the names to delete from it, so that it holds `index`.

  It's a map of :put, files as maps of :name and :body to put in this
  order, and :delete, the names of the segments that `index` no longer
  has. Without `names`, it's every file. The store needs a place of its
  own, e.g. a folder, since the segments of any other index there are
  deleted.

  The :body of the manifest is EDN, and that of a segment is CIFF, as a
  byte array on the JVM and a Uint8Array in JavaScript. A segment is
  named after what it holds, so it's put once. Put the files before you
  delete, so that a store that a crash stops in between still holds an
  index:

      (let [{:keys [put delete]} (plan idx (map :name (list-files)))]
        (doseq [{:keys [name body]} put]
          (put-file! name body))
        (run! delete-file! delete))"
  ([index]
   (plan index #{}))
  ([index names]
   (let [index    (search/restore index)
         held     (set names)
         by-name  (into {} (map (juxt segment/segment-name identity))
                        (rseq (:segments index)))
         put      (for [[name segment] by-name
                        :when (not (contains? held name))]
                    {:name name :body (segment-body segment)})]
     {:put    (conj (vec put) {:name manifest-name :body (manifest index)})
      :delete (->> held
                   (filter #(and (segment-file? %)
                                 (not (contains? by-name %))))
                   sort
                   vec)})))

(defn- malformed
  "An error of a file that doesn't read, with the `message` and the
  `data`."
  [message data]
  (ex-info message (assoc data :type ::malformed)))

(defn- read-varint!
  "The varint at the place in `cursor` of the bytes `bs`, which ends at
  `end`, with the cursor moved past it."
  ^long [bs ^ints cursor ^long end]
  (loop [at (long (aget cursor 0)) scale 1 x 0]
    (cond
      (<= end at)
      (throw (malformed "A number runs past the end" {}))

      ;; seven bytes are more than any number of an index needs
      (< 0x100000000000000 scale)
      (throw (malformed "A number is too long" {}))

      :else
      (let [b (postings/byte-at bs at)
            x (+ x (* scale (rem b 128)))]
        (if (< b 128)
          (do (aset cursor 0 (int (inc at)))
              x)
          (recur (inc at) (* scale 128) x))))))

(defn- read-len!
  "The end of the field of the wire type LEN at the cursor of the bytes
  `bs`, within `end`, with the cursor moved to its body."
  ^long [bs ^ints cursor ^long end]
  (let [n  (read-varint! bs cursor end)
        to (+ (aget cursor 0) n)]
    (when (< end to)
      (throw (malformed "A field runs past the end" {})))
    to))

(defn- skip!
  "Move the cursor of the bytes `bs` past a field of the `wire` type that
  ends within `end`, as a reader does with a field it doesn't know."
  [bs ^ints cursor ^long end ^long wire]
  (case wire
    0 (read-varint! bs cursor end)
    1 (aset cursor 0 (int (+ (aget cursor 0) 8)))
    2 (aset cursor 0 (int (read-len! bs cursor end)))
    5 (aset cursor 0 (int (+ (aget cursor 0) 4)))
    (throw (malformed "A field has a wire type that isn't read"
                      {:wire wire})))
  (when (< end (aget cursor 0))
    (throw (malformed "A field runs past the end" {}))))

(defn- reduce-fields
  "Reduce the fields of the message from the cursor of the bytes `bs` to
  `end` with `f` from `init`. The `f` takes the accumulator, the field
  number and the wire type, and reads the field, or gives nil to skip it."
  [f init bs ^ints cursor end]
  (loop [acc init]
    (if (< (aget cursor 0) (long end))
      (let [t     (read-varint! bs cursor end)
            field (quot t 8)
            wire  (rem t 8)
            read  (f acc field wire)]
        (if (some? read)
          (recur read)
          (do (skip! bs cursor end wire)
              (recur acc))))
      acc)))

(defn- read-text!
  "The text of the field of the wire type LEN at the cursor of the bytes
  `bs`, within `end`."
  [bs ^ints cursor ^long end]
  (let [to   (read-len! bs cursor end)
        text (utf8-text bs (aget cursor 0) to)]
    (aset cursor 0 (int to))
    text))

(defn- read-int!
  "The varint at the cursor of the bytes `bs`, within `end`, which must fit
  an int."
  ^long [bs ^ints cursor ^long end]
  (let [x (read-varint! bs cursor end)]
    (when (< 2147483647 x)
      (throw (malformed "A number is too large" {})))
    x))

(defn- read-packed!
  "The varints of the packed field at the cursor of the bytes `bs`, within
  `end`, as an array."
  [bs ^ints cursor ^long end]
  (let [to         (read-len! bs cursor end)
        ;; each varint takes a byte at least
        ^ints xs   (segment/make-ints (- to (aget cursor 0)))]
    (loop [n 0]
      (if (< (aget cursor 0) to)
        (do (aset xs n (int (read-int! bs cursor to)))
            (recur (inc n)))
        (segment/slice xs 0 n)))))

(defn- read-edn
  "The value of the EDN `text` of the field `field`."
  [text field]
  (try
    (edn/read-string text)
    ;; Throwable on the JVM for the StackOverflowError that deeply nested
    ;; EDN brings about
    (catch #?(:clj Throwable :cljs :default) e
      (throw (ex-info "A field is not EDN"
                      {:type ::malformed :field field}
                      e)))))

;; A postings list holds most of a segment, so its fields are read in loops
;; of their own, by their tags
(defn- read-posting!
  "Read the Posting at the cursor of the bytes `bs`, within `end`, into
  `out`: the gap before its document, and its frequency."
  [bs ^ints cursor ^long end ^ints out]
  (let [to        (read-len! bs cursor end)
        docid-tag (tag 1 :varint)
        tf-tag    (tag 2 :varint)]
    (aset out 0 0)
    (aset out 1 0)
    (loop []
      (when (< (aget cursor 0) to)
        (let [t (read-varint! bs cursor to)]
          (cond
            (= t docid-tag) (aset out 0 (int (read-int! bs cursor to)))
            (= t tf-tag)    (aset out 1 (int (read-int! bs cursor to)))
            :else           (skip! bs cursor to (rem t 8)))
          (recur))))))

(defn- absolute-positions
  "The `packed` positions of the postings with the `placed` counts of
  them, both arrays, each posting's first whole and the rest as the gap
  after the one before, as the positions themselves, or nil when they
  don't add up or don't go up."
  [^ints packed ^ints placed]
  (let [n               (alength packed)
        ^ints positions (segment/make-ints n)]
    (loop [k 0 i 0 start 0]
      (cond
        (and (= i start) (< k (alength placed)) (zero? (aget placed k)))
        (recur (inc k) i start)

        (= i n)
        (when (and (= k (alength placed)) (= i start))
          positions)

        (= i start)
        (when (< k (alength placed))
          (aset positions i (aget packed i))
          (recur (inc k) (inc i) (+ i (aget placed k))))

        :else
        (let [at (+ (aget positions (dec i)) (aget packed i))]
          (when (and (pos? (aget packed i)) (<= at 2147483647))
            (aset positions i (int at))
            (recur k (inc i) start)))))))

(defn- unplaced-runs
  "The runs of `n` postings in the fields numbered in `unpositioned`, the
  fields without positions, which `packed` holds as field 14 of a
  PostingsList, as a vector of pairs of a field and a count for each
  posting, or nil when they don't add up."
  [^ints packed n unpositioned]
  (let [size (alength packed)
        pair #(vector (aget packed (+ %1 1 (* 2 %2)))
                      (aget packed (+ %1 2 (* 2 %2))))]
    (loop [k 0 j 0 acc (transient [])]
      (if (== k (long n))
        (when (== j size)
          (persistent! acc))
        (let [m   (if (< j size) (aget packed j) -1)
              end (+ j 1 (* 2 m))]
          (when (and (<= 0 m) (<= end size))
            (let [runs (mapv #(pair j %) (range m))]
              (when (and (every? #(pos? (second %)) runs)
                         (every? #(contains? unpositioned (first %)) runs)
                         (apply < -1 (map first runs)))
                (recur (inc k) end (conj! acc runs))))))))))

(defn- placed-counts
  "The counts of the positions of postings with the frequencies `tfs`,
  those of their runs in the fields with positions, the rest being those
  of their `runs`, if any, in the fields without, or nil when they don't
  add up."
  [^ints tfs runs]
  (let [n            (alength tfs)
        ^ints placed (segment/make-ints n)]
    (loop [k 0]
      (if (< k n)
        (let [placed-k (- (aget tfs k)
                          (reduce + 0 (map second (some-> runs (nth k)))))]
          (when (and (pos? (aget tfs k)) (<= 0 placed-k))
            (aset placed k (int placed-k))
            (recur (inc k))))
        placed))))

;; A writer counts the occurrences of a field without positions, and drops
;; their positions, so those of a file's runs in such fields are made up
(defn- made-up-positions
  "The `positions` of postings with the `placed` counts of them, with
  positions made up for their `runs` in the fields without positions, as
  many as their counts from the start of each field, in order with the
  others, `total` of them."
  [^ints positions ^ints placed runs total]
  (let [^ints out (segment/make-ints total)
        n         (alength placed)
        ppf       postings/positions-per-field
        end-of    #(if (< % n) (+ %2 (aget placed %)) %2)]
    (loop [k 0 i 0 end (end-of 0 0) rs (first runs) o 0]
      (cond
        (== k n)
        out

        (and (seq rs)
             (or (== i end)
                 (< (first (first rs)) (quot (aget positions i) ppf))))
        (let [[f c] (first rs)]
          (dotimes [q c]
            (aset out (+ o q) (int (+ (* f ppf) q))))
          (recur k i end (rest rs) (+ o c)))

        (< i end)
        (do (aset out o (aget positions i))
            (recur k (inc i) end rs (inc o)))

        :else
        (recur (inc k) i (end-of (inc k) i) (nth runs (inc k) nil) o)))))

(defn- read-list!
  "The PostingsList of the message at the cursor of the bytes `bs`, which
  ends at `end`, of a segment whose fields without positions are numbered
  in `unpositioned`, as a map of its :term, and of its :docs, :tfs and
  :positions as arrays, or throws ::malformed."
  [unpositioned bs ^ints cursor ^long end]
  (let [;; a Posting takes four bytes at least
        cap           (quot (- end (aget cursor 0)) 4)
        ^ints docs    (segment/make-ints cap)
        ^ints tfs     (segment/make-ints cap)
        ^ints posting (segment/make-ints 2)
        term-tag      (tag 1 :len)
        posting-tag   (tag 4 :len)
        unplaced-tag  (tag 14 :len)
        positions-tag (tag 15 :len)
        out-of-order  #(malformed "A postings list is out of order" {:term %})]
    (loop [term nil n 0 doc 0 packed nil unplaced nil]
      (if (< (aget cursor 0) end)
        (let [t (read-varint! bs cursor end)]
          (cond
            (= t term-tag)
            (recur (read-text! bs cursor end) n doc packed unplaced)

            (= t posting-tag)
            (let [_   (read-posting! bs cursor end posting)
                  gap (aget posting 0)
                  doc (+ doc gap)]
              (when (or (<= cap n)
                        (and (pos? n) (zero? gap))
                        (< 2147483647 doc))
                (throw (out-of-order term)))
              (aset docs n (int doc))
              (aset tfs n (aget posting 1))
              (recur term (inc n) doc packed unplaced))

            (= t unplaced-tag)
            (recur term n doc packed (read-packed! bs cursor end))

            (= t positions-tag)
            (recur term n doc (read-packed! bs cursor end) unplaced)

            :else
            (do (skip! bs cursor end (rem t 8))
                (recur term n doc packed unplaced))))
        (let [tfs       (segment/slice tfs 0 n)
              runs      (some-> unplaced (unplaced-runs n unpositioned))
              placed    (when (= (some? unplaced) (some? runs))
                          (placed-counts tfs runs))
              positions (when (and packed placed)
                          (absolute-positions packed placed))]
          (when-not (and term positions)
            (throw (malformed "A postings list has no term or no positions"
                              {:term term})))
          {:term      term
           :docs      (segment/slice docs 0 n)
           :tfs       tfs
           :positions (if runs
                        (made-up-positions positions placed runs
                                           (areduce tfs k m 0
                                                    (+ m (aget tfs k))))
                        positions)})))))

(defn- read-record!
  "The DocRecord of the message at the cursor of the bytes `bs`, which ends
  at `end`, as a map of its :docid, :id, :lengths and :stored."
  [bs ^ints cursor ^long end]
  (let [edn-of #(read-edn (read-text! bs cursor end) %)]
    (reduce-fields (fn [acc field wire]
                     (case [field wire]
                       [1 0]  (assoc acc :docid (read-varint! bs cursor end))
                       [2 2]  (assoc acc :id (edn-of :collection_docid))
                       [14 2] (assoc acc :lengths (read-packed! bs cursor end))
                       [15 2] (assoc acc :stored (edn-of :stored))
                       nil))
                   {:docid 0}
                   bs
                   cursor
                   end)))

(defn- read-header!
  "The Header of the message at the cursor of the bytes `bs`, which ends at
  `end`, as a map of its :lists, :docs, :unpositioned and :version."
  [bs ^ints cursor ^long end]
  (reduce-fields (fn [acc field wire]
                   (case [field wire]
                     [2 0]  (assoc acc :lists (read-varint! bs cursor end))
                     [3 0]  (assoc acc :docs (read-varint! bs cursor end))
                     [14 2] (let [^ints nos (read-packed! bs cursor end)]
                              (assoc acc :unpositioned
                                     (into #{}
                                           (map #(long (aget nos %)))
                                           (range (alength nos)))))
                     [15 0] (assoc acc :version (read-varint! bs cursor end))
                     nil))
                 {:lists 0 :docs 0}
                 bs
                 cursor
                 end))

(defn- read-messages!
  "The messages of the bytes `bs` from the cursor on, each with its size
  before it, read by `read-message!`, a function of the bytes, the cursor
  and where the message ends, `n` of them."
  [read-message! bs ^ints cursor n]
  (let [end (postings/byte-count bs)]
    (loop [acc (transient []) i 0]
      (if (< i (long n))
        (let [to (read-len! bs cursor end)
              m  (read-message! bs cursor to)]
          (aset cursor 0 (int to))
          (recur (conj! acc m) (inc i)))
        (persistent! acc)))))

(defn- starts
  "Where each run of the `counts`, an int array, starts, and where the last
  ends, as an array."
  [^ints counts]
  (let [n         (alength counts)
        ^ints out (segment/make-ints (inc n))]
    (loop [i 0 at 0]
      (when (< i n)
        (let [at (+ at (aget counts i))]
          (aset out (inc i) (int at))
          (recur (inc i) at))))
    out))

(defn- encoded
  "The postings `lists` of the terms of a segment of `width` fields, maps
  of their :docs, :tfs and :positions, as the :term-start and :postings
  of the segment, those numbered in `unpositioned` without positions."
  [lists width unpositioned]
  (let [^Out out   (postings/out)
        ^Writer w  (postings/writer out width
                                    (segment/positioned-fields width
                                                               unpositioned))
        add-list   (fn [term-start {:keys [^ints docs ^ints tfs positions]}]
                     (loop [k 0 at 0]
                       (when (< k (alength docs))
                         (let [to (+ at (aget tfs k))]
                           (.add w (aget docs k) positions at to)
                           (recur (inc k) to))))
                     (.endTerm w)
                     (conj! term-start (.size out)))
        term-start (reduce add-list (transient [0]) lists)]
    {:term-start (persistent! term-start)
     :postings   (.toBytes out)}))

(defn- segment-from
  "The segment of the postings `lists` and the doc `records` that the CIFF
  file named `name` holds, without positions in the fields numbered in
  `unpositioned`, with the documents numbered `deleted` removed, or throws
  ::malformed."
  [name lists records unpositioned deleted]
  (let [n             (count records)
        widths        (map #(some-> ^ints (:lengths %) alength) records)
        width         (or (first widths) 0)
        terms         (mapv :term lists)
        last-docs     (keep #(let [^ints docs (:docs %)]
                               (when (pos? (alength docs))
                                 (aget docs (dec (alength docs)))))
                            lists)
        highest       (reduce max -1 (map (fn [{^ints ps :positions}]
                                            (areduce ps i m -1
                                                     (max m (aget ps i))))
                                          lists))
        ^ints lengths (segment/make-ints (* n width))
        bad           #(throw (malformed % {:name name}))]
    (cond
      (not= (range n) (map :docid records))
      (bad "The documents of a segment aren't numbered in order")

      (or (and (pos? n) (zero? width))
          (not (every? #(= width %) widths)))
      (bad "The documents of a segment have no lengths of the same fields")

      (not (every? neg? (map compare terms (rest terms))))
      (bad "The terms of a segment aren't in order")

      (not (every? #(< % n) last-docs))
      (bad "A posting names a document that the segment doesn't have")

      (and (<= 0 highest)
           (<= width (quot highest postings/positions-per-field)))
      (bad "A position is in a field that the segment doesn't have")

      (not= n (count (into #{} (map :id) records)))
      (bad "Two documents of a segment have the same id")

      (not (every? #(and (integer? %) (< -1 % n)) deleted))
      (bad "A removed document is not of the segment"))
    (doseq [[d {own :lengths}] (map-indexed vector records)]
      (segment/copy-ints! own 0 lengths (* d width) width))
    (segment/segment-of
     (assoc (encoded lists width unpositioned)
            :ids          (mapv :id records)
            :stored       (mapv :stored records)
            :terms        terms
            :doc-start    (starts (segment/as-ints
                                   (mapv #(alength ^ints (:docs %)) lists)))
            :lengths      lengths
            :field-stats  (segment/field-stats lengths n width)
            :unpositioned unpositioned
            :deleted      (set deleted)
            :name         name))))

(defn- read-segment
  "The segment in `body`, the CIFF file named `name`, with the documents
  numbered `deleted` removed, or throws ::malformed."
  [name body deleted]
  (let [bs     #?(:clj  body
                  :cljs (if (instance? js/ArrayBuffer body)
                          (js/Uint8Array. body)
                          body))
        ^ints cursor (segment/make-ints 1)
        header (when #?(:clj  (bytes? bs)
                        :cljs (instance? js/Uint8Array bs))
                 (first (read-messages! read-header! bs cursor 1)))]
    (cond
      (nil? header)
      (throw (malformed "A segment is not bytes" {:name name}))

      (< version (long (:version header 0)))
      (throw (ex-info "A segment is of a newer format"
                      {:type ::newer-format :name name}))

      :else
      (let [unpositioned (:unpositioned header #{})
            lists        (read-messages! (partial read-list! unpositioned)
                                         bs cursor (:lists header))
            records      (read-messages! read-record! bs cursor
                                         (:docs header))]
        (when (< (aget cursor 0) (postings/byte-count bs))
          (throw (malformed "A segment goes on after its documents"
                            {:name name})))
        (segment-from name lists records unpositioned deleted)))))

(defn- read-manifest
  "The manifest in the EDN `body`, text or its UTF-8 bytes, or throws
  ::malformed, or ::newer-format for one of a newer version of the format."
  [body]
  (let [text     (if (or (nil? body) (string? body))
                   body
                   (utf8-text body 0 (postings/byte-count body)))
        manifest (when (string? text)
                   (read-edn text :manifest))
        fields   (:fields manifest)
        named?   #(and (map? %)
                       (segment-file? (:name %))
                       (set? (:deleted %)))]
    (cond
      (not (and (map? manifest)
                (integer? (:version manifest))))
      (throw (malformed "The manifest of the index is missing or not one"
                        {:name manifest-name}))

      (< version (long (:version manifest)))
      (throw (ex-info "The index is of a newer format"
                      {:type ::newer-format :name manifest-name}))

      (not (and (map? fields)
                (= (set (vals fields)) (set (range (count fields))))
                (vector? (:segments manifest))
                (every? named? (:segments manifest))))
      (throw (malformed "The manifest of the index is not one"
                        {:name manifest-name}))

      :else
      manifest)))

(defn- read-index
  "The index in the files `fetched`, as read-files reads it."
  [fetched]
  (let [bodies   (into {} (map (juxt :name :body)) fetched)
        manifest (read-manifest (get bodies manifest-name))
        segments (mapv (fn [{:keys [name deleted]}]
                         (if-let [body (get bodies name)]
                           (read-segment name body deleted)
                           (throw (malformed "A segment of the index is missing"
                                             {:name name}))))
                       (:segments manifest))
        index    (indexing/with-segments
                  (cond-> {:segments []
                           :docs     {}
                           :fields   (:fields manifest)}
                    (:defaults manifest)
                    (assoc :defaults (:defaults manifest)))
                  segments
                  0)]
    (when-not (= (count (:docs index)) (reduce + 0 (map segment/live segments)))
      (throw (malformed "Two documents of the index have the same id"
                        {:name manifest-name})))
    (when-not (every? #(<= (.-width ^Segment %) (count (:fields index)))
                      segments)
      (throw (malformed "A segment has fields that the index doesn't"
                        {:name manifest-name})))
    index))

(defn read-files
  "The index in the files `fetched`, maps of :name and :body, with the
  :term-fn of `opts`: the manifest index.edn, as text or UTF-8 bytes, and
  the segments it lists.

  A function isn't in the files, so give the :term-fn that the index was
  built with again. It throws ::malformed when a file is missing or
  doesn't read, e.g. for an app to build the index again, and
  ::newer-format for files of a newer version of the format."
  ([fetched]
   (read-files fetched {}))
  ([fetched opts]
   (search.queries/checked opts)
   (indexing/with-term-fn (read-index fetched) (:term-fn opts))))

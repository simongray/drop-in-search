(ns dk.simongray.drop-in-search.ciff
  "An index of dk.simongray.drop-in-search kept as documents in a store: a
  manifest in EDN, and a document for each segment in the Common Index
  File Format, CIFF, which other search engines can read.

  The plan function gives the documents to put and the names to delete,
  and read-documents reads them back. Neither touches a store, so the
  documents can go wherever an app keeps things, e.g. a folder or
  IndexedDB.

  CIFF is that of https://github.com/osirrc/ciff, its README and its
  CommonIndexFileFormat.proto of 2020-03. What CIFF lacks has fields of
  its own, which a reader of CIFF skips, as the encoding of protobuf has
  it, https://protobuf.dev/programming-guides/encoding/"
  (:require #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [dk.simongray.drop-in-search :as search]
            [dk.simongray.drop-in-search.segment :as segment
             #?@(:cljs [:refer [Segment]])])
  #?(:clj (:import [dk.simongray.drop_in_search.segment Segment]
                   [java.nio.charset StandardCharsets])))

(def ^:no-doc version
  "The version of the format of the documents of an index, which changes
  only when the meaning of a part changes."
  1)

(def ^:no-doc manifest-name
  "The name of the document that lists the segments of an index."
  "index.edn")

(defn- make-bytes
  [n]
  #?(:clj  (byte-array n)
     :cljs (js/Uint8Array. n)))

(defn- byte-count
  ^long [bs]
  #?(:clj  (alength ^bytes bs)
     :cljs (.-length bs)))

(defn- byte-at
  "The byte at `i` of the bytes `bs`, from 0 to 255."
  ^long [bs ^long i]
  #?(:clj  (bit-and (aget ^bytes bs i) 0xFF)
     :cljs (aget bs i)))

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
  (+ at (byte-count src)))

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
                         :bytes  (byte-count v)
                         :packed (reduce + 0 (map varint-size v)))
        wire           (case kind :varint :varint :double :i64 :len)]
    (+ (varint-size (tag field wire))
       (if (#{:bytes :packed} kind) (varint-size body) 0)
       body)))

(defn- put-field!
  "Put the field `fld`, as field-size takes it, into the bytes `bs` at `at`,
  and give where the next one goes."
  ^long [bs ^long at fld]
  (let [[field kind v] fld]
    (case kind
      :varint (put-varint! bs (put-varint! bs at (tag field :varint)) v)
      :double (put-double! bs (put-varint! bs at (tag field :i64)) v)
      :bytes  (let [at (put-varint! bs at (tag field :len))]
                (put-bytes! v bs (put-varint! bs at (byte-count v))))
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

;; The CIFF messages, with these fields of their own: a PostingsList has
;; the positions of its postings as field 15, packed, each posting's first
;; whole and the rest as the gap after the one before, a DocRecord has the
;; length of each field as field 14, packed, and what is stored as EDN as
;; field 15, and the Header has the version of this format as field 15
(defn- header-fields
  [^Segment segment]
  (let [n-terms (alength ^objects (.-terms segment))
        n-docs  (segment/size segment)
        total   (areduce ^ints (.-lengths segment) i n 0
                         (+ n (aget ^ints (.-lengths segment) i)))]
    [[1 :varint 1]
     [2 :varint n-terms]
     [3 :varint n-docs]
     [4 :varint n-terms]
     [5 :varint n-docs]
     [6 :varint total]
     [7 :double (if (pos? n-docs) (/ (double total) n-docs) 0.0)]
     [8 :bytes (utf8 "A segment of an index of dk.simongray.drop-in-search")]
     [15 :varint version]]))

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

(defn- reduce-positions
  "Reduce the positions of the postings of `segment` from `from` to `to`
  with `f` from `init`, as they're packed: each posting's first whole and
  the rest as the gap after the one before."
  [f init ^Segment segment from to]
  (let [^ints pos-start (.-pos-start segment)
        ^ints positions (.-positions segment)
        to              (long to)]
    (loop [k (long from) i (long (aget pos-start from)) acc init]
      (if (< k to)
        (let [first? (= i (aget pos-start k))
              gap    (if first?
                       (aget positions i)
                       (- (aget positions i) (aget positions (dec i))))
              next-k (if (= (inc i) (aget pos-start (inc k))) (inc k) k)]
          (recur next-k (inc i) (f acc gap)))
        acc))))

(defn- list-sizes
  "The bytes of the body of the PostingsList of the term `t` of `segment`,
  whose UTF-8 is `term-bytes`, and of its packed positions, as a pair."
  [^Segment segment term-bytes t]
  (let [^ints doc-start (.-doc-start segment)
        ^ints docs      (.-docs segment)
        ^ints pos-start (.-pos-start segment)
        from            (aget doc-start t)
        to              (aget doc-start (inc t))
        packed          (reduce-positions #(+ (long %1) (varint-size %2))
                                          0 segment from to)
        postings        (loop [k from n 0]
                          (if (< k to)
                            (let [gap  (- (aget docs k)
                                          (if (= k from) 0 (aget docs (dec k))))
                                  tf   (- (aget pos-start (inc k))
                                          (aget pos-start k))
                                  body (+ 2 (varint-size gap) (varint-size tf))]
                              (recur (inc k) (+ n 1 (varint-size body) body)))
                            n))
        cf              (- (aget pos-start to) (aget pos-start from))
        n               (byte-count term-bytes)]
    [(+ 1 (varint-size n) n
        1 (varint-size (- to from))
        1 (varint-size cf)
        postings
        1 (varint-size packed) packed)
     packed]))

(defn- put-list!
  "Put the PostingsList of the term `t` of `segment` into the bytes `bs` at
  `at`, with its UTF-8 `term-bytes` and its `sizes` as list-sizes gives
  them, and give where the next one goes."
  [bs at ^Segment segment term-bytes [body packed] t]
  (let [^ints doc-start (.-doc-start segment)
        ^ints docs      (.-docs segment)
        ^ints pos-start (.-pos-start segment)
        from            (aget doc-start t)
        to              (aget doc-start (inc t))
        put             (fn [at field kind v] (put-field! bs at [field kind v]))
        at              (-> (put-varint! bs at body)
                            (put 1 :bytes term-bytes)
                            (put 2 :varint (- to from))
                            (put 3 :varint (- (aget pos-start to)
                                              (aget pos-start from))))
        posting-tag     (tag 4 :len)
        docid-tag       (tag 1 :varint)
        tf-tag          (tag 2 :varint)
        at              (loop [k from at (long at)]
                          (if (< k to)
                            (let [gap  (- (aget docs k)
                                          (if (= k from) 0 (aget docs (dec k))))
                                  tf   (- (aget pos-start (inc k))
                                          (aget pos-start k))
                                  body (+ 2 (varint-size gap) (varint-size tf))
                                  at   (put-varint! bs at posting-tag)
                                  at   (put-varint! bs at body)
                                  at   (put-varint! bs at docid-tag)
                                  at   (put-varint! bs at gap)
                                  at   (put-varint! bs at tf-tag)]
                              (recur (inc k) (put-varint! bs at tf)))
                            at))
        at              (put-varint! bs at (tag 15 :len))]
    (reduce-positions #(put-varint! bs %1 %2)
                      (put-varint! bs at packed)
                      segment from to)))

(defn- segment-body
  "The document of `segment` in CIFF, as bytes."
  [^Segment segment]
  (binding [*print-length* nil
            *print-level*  nil]
    (let [terms      (mapv utf8 (.-terms segment))
          sizes      (mapv #(list-sizes segment (terms %) %)
                           (range (count terms)))
          records    (mapv #(doc-fields segment %)
                           (range (segment/size segment)))
          header     (header-fields segment)
          total      (+ (delimited-size header)
                        (reduce + 0 (map (fn [[body]]
                                           (+ (varint-size body) body))
                                         sizes))
                        (reduce + 0 (map delimited-size records)))
          bs         (make-bytes total)
          at         (put-message! bs 0 header)
          at         (reduce #(put-list! bs %1 segment (terms %2) (sizes %2) %2)
                             at
                             (range (count terms)))]
      (reduce #(put-message! bs %1 %2) at records)
      bs)))

(defn- manifest
  "The manifest of `index` as EDN: its segments by the names of their
  documents, with the numbers of their removed documents, and its fields
  and defaults."
  [index]
  (binding [*print-length* nil
            *print-level*  nil]
    (pr-str (cond-> {:version  version
                     :fields   (:fields index)
                     :segments (mapv (fn [^Segment segment]
                                       {:name    (segment/segment-name segment)
                                        :deleted (.-deleted segment)})
                                     (:segments index))}
              (:defaults index) (assoc :defaults (:defaults index))))))

(defn- segment-document?
  [name]
  (boolean (re-matches #"segment-[0-9a-f]{16}\.ciff" (str name))))

(defn plan
  "The documents to put into a store that holds the documents of the
  `names`, and the names to delete from it, so that it holds `index`.

  It's a map of :put, documents as maps of :name and :body to put in this
  order, and :delete, the names of the documents of segments that `index`
  no longer has. Without `names`, it's every document. The store needs
  a place of its own, e.g. a folder, since plan deletes the documents of
  any index there but this one.

  The manifest index.edn lists the segments, and its :body is EDN. Each
  segment is a document in CIFF, named after what it holds, so it's put
  once. Its :body is bytes: a byte array on the JVM and a Uint8Array in
  JavaScript. Put them all before you delete, and the manifest last, so
  that a store that a crash stops in between still holds an index:

      (let [{:keys [put delete]} (plan idx (map :name (list-documents)))]
        (doseq [{:keys [name body]} put]
          (put-document! name body))
        (run! delete-document! delete))"
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
                   (filter #(and (segment-document? %)
                                 (not (contains? by-name %))))
                   sort
                   vec)})))

(defn- malformed
  "An error of a document that doesn't read, with the `message` and the
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
      (let [b (byte-at bs at)
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
  "The `packed` positions of the postings with the frequencies `tfs`, both
  arrays, each posting's first whole and the rest as the gap after the one
  before, as the positions themselves, or nil when they don't add up or
  don't go up."
  [^ints packed ^ints tfs]
  (let [n               (alength packed)
        ^ints positions (segment/make-ints n)]
    (loop [k 0 i 0 start 0]
      (cond
        (= i n)
        (when (and (= k (alength tfs)) (= i start))
          positions)

        (= i start)
        (when (and (< k (alength tfs)) (pos? (aget tfs k)))
          (aset positions i (aget packed i))
          (recur (inc k) (inc i) (+ i (aget tfs k))))

        :else
        (let [at (+ (aget positions (dec i)) (aget packed i))]
          (when (and (pos? (aget packed i)) (<= at 2147483647))
            (aset positions i (int at))
            (recur k (inc i) start)))))))

(defn- read-list!
  "The PostingsList of the message at the cursor of the bytes `bs`, which
  ends at `end`, as a map of its :term, and of its :docs, :tfs and
  :positions as arrays, or throws ::malformed."
  [bs ^ints cursor ^long end]
  (let [;; a Posting takes four bytes at least
        cap           (quot (- end (aget cursor 0)) 4)
        ^ints docs    (segment/make-ints cap)
        ^ints tfs     (segment/make-ints cap)
        ^ints posting (segment/make-ints 2)
        term-tag      (tag 1 :len)
        posting-tag   (tag 4 :len)
        positions-tag (tag 15 :len)
        out-of-order  #(malformed "A postings list is out of order" {:term %})]
    (loop [term nil n 0 doc 0 packed nil]
      (if (< (aget cursor 0) end)
        (let [t (read-varint! bs cursor end)]
          (cond
            (= t term-tag)
            (recur (read-text! bs cursor end) n doc packed)

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
              (recur term (inc n) doc packed))

            (= t positions-tag)
            (recur term n doc (read-packed! bs cursor end))

            :else
            (do (skip! bs cursor end (rem t 8))
                (recur term n doc packed))))
        (let [tfs       (segment/slice tfs 0 n)
              positions (some-> packed (absolute-positions tfs))]
          (when-not (and term positions)
            (throw (malformed "A postings list has no term or no positions"
                              {:term term})))
          {:term      term
           :docs      (segment/slice docs 0 n)
           :tfs       tfs
           :positions positions})))))

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
  `end`, as a map of its :lists, :docs and :version."
  [bs ^ints cursor ^long end]
  (reduce-fields (fn [acc field wire]
                   (case [field wire]
                     [2 0]  (assoc acc :lists (read-varint! bs cursor end))
                     [3 0]  (assoc acc :docs (read-varint! bs cursor end))
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
  (let [end (byte-count bs)]
    (loop [acc (transient []) i 0]
      (if (< i (long n))
        (let [to (read-len! bs cursor end)
              m  (read-message! bs cursor to)]
          (aset cursor 0 (int to))
          (recur (conj! acc m) (inc i)))
        (persistent! acc)))))

(defn- joined-ints
  "The int `arrays` one after the other, as one array."
  [arrays]
  (let [^ints out (segment/make-ints
                   (reduce + 0 (map #(alength ^ints %) arrays)))]
    (reduce (fn [at ^ints arr]
              (segment/copy-ints! arr 0 out at (alength arr))
              (+ (long at) (alength arr)))
            0
            arrays)
    out))

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

(defn- segment-from
  "The segment of the postings `lists` and the doc `records` that a CIFF
  document named `name` holds, with the documents numbered `deleted`
  removed, or throws ::malformed."
  [name lists records deleted]
  (let [n               (count records)
        widths          (map #(some-> ^ints (:lengths %) alength) records)
        width           (or (first widths) 0)
        terms           (mapv :term lists)
        last-docs       (keep #(let [^ints docs (:docs %)]
                                 (when (pos? (alength docs))
                                   (aget docs (dec (alength docs)))))
                              lists)
        ^ints positions (joined-ints (map :positions lists))
        highest         (areduce positions i m 0 (max m (aget positions i)))
        ^ints lengths   (segment/make-ints (* n width))
        bad             #(throw (malformed % {:name name}))]
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

      (and (pos? (alength positions))
           (<= width (quot highest segment/span)))
      (bad "A position is in a field that the segment doesn't have")

      (not= n (count (into #{} (map :id) records)))
      (bad "Two documents of a segment have the same id")

      (not (every? #(and (integer? %) (< -1 % n)) deleted))
      (bad "A removed document is not of the segment"))
    (doseq [[d {own :lengths}] (map-indexed vector records)]
      (segment/copy-ints! own 0 lengths (* d width) width))
    (segment/segment-of
     {:ids         (mapv :id records)
      :stored      (mapv :stored records)
      :terms       terms
      :doc-start   (starts (segment/as-ints (mapv #(alength ^ints (:docs %))
                                                  lists)))
      :docs        (joined-ints (map :docs lists))
      :pos-start   (starts (joined-ints (map :tfs lists)))
      :positions   positions
      :lengths     lengths
      :field-stats (segment/field-stats lengths n width)
      :deleted     (set deleted)
      :name        name})))

(defn- read-segment
  "The segment in `body`, the CIFF document named `name`, with the documents
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
      (let [lists   (read-messages! read-list! bs cursor (:lists header))
            records (read-messages! read-record! bs cursor (:docs header))]
        (when (< (aget cursor 0) (byte-count bs))
          (throw (malformed "A segment goes on after its documents"
                            {:name name})))
        (segment-from name lists records deleted)))))

(defn- read-manifest
  "The manifest in the EDN `body`, text or its UTF-8 bytes, or throws
  ::malformed, or ::newer-format for one of a newer version of the format."
  [body]
  (let [text     (if (or (nil? body) (string? body))
                   body
                   (utf8-text body 0 (byte-count body)))
        manifest (when (string? text)
                   (read-edn text :manifest))
        fields   (:fields manifest)
        named?   #(and (map? %)
                       (segment-document? (:name %))
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

(defn read-documents
  "The index in the documents `fetched`, maps of :name and :body as plan
  puts them, the manifest index.edn and the segments that it lists. The
  manifest can be text or its UTF-8 bytes.

  It throws ::malformed when a document is missing or doesn't read, e.g.
  for an app to build the index again, and ::newer-format for documents
  of a newer version of the format."
  [fetched]
  (let [bodies   (into {} (map (juxt :name :body)) fetched)
        manifest (read-manifest (get bodies manifest-name))
        segments (mapv (fn [{:keys [name deleted]}]
                         (if-let [body (get bodies name)]
                           (read-segment name body deleted)
                           (throw (malformed "A segment of the index is missing"
                                             {:name name}))))
                       (:segments manifest))
        index    (search/with-segments
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

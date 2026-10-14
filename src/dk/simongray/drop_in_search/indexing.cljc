(ns dk.simongray.drop-in-search.indexing
  "The segments of an index, and how adding and removing documents changes
  them, as in Lucene's index, https://lucene.apache.org/core/9_11_1/core/

  An index is a map of:

  - :segments, a vector of its segments
  - :docs, a map of each id to its place, one number of the index of its
    segment and its number there
  - :fields, a map of each field to its number
  - :defaults, the options of its queries, if any

  Its :term-fn, if any, is in its metadata, since a function can't be
  printed with it."
  (:require [dk.simongray.drop-in-search.segment :as segment
             #?@(:cljs [:refer [Segment]])])
  #?(:clj (:import [dk.simongray.drop_in_search.segment Segment])))

(defn ^:no-doc term-fn
  "The function that maps each term of `index`, or nil."
  [index]
  (::term-fn (meta index)))

(defn ^:no-doc with-term-fn
  "The `index` with `f` as the function that maps each of its terms, or
  as it is for a nil `f`."
  [index f]
  (cond-> index
    f (vary-meta assoc ::term-fn f)))

;; One number takes far less memory than a pair of them, in a map of every
;; document of an index
(defn ^:no-doc place
  "The place of the document numbered `no` in the segment at `i` of an
  index, as one number."
  [i no]
  (+ (* (long i) 4294967296) (long no)))

(defn ^:no-doc place-parts
  "The index of the segment and the number of the document at the place
  `p`, as a pair."
  [p]
  [(quot (long p) 4294967296) (rem (long p) 4294967296)])

(defn- located
  "The `docs` of an index, a map of each id to its place, with those of
  the `segments` from the one at `i` on."
  [docs segments i]
  (persistent!
   (reduce (fn [docs i]
             (let [^Segment segment (nth segments i)
                   deleted          (.-deleted segment)]
               (reduce-kv (fn [docs no id]
                            (if (contains? deleted no)
                              docs
                              (assoc! docs id (place i no))))
                          docs
                          (.-ids segment))))
           (transient docs)
           (range i (count segments)))))

(defn ^:no-doc with-segments
  "The `index` with the `segments`, and its documents in them found again
  from the segment at `i` on."
  [index segments i]
  (assoc index
         :segments segments
         :docs     (located (:docs index) segments i)))

(defn- merged-from
  "The `index` with its segments from the one at `i` on merged into one."
  [index i]
  (let [segments (:segments index)]
    (with-segments index
                   (conj (subvec segments 0 i)
                         (segment/combined (subvec segments i)))
                   i)))

;; Each document is copied about once for each doubling of the documents
;; after it, and an index keeps a few segments, each larger than those
;; after it
(defn- balanced
  "The `index` with its last two segments merged while the last holds at
  least half the documents of the one before it."
  [index]
  (let [segments (:segments index)
        n        (count segments)]
    (if (and (<= 2 n)
             (<= (segment/live (nth segments (- n 2)))
                 (* 2 (segment/live (peek segments)))))
      (recur (merged-from index (- n 2)))
      index)))

(defn ^:no-doc compacted
  "The `index` with all its segments merged into one."
  [index]
  (if (< 1 (count (:segments index)))
    (merged-from index 0)
    index))

(defn- pruned
  "The `index` with each segment that holds more removed documents than
  others written again without them, or left out when it holds no others."
  [index]
  (let [segments (:segments index)
        sparse?  #(< (segment/live %) (count (.-deleted ^Segment %)))
        i        (first (keep-indexed #(when (sparse? %2) %1) segments))]
    (if (nil? i)
      index
      (with-segments index
                     (into (subvec segments 0 i)
                           (keep #(cond
                                    (not (sparse? %))       %
                                    (pos? (segment/live %)) (segment/combined
                                                             [%])
                                    :else                   nil))
                           (subvec segments i))
                     i))))

;; A removed document stays in its segment, marked, until a merge leaves
;; it out, as in Lucene
(defn ^:no-doc removed
  "The `index` without the documents of `ids`."
  [index ids]
  (let [found (select-keys (:docs index) ids)]
    (if (empty? found)
      index
      (let [by-segment (reduce (fn [acc [_ p]]
                                 (let [[i no] (place-parts p)]
                                   (update acc i (fnil conj []) no)))
                               {}
                               found)]
        (-> index
            (update :segments #(reduce-kv (fn [segments i nos]
                                            (update segments i
                                                    segment/with-removed
                                                    nos))
                                          %
                                          by-segment))
            (update :docs #(persistent! (reduce dissoc!
                                                (transient %)
                                                (keys found))))
            pruned
            balanced)))))

(defn- latest
  "The `docs` without those that a later one with the same id replaces,
  in order."
  [docs]
  (let [last-at (zipmap (map :id docs) (range))]
    (keep-indexed #(when (= %1 (last-at (:id %2))) %2) docs)))

(defn- with-segment
  "The `index` with a new segment of the documents `docs`, the terms of
  whose fields `f` maps, if given, with the :max-field-terms and
  :without-positions of the index."
  [index f docs]
  (let [opts         (:defaults index)
        entries      (mapv #(update % :fields segment/analyzed-fields f
                                    (:max-field-terms opts))
                           docs)
        numbers      (reduce segment/numbered
                             (:fields index)
                             (map :fields entries))
        unpositioned (into #{} (keep numbers) (:without-positions opts))
        segments     (:segments index)]
    (-> (assoc index :fields numbers)
        (with-segments (conj segments
                             (segment/built numbers unpositioned entries))
                       (count segments)))))

;; A segment is built of at most 4096 documents at once, so that building
;; it takes little memory, and merges join the segments
(defn- with-segments-of
  "The `index` with the documents `docs`, which replace any of the same
  ids, and `merged`, a function of an index, applied after each segment
  of them."
  [index docs merged]
  (when-let [doc (some #(when (nil? (:id %)) %) docs)]
    (throw (ex-info "A document needs an :id" {:document doc})))
  (let [docs (latest docs)
        f    (term-fn index)]
    (reduce #(merged (with-segment %1 f %2))
            (removed index (map :id docs))
            (partition-all 4096 docs))))

(defn ^:no-doc added
  "The `index` with the documents `docs`, which replace any of the same
  ids."
  [index docs]
  (with-segments-of index docs balanced))

;; The segments of the documents are merged once they're all built, which
;; copies each document once
(defn ^:no-doc indexed
  "The `index` with the documents `docs`, which replace any of the same
  ids, in one segment."
  [index docs]
  (compacted (with-segments-of index docs identity)))

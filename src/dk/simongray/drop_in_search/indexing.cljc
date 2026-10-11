(ns dk.simongray.drop-in-search.indexing
  "The segments of an index, and how adding and removing documents changes
  them, as in Lucene's index, https://lucene.apache.org/core/9_11_1/core/

  An index is a map of:

  - :segments, a vector of its segments
  - :docs, a map of each id to the index of its segment and its number
    there
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

(defn- located
  "The `docs` of an index, a map of each id to the index of its segment and
  its number there, with those of the `segments` from the one at `i` on."
  [docs segments i]
  (persistent!
   (reduce (fn [docs i]
             (let [^Segment segment (nth segments i)
                   deleted          (.-deleted segment)]
               (reduce-kv (fn [docs no id]
                            (if (contains? deleted no)
                              docs
                              (assoc! docs id [i no])))
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
      (let [by-segment (reduce (fn [acc [_ [i no]]]
                                 (update acc i (fnil conj []) no))
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

;; A segment is built of at most 4096 documents at once, so that building
;; it takes little memory, and merges join the segments
(defn ^:no-doc added
  "The `index` with the documents `docs`, which replace any of the same
  ids."
  [index docs]
  (when-let [doc (some #(when (nil? (:id %)) %) docs)]
    (throw (ex-info "A document needs an :id" {:document doc})))
  (let [docs (latest docs)
        f    (term-fn index)]
    (reduce (fn [index chunk]
              (let [entries  (mapv #(update % :fields segment/analyzed-fields f)
                                   chunk)
                    numbers  (reduce segment/numbered
                                     (:fields index)
                                     (map :fields entries))
                    segments (:segments index)]
                (-> (assoc index :fields numbers)
                    (with-segments (conj segments
                                         (segment/built numbers entries))
                                   (count segments))
                    balanced)))
            (removed index (map :id docs))
            (partition-all 4096 docs))))

(ns dk.simongray.drop-in-search.matching
  "The documents of each segment of an index that match a prepared query,
  and their scores, its clauses combined as in Lucene's BooleanQuery,
  https://lucene.apache.org/core/9_11_1/core/"
  (:require [dk.simongray.drop-in-search.hits :as hits]
            [dk.simongray.drop-in-search.queries :as search.queries]
            [dk.simongray.drop-in-search.scoring :as scoring]
            [dk.simongray.drop-in-search.segment :as segment
             #?@(:cljs [:refer [Segment]])])
  #?(:clj (:import [dk.simongray.drop_in_search.segment Segment])))

(declare hits-in)

(defn- excluded
  "The `found` hits in `segment` without those of the `negative` queries,
  with the constants of `ctx`."
  [ctx segment negative found]
  (reduce (fn [found q]
            (if (zero? (hits/hit-count found))
              (reduced found)
              (hits/without found (hits-in ctx segment q (:docs found)))))
          found
          negative))

(defn- cost
  "The postings in `segment` of the terms of the prepared query `node`, for
  an :and or a phrase those of its rarest part, so that an :and can start
  from its rarest part."
  [segment node]
  (let [costs   #(map (partial cost segment)
                      (first (search.queries/clauses %)))
        held-by #(segment/postings-count (segment/term-ranges segment %))]
    (case (search.queries/kind node)
      :and    (reduce min ##Inf (costs (:and node)))
      :or     (reduce + 0 (costs (:or node)))
      :phrase (reduce min ##Inf (map held-by (:places node)))
      :term   (held-by (concat (:terms (:summed node)) (:terms (:typos node))))
      0)))

;; Lucene's BooleanQuery: the scores of the clauses that match add up, and
;; a clause of :not, MUST_NOT, only leaves documents out. An :and starts
;; from its rarest clause, and each next one looks only at the documents
;; that are left
(defn- all-of
  "The hits in `segment` among the `candidates` of the documents that
  match all the prepared `queries` but those under a :not, with the
  constants of `ctx`."
  [ctx segment queries candidates]
  (let [[positive negative] (search.queries/clauses queries)
        [rarest & others]   (sort-by #(cost segment %) positive)
        narrowed            (fn [found q]
                              (if (zero? (hits/hit-count found))
                                (reduced found)
                                (->> (hits-in ctx segment q (:docs found))
                                     (hits/added-up found))))]
    (if rarest
      (->> (reduce narrowed (hits-in ctx segment rarest candidates) others)
           (excluded ctx segment negative))
      (hits/no-hits))))

(defn- any-of
  "The hits in `segment` among the `candidates` of the documents that
  match any of the prepared `queries` but those under a :not, with the
  constants of `ctx`."
  [ctx segment queries candidates]
  (let [[positive negative] (search.queries/clauses queries)]
    (if (seq positive)
      (->> positive
           (map #(hits-in ctx segment % candidates))
           (reduce (partial hits/joined :+))
           (excluded ctx segment negative))
      (hits/no-hits))))

(defn- term-hits
  "The hits in `segment` among the `candidates` of the prepared term
  `leaf`, with the saturation of `ctx`: the best of those of its :summed
  terms and of each of its :typos terms at its boost."
  [{:keys [k1]} segment {:keys [weights summed typos]} candidates]
  (let [scored (fn [terms idf]
                 (-> (scoring/range-frequencies segment
                                                weights
                                                (segment/term-ranges segment
                                                                     terms)
                                                candidates)
                     (scoring/saturated idf k1)))
        exact  (when summed
                 [(scored (:terms summed) (:idf summed))])
        close  (map (fn [term boost]
                      (scored [term] (* (double boost) (:idf typos))))
                    (:terms typos)
                    (:boosts typos))]
    (if-let [parts (seq (concat exact close))]
      (reduce (partial hits/joined :max) parts)
      (hits/no-hits))))

;; One walk through each term's postings for all the candidates, as in
;; scoring's add-frequencies!, rather than a search for each candidate
(defn- place-slices
  "For each of the `candidates`, a sorted array of numbers of documents of
  `segment`, the ranges of its positions for a place of a phrase, from the
  `ranges` of the postings of the terms that the place can hold, as an
  array of vectors of pairs, or nil for a document without one."
  [^Segment segment ^ints candidates ranges]
  (let [^ints docs      (.-docs segment)
        ^ints pos-start (.-pos-start segment)
        n               (alength candidates)
        ^objects slices (object-array n)]
    (doseq [[from to] ranges]
      (loop [j 0 k (long from)]
        (when (and (< j n) (< k (long to)))
          (let [d (aget candidates j)
                e (aget docs k)]
            (cond
              (< d e) (recur (segment/seek candidates j n e) k)
              (> d e) (recur j (segment/seek docs k to d))
              :else   (let [slice [(aget pos-start k) (aget pos-start (inc k))]]
                        (aset slices j (conj (or (aget slices j) []) slice))
                        (recur (inc j) (inc k))))))))
    slices))

(defn- holds?
  "Whether one of the `slices`, ranges of sorted positions in `segment`,
  holds the position `x`."
  [^Segment segment slices x]
  (let [^ints positions (.-positions segment)
        x               (long x)]
    (boolean
     (some (fn [[from to]]
             (let [to (long to)
                   i  (segment/lower-bound positions (long from) to x)]
               (and (< i to) (= x (aget positions i)))))
           slices))))

(defn- phrase-starts
  "The positions in `segment` where the places of a phrase start in a row,
  from the `slices` of each place in one document."
  [^Segment segment slices]
  (let [^ints positions (.-positions segment)]
    (for [[from to] (first slices)
          i         (range from to)
          :let      [start (aget positions i)]
          :when     (every? #(holds? segment (nth slices %) (+ start %))
                            (range 1 (count slices)))]
      start)))

(defn- phrase-frequency
  "BM25F's frequency of a phrase in the document `d` of `segment`, from the
  `slices` of each place of it there, with the `weights` of the fields."
  [segment weights d slices]
  (->> (phrase-starts segment slices)
       (map #(scoring/occurrence segment weights d
                                 (quot % segment/positions-per-field)))
       (reduce + 0.0)))

(defn- phrase-hits
  "The hits in `segment` among the `candidates` of the prepared phrase
  `leaf`, with the saturation of `ctx`. The documents of its rarest place
  are those to look in."
  [{:keys [k1]} segment {:keys [weights places idf]} candidates]
  (let [ranges (mapv #(segment/term-ranges segment %) places)]
    (if (some empty? ranges)
      (hits/no-hits)
      (let [rarest     (apply min-key segment/postings-count ranges)
            ^ints docs (:docs (scoring/range-frequencies segment
                                                         weights
                                                         rarest
                                                         candidates))
            by-place   (mapv #(place-slices segment docs %) ranges)
            slices-of  (fn [j]
                         (mapv (fn [^objects slices] (aget slices j))
                               by-place))
            found      (into []
                             (keep #(let [d (aget docs %)
                                          f (phrase-frequency segment
                                                              weights
                                                              d
                                                              (slices-of %))]
                                      (when (pos? f)
                                        [d f])))
                             (range (alength docs)))]
        (scoring/saturated {:docs   (segment/as-ints (mapv first found))
                            :scores (segment/as-doubles (mapv second found))}
                           idf
                           k1)))))

(defn- hits-in
  "The hits in `segment` among the `candidates`, a sorted array of the
  numbers of documents, or all of them when nil, of the prepared query
  `node`, with the constants of `ctx`. A query of :not alone matches
  nothing."
  [ctx segment node candidates]
  (case (search.queries/kind node)
    :and    (all-of ctx segment (:and node) candidates)
    :or     (any-of ctx segment (:or node) candidates)
    :phrase (phrase-hits ctx segment node candidates)
    :term   (term-hits ctx segment node candidates)
    (hits/no-hits)))

(defn- results
  "The documents of `segment` that match the prepared query `node`, as
  vectors of an id, a score and what is stored, without those removed,
  with the constants of `ctx`."
  [ctx ^Segment segment node]
  (let [found           (hits-in ctx segment node nil)
        ^ints docs      (:docs found)
        ^doubles scores (:scores found)
        deleted         (.-deleted segment)
        ids             (.-ids segment)
        stored          (.-stored segment)]
    (into []
          (keep (fn [i]
                  (let [d (long (aget docs i))]
                    (when-not (contains? deleted d)
                      [(nth ids d) (aget scores i) (nth stored d)]))))
          (range (alength docs)))))

(defn ^:no-doc matched
  "The documents of the segments of `ctx` that match the prepared query
  `node`, as vectors of an id, a score and what is stored."
  [ctx node]
  (into [] (mapcat #(results ctx % node)) (:segments ctx)))

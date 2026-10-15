(ns dk.simongray.drop-in-search.like
  "The terms that set some documents apart, and the query of them that
  finds documents like them, as Lucene's MoreLikeThis makes it.

  The comments name the source of each part, and where it departs from it:

  - Lucene's MoreLikeThis,
    https://lucene.apache.org/core/9_11_1/queries/org/apache/lucene/queries/mlt/MoreLikeThis.html
  - Elasticsearch's more_like_this query,
    https://www.elastic.co/docs/reference/query-languages/query-dsl/query-dsl-mlt-query
  - The Rocchio algorithm of relevance feedback, as Manning, Raghavan and
    Schütze give it in Introduction to Information Retrieval, 2008,
    section 9.1.1,
    https://nlp.stanford.edu/IR-book/html/htmledition/the-rocchio71-algorithm-1.html"
  (:require [dk.simongray.drop-in-search.analysis :as analysis]
            [dk.simongray.drop-in-search.postings :as postings
             #?@(:cljs [:refer [Cursor]])]
            [dk.simongray.drop-in-search.scoring :as scoring]
            [dk.simongray.drop-in-search.segment :as segment
             #?@(:cljs [:refer [Segment]])]
            [dk.simongray.drop-in-search.terms :as terms])
  #?(:clj (:import [dk.simongray.drop_in_search.postings Cursor]
                   [dk.simongray.drop_in_search.segment Segment])))

;; Lucene and Elasticsearch look for 25 terms. They leave out a term that
;; the documents hold fewer than 2 times, which suits long texts, but most
;; documents of an app are a title and a few lines, where it leaves out
;; nearly every term. They also leave out a term of fewer than 5 documents,
;; which suits an index of millions, but an index of an app often holds
;; one person's thousands, where a term of a few documents is the best
;; link between them. Rocchio's weights in Manning et al. are 0.75 for the
;; documents liked and 0.15 for those unliked, and here those liked weigh
;; 1, so those unliked weigh 0.15 / 0.75. Elasticsearch asks a match to
;; hold 30% of the terms, but Lucene asks for none, as here, since two
;; rare terms, e.g. a film's year and its star, are often the best link
;; between short documents
(def defaults
  "The options of more-like-this, when its options don't set them:

  - :max-query-terms, the most terms that the query looks for
  - :min-term-freq, the fewest times that the documents hold a term
  - :min-doc-freq, the fewest documents of the index that hold a term
  - :unlike-weight, how much the documents of :unlike weigh against
    those liked"
  {:max-query-terms 25
   :min-term-freq   1
   :min-doc-freq    2
   :unlike-weight   0.2})

(defn- document-of
  "The document or text `x` as a document, a text as one of a field of no
  name."
  [x]
  (if (string? x)
    {:fields {nil x}}
    x))

(defn ^:no-doc documents
  "The documents of `x`: one document or text, or a collection of them, as
  documents."
  [x]
  (mapv document-of (if (or (string? x) (map? x)) [x] x)))

(defn- field-weight
  "How much a term of `field` counts by the :boosts and :fields of `opts`:
  its boost, or 1, and 0 when it isn't one of the :fields. A text of no
  field counts 1."
  [{:keys [boosts fields]} field]
  (cond
    (nil? field)
    1

    (and fields (not (contains? fields field)))
    0

    :else
    (get boosts field 1)))

;; Lucene counts each field on its own, but here, as in BM25F, a term's
;; frequency is its occurrences weighted by their fields and added up, so
;; a word in a title boosted by 3 counts 3 times. The fields are read in
;; order so that the sums are the same on both platforms
(defn- frequencies-of
  "A map of each term of the document `doc` to its frequency, the times it
  occurs weighted by its fields, analyzed as an index of the :term-fn of
  `ctx` and with the :max-field-terms of `opts` does it."
  [{:keys [term-fn]} {:keys [max-field-terms] :as opts} doc]
  (let [analyzed (segment/analyzed-fields (:fields doc)
                                          term-fn
                                          max-field-terms)]
    (persistent!
     (reduce (fn [acc field]
               (let [w (double (field-weight opts field))]
                 (if (pos? w)
                   (reduce (fn [acc t]
                             (assoc! acc t (+ (double (get acc t 0.0)) w)))
                           acc
                           (mapcat #(if (vector? %) % [%])
                                   (get analyzed field)))
                   acc)))
             (transient {})
             (sort-by str (keys analyzed))))))

;; Lucene's isNoiseWord, with the stop words analyzed as the index's
;; terms are, so that a stop word finds its term
(defn- noise-pred
  "A predicate of a term that is noise by the `opts`: shorter than
  :min-word-length, longer than :max-word-length, or one of the
  :stop-words, with the :term-fn of `ctx`."
  [{:keys [term-fn]} {:keys [min-word-length max-word-length stop-words]}]
  (let [stops (into #{}
                    (comp (mapcat #(analysis/mapped term-fn
                                                    (analysis/tokens %)))
                          (mapcat #(if (vector? %) % [%])))
                    stop-words)]
    (fn [term]
      (let [n (count term)]
        (or (and min-word-length (< n (long min-word-length)))
            (and max-word-length (< (long max-word-length) n))
            (contains? stops term))))))

(defn ^:no-doc of-total
  "The number `x`, or the share `x` of `total` when `x` is below 1."
  [x total]
  (if (< x 1)
    (* (double x) (long total))
    x))

;; The groups are counted until there are more than `most`, so that a
;; common term costs only the postings that it takes to tell
(defn- group-count
  "The groups that `group-fn` gives of the ids of the documents of the
  `segments` that hold `term`, counted up to one more than `most`."
  [segments group-fn most term]
  (count
   (reduce (fn [seen ^Segment segment]
             (let [t (segment/term-number segment term)]
               (if (neg? t)
                 seen
                 (let [^Cursor c (segment/cursor segment t)
                       ids       (.-ids segment)
                       deleted   (.-deleted segment)
                       group-of  #(group-fn (nth ids %))]
                   (loop [seen seen]
                     (let [d (.nextDoc c)]
                       (cond
                         (< most (count seen))
                         (reduced seen)

                         (== d postings/no-more)
                         seen

                         (contains? deleted d)
                         (recur seen)

                         :else
                         (recur (conj seen (group-of d))))))))))
           #{}
           segments)))

;; Manning et al. take the vectors of length-normalized documents, so that
;; each document counts the same, however long. The squares are added up
;; in order so that the sum is the same on both platforms
(defn- normalized
  "The vector `v`, a map of terms to weights, at a length of 1."
  [v]
  (let [length (Math/sqrt (reduce + 0.0 (sort (map #(* % %) (vals v)))))]
    (if (pos? length)
      (update-vals v #(/ % length))
      v)))

(defn- centroid
  "The mean of the vectors `vs`, maps of terms to weights, weighted by the
  `weights`, or nil for none."
  [vs weights]
  (let [total (double (reduce + 0.0 weights))]
    (when (pos? total)
      (update-vals (apply merge-with +
                          (map (fn [v w] (update-vals v #(* % (double w))))
                               vs
                               weights))
                   #(/ % total)))))

;; Lucene weighs a term by its frequency and its inverse document frequency
;; of TF-IDF, but here the inverse document frequency is BM25's, which
;; ranks the results. With documents unliked, the weights are Rocchio's:
;; the mean of the documents liked less that of those unliked, times
;; :unlike-weight, with what is no longer positive left out. Elasticsearch
;; instead leaves out every term of a document unliked
(defn ^:no-doc interesting
  "The terms that set the documents `like` apart, those that a query of
  documents like them looks for, with the `opts` of more-like-this and
  the :segments, :total, :weights and :term-fn of `ctx`, and the count of
  the groups of the index, a delay under :groups. They are pairs of a term
  and its weight, the best first."
  [{:keys [segments total weights groups] :as ctx}
   like
   {:keys [unlike weight-fn group-fn] :as opts}]
  (let [{:keys [max-query-terms min-term-freq min-doc-freq max-doc-freq
                max-group-freq unlike-weight]} (merge defaults opts)
        weight-fn (or weight-fn (constantly 1))
        liked     (documents like)
        unliked   (documents unlike)
        tfs       (map #(frequencies-of ctx opts %) liked)
        untfs     (map #(frequencies-of ctx opts %) unliked)
        summed    (apply merge-with + {} tfs)
        noise?    (noise-pred ctx opts)
        most-docs (some-> max-doc-freq (of-total total))
        idfs      (into {}
                        (keep (fn [t]
                                (let [d (terms/doc-count segments weights [t])]
                                  (when (and (<= (long min-doc-freq) d)
                                             (or (nil? most-docs)
                                                 (<= d most-docs)))
                                    [t (scoring/idf total d)]))))
                        (into #{}
                              (comp (mapcat keys) (remove noise?))
                              (concat tfs untfs)))
        vector-of (fn [tf]
                    (normalized (into {}
                                      (keep (fn [[t f]]
                                              (when-let [idf (idfs t)]
                                                [t (* (double f) idf)])))
                                      tf)))
        liking    (centroid (map vector-of tfs) (map weight-fn liked))
        unliking  (centroid (map vector-of untfs) (map weight-fn unliked))
        most      (when (and group-fn max-group-freq)
                    (long (of-total max-group-freq @groups)))
        common?   #(and most (< most (group-count segments group-fn most %)))]
    (->> liking
         (keep (fn [[t w]]
                 (let [w (- (double w)
                            (* (double unlike-weight)
                               (double (get unliking t 0.0))))]
                   (when (and (pos? w)
                              (<= (double min-term-freq) (summed t)))
                     [t w]))))
         ;; equal weights by term, so that the order is the same on both
         ;; platforms
         (sort-by (fn [[t w]] [(- (double w)) t]))
         ;; a term that too many groups hold is counted only when it's
         ;; among the best that are left, since that costs its postings
         (into [] (comp (remove (comp common? first))
                        (take max-query-terms))))))

;; Lucene's createQuery: a clause that a match may hold for each term,
;; boosted by its weight over the best one's times :boost-terms when
;; that's given. Elasticsearch's minimum_should_match, here :min-match, is
;; rounded down as a share of the terms
(defn ^:no-doc query-of
  "The query of the `terms`, pairs of a term and its weight, the best
  first, as data, with the :boost-terms and :min-match of `opts`, or nil
  for no terms. Each term is exact, without typos."
  [terms {:keys [boost-terms min-match]}]
  (when (seq terms)
    (let [best    (double (second (first terms)))
          boosted (and boost-terms (pos? boost-terms))
          least   (when min-match
                    (min (count terms)
                         (long (Math/floor (of-total min-match
                                                     (count terms))))))]
      (cond-> {:or (mapv (fn [[t w]]
                           (cond-> {:term t :fuzzy 0}
                             boosted (assoc :boost (* (double boost-terms)
                                                      (/ (double w) best)))))
                         terms)}
        (and least (< 1 least)) (assoc :min-match least)))))

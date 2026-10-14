(ns dk.simongray.drop-in-search.terms
  "The terms of an index that the words of a query stand for, and what
  scoring them takes.

  A prefix stands for the terms that it starts, and a word with typos for
  the terms within its edits. A query is prepared with them once for all
  the segments, and the words that a result's document holds only with
  typos go under its :typos."
  (:require [clojure.string :as str]
            [dk.simongray.drop-in-search.hits :as hits]
            [dk.simongray.drop-in-search.indexing :as indexing]
            [dk.simongray.drop-in-search.postings :as postings
             #?@(:cljs [:refer [Cursor]])]
            [dk.simongray.drop-in-search.queries :as search.queries]
            [dk.simongray.drop-in-search.ranking :as ranking]
            [dk.simongray.drop-in-search.scoring :as scoring]
            [dk.simongray.drop-in-search.segment :as segment])
  #?(:clj (:import [dk.simongray.drop_in_search.postings Cursor])))

(def ^:no-doc fuzzy-weight
  "How much a match with typos weighs before its edits count, as the
  fuzzyWeight of MiniSearch has it."
  0.45)

(defn- prefixed
  "The terms of `segment` that start with `prefix`, in order, as pairs of a
  term and the number of documents that hold it."
  [segment prefix]
  (let [n (segment/term-count segment)]
    (loop [i   (segment/term-bound segment prefix)
           acc (transient [])]
      (let [term (when (< i n) (segment/term-at segment i))]
        (if (and term (str/starts-with? term prefix))
          (recur (inc i)
                 (conj! acc [term (segment/doc-frequency segment i)]))
          (persistent! acc))))))

(defn- term-counts
  "The terms of the `segments` that start with `prefix`, as a map of each
  to the number of documents that hold it."
  [segments prefix]
  (persistent!
   (reduce (fn [acc [term n]]
             (assoc! acc term (+ (long n) (long (get acc term 0)))))
           (transient {})
           (mapcat #(prefixed % prefix) segments))))

(defn- more-held
  "A comparator of the map entries `x` and `y` of a term and the number of
  documents that hold it: the most documents first, then the terms in
  order."
  [x y]
  (let [c (compare (val y) (val x))]
    (if (zero? c)
      (compare (key x) (key y))
      c)))

(defn- completions
  "The terms of the `segments` that start with `prefix`, at most `most` of
  them, those that the most documents hold."
  [segments prefix most]
  (let [counts (term-counts segments prefix)]
    (if (<= (count counts) (long most))
      (vec (sort (keys counts)))
      (mapv key (ranking/best more-held most counts)))))

(defn- fuzzy-expansions
  "The terms of the `segments` within the edits of the fuzzy `leaf`, at
  most `most` of them, as pairs of a term and its edits: the closest
  first, and of those the ones that the most documents hold. Only terms
  that start with the same character count, which keeps the search short."
  [segments {:keys [term] :as leaf} most]
  (let [edits (long (search.queries/edits leaf))
        n     (count term)]
    (->> (term-counts segments (subs term 0 1))
         (filter #(<= (Math/abs (- (count (key %)) n)) edits))
         (keep (fn [[found held]]
                 (when-let [d (search.queries/distance term found edits)]
                   [found d held])))
         (sort-by (fn [[found d held]] [d (- (long held)) found]))
         (take most)
         (map pop))))

(defn ^:no-doc known?
  "Whether a document of the `segments` holds the term of `leaf`, or with
  :prefix? a term that it starts."
  [segments {:keys [term prefix?]}]
  (boolean
   (some (fn [segment]
           (let [i (segment/term-bound segment term)]
             (and (< i (segment/term-count segment))
                  (if prefix?
                    (str/starts-with? (segment/term-at segment i) term)
                    (= term (segment/term-at segment i))))))
         segments)))

(defn- distinct-docs
  "The documents of `segment` that hold one of the terms numbered `ts`."
  [segment ts]
  (let [^ints seen (segment/make-ints (segment/size segment))]
    (doseq [t ts]
      (let [^Cursor c (segment/cursor segment t)]
        (loop []
          (let [d (.nextDoc c)]
            (when-not (== d postings/no-more)
              (aset seen d 1)
              (recur))))))
    (areduce seen i n 0 (+ n (aget seen i)))))

(defn- segment-doc-count
  "The documents of `segment` with one of `terms` in a field that the
  `weights` count, or in any field when `weights` is nil."
  [segment weights terms]
  (let [ts (segment/term-numbers segment terms)]
    (cond
      (and weights (not (scoring/every-field? weights)))
      (hits/hit-count (scoring/term-frequencies segment weights ts nil))

      (< 1 (count ts))
      (distinct-docs segment ts)

      :else
      (segment/postings-count segment ts))))

(defn- doc-count
  "The documents of the `segments` with one of `terms` in a field that the
  `weights` count, or in any field when `weights` is nil."
  [segments weights terms]
  (reduce + 0 (map #(segment-doc-count % weights terms) segments)))

(defn- leaf-weights
  "The weights of the fields for the term or phrase `leaf`, those of `ctx`
  with all but its :field left out when it names one, and all but the
  fields numbered in its :only when it has those."
  [{:keys [weights numbers]} {:keys [field only]}]
  (cond-> weights
    field (scoring/restricted (get numbers field))
    only  (scoring/only only)))

;; A prefix counts as one term of all the terms it starts, with the number
;; of documents that hold any of them. Lucene blends the expansions of a
;; prefix to the largest document frequency, so that the rarest one doesn't
;; outrank the rest.
;;
;; MiniSearch weighs a match with typos by 0.45 length / (length + edits),
;; and each document counts its best match, so that an exact match outranks
;; one with typos. The expansions share one inverse document frequency, as
;; Lucene blends those of a FuzzyQuery
(defn- prepared-term
  "The term `leaf` with what scoring it in each segment of `ctx` takes:
  its :weights, the :summed terms that it or its completions are, whose
  frequencies add up, and the :typos terms within its edits, each with a
  boost, both with their inverse document frequency."
  [{:keys [segments total max-completions max-expansions] :as ctx}
   {:keys [term prefix?] :as leaf}]
  (let [weights (leaf-weights ctx leaf)
        idf-of  #(scoring/idf total (doc-count segments weights %))
        fuzzy?  (pos? (long (search.queries/edits leaf)))
        terms   (cond
                  prefix? (completions segments term max-completions)
                  fuzzy?  nil
                  :else   [term])
        close   (when fuzzy?
                  (fuzzy-expansions segments leaf max-expansions))
        n       (count term)
        boost   (fn [[_ d]]
                  (if (zero? (long d))
                    1.0
                    (* fuzzy-weight (/ (double n) (+ n (long d))))))]
    (cond-> (assoc leaf :weights weights)
      terms (assoc :summed {:terms terms
                            :idf   (idf-of terms)})
      close (assoc :typos {:terms  (mapv first close)
                           :boosts (mapv boost close)
                           :idf    (idf-of (map first close))}))))

;; Lucene's PhraseQuery: the inverse document frequency of a phrase is the
;; sum of those of its terms
(defn- prepared-phrase
  "The phrase `leaf` with what scoring it in each segment of `ctx` takes:
  its :weights, the :places, the terms that each place of it holds, its
  last term's completions when it's a prefix, and its :idf."
  [{:keys [segments total max-completions] :as ctx}
   {:keys [phrase prefix?] :as leaf}]
  (let [places (conj (mapv vector (pop phrase))
                     (if prefix?
                       (completions segments (peek phrase) max-completions)
                       [(peek phrase)]))]
    (assoc leaf
           :weights (leaf-weights ctx leaf)
           :places  places
           :idf     (reduce + (map #(scoring/idf total
                                                 (doc-count segments nil %))
                                   places)))))

;; Xapian treats a phrase as its words where it has no positions, from
;; 0.8.4 on, https://trac.xapian.org/ticket/56, and so a phrase is itself
;; in the fields with positions, and its words in those without
(defn- placed-phrase
  "The phrase `leaf` as a query of the fields of `ctx` that it counts: the
  phrase in those with positions, or its words in those without, or
  either."
  [{:keys [unpositioned] :as ctx} leaf]
  (let [counted (scoring/counted (leaf-weights ctx leaf))
        without (filterv #(contains? unpositioned %) counted)
        words   #(search.queries/map-leaves (fn [w] (assoc w :only without))
                                            (search.queries/phrase-words leaf))]
    (cond
      (empty? without)
      leaf

      (= (count without) (count counted))
      (words)

      :else
      {:or [(assoc leaf :only (remove (set without) counted)) (words)]})))

(defn ^:no-doc prepared
  "The query `node` with what scoring its terms and phrases takes, worked
  out once for all the segments of `ctx`."
  [ctx node]
  (->> node
       (search.queries/map-leaves #(if (:phrase %) (placed-phrase ctx %) %))
       (search.queries/map-leaves #(if (:phrase %)
                                     (prepared-phrase ctx %)
                                     (prepared-term ctx %)))))

(defn ^:no-doc typos-of
  "The terms of the prepared query `node` that match other terms with
  typos, those under a :not left out, as a map of each to the :exact terms
  it matches, itself or its completions, and the :near ones."
  [node]
  (into {}
        (keep (fn [{:keys [term summed typos]}]
                (let [near (filterv #(not= term %) (:terms typos))]
                  (when (seq near)
                    [term {:exact (or (:terms summed) [term]) :near near}]))))
        (search.queries/leaves node)))

(defn- holds-term?
  "Whether the document `d` of `segment` holds `term`."
  [segment term d]
  (boolean
   (some (fn [t]
           (= (long d) (.advance ^Cursor (segment/cursor segment t) d)))
         (segment/term-numbers segment [term]))))

(defn ^:no-doc with-typos
  "The `result` of a query of `index` with :typos, a map of each term of
  `typos` that its document holds only with typos to the near terms that
  it holds, when there are any."
  [index typos {:keys [id] :as result}]
  (let [[i d]   (indexing/place-parts (get (:docs index) id))
        segment (nth (:segments index) i)
        holds?  #(holds-term? segment % d)
        held    (into {}
                      (keep (fn [[term {:keys [exact near]}]]
                              (when-not (some holds? exact)
                                (let [found (filterv holds? near)]
                                  (when (seq found)
                                    [term found])))))
                      typos)]
    (cond-> result
      (seq held) (assoc :typos held))))

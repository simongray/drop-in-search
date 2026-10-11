(ns dk.simongray.drop-in-search
  "A full-text index of documents, and queries of it ranked by BM25F, on
  both platforms.

  A document is a map of its :id, its :fields, and what a result gives
  back, under :stored. A field holds text, or anything whose text counts,
  such as a number or a set of keywords, but a vector holds terms to take
  as they are. The text of an instant is its date and time in UTC, e.g.
  2024-03-01 09:30:00, and that of anything else is what str gives.

  Most of the code behind these functions is in these namespaces:

  - [[dk.simongray.drop-in-search.indexing]]: adding and removing
    documents
  - [[dk.simongray.drop-in-search.queries]]: the query language
  - [[dk.simongray.drop-in-search.terms]]: the terms that the words of a
    query stand for
  - [[dk.simongray.drop-in-search.matching]]: the documents that match a
    query
  - [[dk.simongray.drop-in-search.scoring]]: the BM25F score of a
    document that matches
  - [[dk.simongray.drop-in-search.hits]]: the documents that match in a
    segment, and their scores, as arrays
  - [[dk.simongray.drop-in-search.ranking]]: the order of the results"
  (:refer-clojure :exclude [remove])
  (:require [dk.simongray.drop-in-search.indexing :as indexing]
            [dk.simongray.drop-in-search.matching :as matching]
            [dk.simongray.drop-in-search.queries :as search.queries]
            [dk.simongray.drop-in-search.ranking :as ranking]
            [dk.simongray.drop-in-search.scoring :as scoring]
            [dk.simongray.drop-in-search.segment :as segment
             #?@(:cljs [:refer [Segment]])]
            [dk.simongray.drop-in-search.terms :as terms])
  #?(:clj (:import [dk.simongray.drop_in_search.segment Segment])))

(defn index
  "An index of the documents `docs`, or an empty one, with `opts` as the
  defaults of its queries, e.g. the :boosts of its fields, and its
  :term-fn.

  The :term-fn is a function of each term of a text that gives the term
  to index and to look for in its place, e.g. a stemmer's, so that posts
  finds post. The other `opts` print with the index, so they must be data,
  but a function can't, so give the :term-fn to restore and
  ciff/read-files again."
  ([]
   {:segments [] :docs {} :fields {}})
  ([docs]
   (index docs {}))
  ([docs {:keys [term-fn] :as opts}]
   {:pre [(map? opts)]}
   (let [defaults (dissoc opts :term-fn)]
     (cond-> (-> (index)
                 (indexing/with-term-fn term-fn)
                 (indexing/added docs)
                 indexing/compacted)
       (seq defaults) (assoc :defaults defaults)))))

(defn restore
  "The `index` read back from EDN with its arrays made again, which a query
  of it otherwise does each time, and the :term-fn of `opts` that it was
  built with, or nil for a nil `index`."
  ([index]
   (restore index {}))
  ([index {:keys [term-fn] :as opts}]
   (some-> index
           (update :segments (partial mapv #(if (instance? Segment %)
                                              %
                                              (segment/segment-of %))))
           (indexing/with-term-fn term-fn))))

(defn remove
  "The `index` without the document `id`, and the `ids` after it."
  ([index]
   index)
  ([index id & ids]
   (indexing/removed (restore index) (cons id ids))))

(defn add
  "The `index` with the document `doc`, and the `docs` after it, each a map
  of :id, :fields and :stored. A document replaces any earlier one with
  the same id."
  ([index]
   index)
  ([index doc & docs]
   (indexing/added (restore index) (cons doc docs))))

(defn ids
  "The ids of the documents in `index`."
  [index]
  (keys (:docs index)))

;; Xapian takes the 100 most frequent completions of a prefix, and Lucene's
;; FuzzyQuery the 50 closest terms to a word with typos
(def default-limits
  "The limits that keep the cost of a query that anyone can type within
  bounds, when its options don't set them:

  - :max-completions, the terms that a prefix stands for, those of the
    most documents, as in Xapian
  - :max-expansions, the terms that a word with typos stands for, the
    closest, as in Lucene's FuzzyQuery"
  {:max-completions 100
   :max-expansions  50})

(defn- name-of
  [field]
  (if (keyword? field)
    (name field)
    (str field)))

(defn query-opts
  "The `opts` over the defaults of `index`, to read a query as the index
  does: with the names of its fields among the :aliases, its :term-fn,
  and a :known-pred that tells the terms its documents hold."
  ([index]
   (query-opts index {}))
  ([index opts]
   (let [{:keys [defaults fields segments] :as index} (restore index)
         opts  (merge defaults opts)
         names (into {} (for [field (keys fields)]
                          [(name-of field) field]))]
     (cond-> (assoc opts
                    :aliases    (merge names (:aliases opts))
                    :known-pred #(terms/known? segments %))
       (indexing/term-fn index) (assoc :term-fn (indexing/term-fn index))))))

(defn- context
  "The constants that preparing and matching a query of `index` with the
  `opts` take."
  [index {:keys [boosts] :as opts}]
  (let [segments (:segments index)
        numbers  (:fields index)
        boosts   (if-let [only (:fields opts)]
                   (into {} (for [field (keys numbers)]
                              [field (if (contains? only field)
                                       (get boosts field 1)
                                       0)]))
                   boosts)]
    {:segments        segments
     :numbers         numbers
     :weights         (scoring/weights numbers
                                       segments
                                       boosts
                                       (:b opts scoring/b))
     :k1              (double (:k1 opts scoring/k1))
     :total           (reduce + 0 (map segment/size segments))
     :max-completions (:max-completions opts)
     :max-expansions  (:max-expansions opts)}))

(defn query
  "The documents of `index` that match the query `q`, best first, as maps
  of :id, :score and :stored, with the `opts` below.

  The query is a string, the same query as data, or a vector of terms
  taken as they are. The `opts` are those of search.queries/parse, and
  these, over the defaults of the index:

  - :limit, the most results, and :offset, how many to skip first
  - :fuzzy, which words match words with typos too: by default those that
    no document holds, true for all, a number for all with at most that
    many edits, up to 2, and false for none but those marked with ~
  - :typo-lengths, the shortest words with one edit and with two, [3 6]
    by default, as Elasticsearch's AUTO
  - :boosts, the weight of a match in each field, 1 by default, where 0
    leaves a field out unless the query names it
  - :fields, the set of fields to search, all by default, which gives the
    other fields the weight 0
  - :k1 and :b, the saturation and length normalization of BM25F
  - :filter-pred, a predicate of a result to keep it
  - :rank-fn, a function of a result to order by, ascending, instead of
    the score
  - :max-completions and :max-expansions, the most terms that a prefix
    and a word with typos stand for

  Equal scores are ordered by id, numbers first and ascending, and other
  ids by hash, so the order is stable on both platforms.

  The results have the :total of the documents that match in their
  metadata, e.g. for \"1 to 10 of 243\", unless a :filter-pred had no need
  to read them all. When words matched others with typos, the metadata
  also has :typos, a map of each such word to those it matched, the
  closest first, and so does each result that holds a word only with
  typos:

      (meta (query idx \"intervew\"))
      ;; => {:total 2 :typos {\"intervew\" [\"interview\" \"interviews\"]}}"
  ([index q]
   (query index q {}))
  ([index q opts]
   {:pre [(map? opts)]}
   (let [index  (restore index)
         {:keys [limit offset] :or {offset 0} :as opts}
         (merge default-limits (:defaults index) opts)
         ctx    (context index opts)
         node   (some->> (query-opts index opts)
                         (search.queries/fuzzy-query q)
                         (terms/prepared ctx))
         found  (if node
                  (matching/matched ctx node)
                  [])
         typos  (terms/typos-of node)
         result (fn [[id score stored]]
                  {:id id :score score :stored stored})
         ranked (ranking/top found
                             result
                             (some-> limit (+ (long offset)))
                             opts)
         page   (subvec ranked (min (long offset) (count ranked)))
         total  (:total (meta ranked))
         facts  (cond-> {}
                  total       (assoc :total total)
                  (seq typos) (assoc :typos (update-vals typos :near)))]
     (cond-> page
       (seq typos) (->> (mapv #(terms/with-typos index typos %)))
       (seq facts) (with-meta facts)))))

(comment
  (def idx
    (index [{:id 1 :fields {:title "Clojure and the value of simplicity"}}
            {:id 2 :fields {:title "Rust" :notes "A little Clojure"}}]
           {:boosts {:title 3 :notes 1}}))
  (query idx "cloj")
  (query idx "title:clojure | notes:clojure")
  (search.queries/parse "notes:clojure" (query-opts idx))
  (search.queries/snippet "A little Clojure" "notes:clojure" (query-opts idx))
  #_.)

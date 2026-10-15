(ns dk.simongray.drop-in-search.ranking
  "The best results of a query: the highest scores first, and equal scores
  by id, unless a function of a result ranks them.")

;; Numbers come first, and other ids compare by hash and then as strings,
;; so that the order is total and the same on both platforms, which hash
;; numbers differently. BM25 ties are common, so this must stay cheap
(defn- compare-ids
  "The order of the ids `x` and `y` of two results of the same score."
  [x y]
  (cond
    (and (number? x) (number? y))
    (compare x y)

    (or (number? x) (number? y))
    (if (number? x) -1 1)

    :else
    (let [hx (long (hash x))
          hy (long (hash y))]
      (cond
        (< hx hy) -1
        (> hx hy) 1
        :else     (compare (str x) (str y))))))

(defn- better
  "A comparator of the found documents `a` and `b`, vectors of an id and a
  score, and what is stored: the higher score first, then by id."
  [a b]
  (let [sa (double (nth a 1))
        sb (double (nth b 1))]
    (cond
      (> sa sb) -1
      (< sa sb) 1
      :else     (compare-ids (nth a 0) (nth b 0)))))

(defn- kept
  "The vector `top` of at most `n` results, sorted by the comparator
  `cmp`, with `result` in its place, or as it is when `result` is no
  better than the worst of `n`."
  [cmp n top result]
  (if (and (= (count top) n)
           (<= 0 (cmp result (peek top))))
    top
    (let [i   (loop [i 0]
                (if (and (< i (count top))
                         (<= (cmp (top i) result) 0))
                  (recur (inc i))
                  i))
          top (into (conj (subvec top 0 i) result) (subvec top i))]
      (if (> (count top) n)
        (pop top)
        top))))

(defn ^:no-doc best
  "The `results` sorted by the comparator `cmp`, at most `n` of them if
  `n` is given."
  [cmp n results]
  (cond
    (and n (<= n 0))
    []

    (or (nil? n) (>= n (Math/sqrt (count results))))
    (let [sorted (sort cmp results)]
      (vec (if n (take n sorted) sorted)))

    ;; the best so far are kept in a small sorted vector, so that most
    ;; candidates cost one comparison with its worst
    :else
    (reduce #(kept cmp n %1 %2) [] results)))

;; Elasticsearch's field collapsing and the group.limit of Solr's result
;; grouping keep the best results of each group, in the order of the rest
(defn- capped
  "A transducer of results that keeps at most `n` of each group that
  `group-fn` gives of a result's id, the first ones, as distinct keeps the
  first of each."
  [group-fn n]
  (fn [rf]
    (let [seen (volatile! {})]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc result]
         (let [group (group-fn (:id result))
               k     (long (get @seen group 0))]
           (if (< k (long n))
             (do (vswap! seen assoc group (inc k))
                 (rf acc result))
             acc)))))))

(defn- in-order
  "The `found` documents best first, as a lazy seq that sorts only the
  best `m` of them at first, and four times as many each time it's read
  past those, from the one at `read`."
  ([found m]
   (in-order found m 0))
  ([found m read]
   (lazy-seq
    (let [best-m (best better m found)]
      (concat (subvec best-m read)
              (when (< (count best-m) (count found))
                (in-order found (* 4 (long m)) (count best-m))))))))

(defn ^:no-doc top
  "The best `n` of the `found` documents, or all of them for a nil `n`, as
  the results that `result-fn` makes of each, with the :filter-pred,
  :rank-fn, :group-fn and :max-per-group of `opts`.

  The results have the :total of those found that the filter and the cap
  of each group keep in their metadata, unless they stopped once they had
  `n`."
  [found result-fn n {:keys [filter-pred rank-fn group-fn max-per-group]}]
  (let [filtered (if filter-pred (filter filter-pred) identity)
        cap      (when (and group-fn max-per-group)
                   (capped group-fn max-per-group))
        page     (fn [kept]
                   (with-meta (if n (vec (take n kept)) kept)
                              {:total (count kept)}))]
    (cond
      rank-fn
      (page (->> (into [] (comp (map result-fn) filtered) found)
                 (map (fn [r] [(rank-fn r) r]))
                 (sort-by first)
                 (into [] (comp (map second) (or cap identity)))))

      ;; a filter that costs, e.g. criteria of a library, reads the best
      ;; first, and more of them only when too few are kept, and so does a
      ;; cap, whose groups count in order
      (and n (or filter-pred cap))
      (let [kept (into []
                       (comp (map result-fn)
                             filtered
                             (or cap identity)
                             (take n))
                       (in-order found (* 2 (max 1 (long n)))))]
        ;; every one was read, and too few kept to stop early
        (cond-> kept
          (< (count kept) (long n)) (with-meta {:total (count kept)})))

      cap
      (page (into [] (comp (map result-fn) filtered cap)
                  (best better nil found)))

      :else
      (let [kept (if filter-pred
                   (filterv #(filter-pred (result-fn %)) found)
                   found)]
        (with-meta (->> kept
                        (best better n)
                        (mapv result-fn))
                   {:total (count kept)})))))

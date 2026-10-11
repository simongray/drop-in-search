(ns dk.simongray.drop-in-search.ranking
  "The best results of a query: the highest scores first, and equal scores
  by id, unless a function of a result ranks them.")

;; Ids that aren't both numbers compare by hash and then as strings, so
;; that the order is total and the same on both platforms, which hash
;; numbers differently. BM25 ties are common, so this must stay cheap
(defn- compare-ids
  "The order of the ids `x` and `y` of two results of the same score."
  [x y]
  (if (and (number? x) (number? y))
    (compare x y)
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

(defn ^:no-doc top
  "The best `n` of the `found` documents, or all of them for a nil `n`, as
  the results that `result-fn` makes of each, with the :filter-pred and
  :rank-fn of `opts`."
  [found result-fn n {:keys [filter-pred rank-fn] :as opts}]
  (cond
    rank-fn
    (->> found
         (map result-fn)
         (filter (or filter-pred any?))
         (map (fn [r] [(rank-fn r) r]))
         (sort-by first)
         (map second)
         (#(if n (take n %) %))
         vec)

    ;; a filter that costs, e.g. criteria of a library, reads the best
    ;; first, and more of them only when too few are kept
    (and filter-pred n)
    (loop [m (* 2 (max 1 (long n))) read 0 kept []]
      (let [best-m (best better m found)
            kept   (into kept
                         (comp (filter #(filter-pred (result-fn %)))
                               (take (- (long n) (count kept))))
                         (subvec best-m read))]
        (if (or (= (count kept) n)
                (>= m (count found)))
          (mapv result-fn kept)
          (recur (* 4 m) (count best-m) kept))))

    :else
    (->> (if filter-pred
           (filter #(filter-pred (result-fn %)) found)
           found)
         (best better n)
         (mapv result-fn))))

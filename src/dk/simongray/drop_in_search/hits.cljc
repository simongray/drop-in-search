(ns dk.simongray.drop-in-search.hits
  "The hits of a query in a segment, and how the hits of its parts
  combine.

  Hits are a map of :docs, the numbers of the documents that match, in
  order, and :scores, the score of each, both arrays."
  (:require [dk.simongray.drop-in-search.segment :as segment]))

(defn ^:no-doc no-hits
  "Hits of no documents."
  []
  {:docs (segment/make-ints 0) :scores (segment/make-doubles 0)})

(defn ^:no-doc hit-count
  "The number of documents of `hits`."
  [hits]
  (alength ^ints (:docs hits)))

(defn ^:no-doc collected
  "The hits of the positive values of `acc`, an array of a value for each
  of the `candidates`, or for each document when nil."
  [^doubles acc ^ints candidates]
  (let [n               (alength acc)
        m               (areduce acc i m 0 (if (pos? (aget acc i)) (inc m) m))
        ^ints docs      (segment/make-ints m)
        ^doubles scores (segment/make-doubles m)]
    (loop [j 0 i 0]
      (when (< j n)
        (if (pos? (aget acc j))
          (do (aset docs i (if candidates (aget candidates j) (int j)))
              (aset scores i (aget acc j))
              (recur (inc j) (inc i)))
          (recur (inc j) i))))
    {:docs docs :scores scores}))

(defn ^:no-doc joined
  "The hits `x` and `y` as one, where `op`, :+ or :max, combines the scores
  of a document in both."
  [op x y]
  (let [{^ints xd :docs ^doubles xs :scores} x
        {^ints yd :docs ^doubles ys :scores} y
        nx               (alength xd)
        ny               (alength yd)
        ^ints docs       (segment/make-ints (+ nx ny))
        ^doubles scores  (segment/make-doubles (+ nx ny))
        n                (loop [i 0 j 0 n 0]
                           (cond
                             (and (= i nx) (= j ny))
                             n

                             (or (= j ny)
                                 (and (< i nx)
                                      (< (aget xd i) (aget yd j))))
                             (do (aset docs n (aget xd i))
                                 (aset scores n (aget xs i))
                                 (recur (inc i) j (inc n)))

                             (or (= i nx)
                                 (> (aget xd i) (aget yd j)))
                             (do (aset docs n (aget yd j))
                                 (aset scores n (aget ys j))
                                 (recur i (inc j) (inc n)))

                             :else
                             (let [sx (aget xs i)
                                   sy (aget ys j)]
                               (aset docs n (aget xd i))
                               (aset scores n (if (= :max op)
                                                (max sx sy)
                                                (+ sx sy)))
                               (recur (inc i) (inc j) (inc n)))))]
    {:docs   (segment/slice docs 0 n)
     :scores (segment/trimmed-doubles scores n)}))

(defn ^:no-doc added-up
  "The hits `more`, whose documents are all among those of `hits`, with
  the scores of `hits` added to theirs."
  [hits more]
  (let [{^ints hd :docs ^doubles hs :scores} hits
        {^ints md :docs ^doubles ms :scores} more
        n                (alength md)
        ^doubles scores  (segment/make-doubles n)]
    (loop [i 0 j 0]
      (when (< i n)
        (let [j (segment/seek hd j (alength hd) (aget md i))]
          (aset scores i (+ (aget hs j) (aget ms i)))
          (recur (inc i) (inc j)))))
    {:docs md :scores scores}))

(defn ^:no-doc without
  "The `hits` without the documents of the hits `other`."
  [hits other]
  (let [{^ints hd :docs ^doubles hs :scores} hits
        ^ints od         (:docs other)
        n                (alength hd)
        m                (alength od)
        ^ints docs       (segment/make-ints n)
        ^doubles scores  (segment/make-doubles n)
        kept             (loop [i 0 j 0 kept 0]
                           (if (< i n)
                             (let [d (aget hd i)
                                   j (segment/seek od j m d)]
                               (if (and (< j m) (= d (aget od j)))
                                 (recur (inc i) j kept)
                                 (do (aset docs kept d)
                                     (aset scores kept (aget hs i))
                                     (recur (inc i) j (inc kept)))))
                             kept))]
    {:docs   (segment/slice docs 0 kept)
     :scores (segment/trimmed-doubles scores kept)}))

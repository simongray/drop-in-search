(ns dk.simongray.drop-in-search.scoring
  "BM25F's scores of the documents of a segment that hold a term, from the
  weights and lengths of their fields.

  The comments name the source of each part, and where it departs from it:

  - Robertson and Zaragoza, The Probabilistic Relevance Framework: BM25
    and Beyond, Foundations and Trends in Information Retrieval 3(4),
    2009, https://www.staff.city.ac.uk/~sbrp622/papers/foundations_bm25_review.pdf
  - Lucene's BM25Similarity, https://lucene.apache.org/core/9_11_1/core/"
  (:require [dk.simongray.drop-in-search.hits :as hits]
            [dk.simongray.drop-in-search.postings :as postings
             #?@(:cljs [:refer [Cursor]])]
            [dk.simongray.drop-in-search.segment :as segment
             #?@(:cljs [:refer [Segment]])])
  #?(:clj (:import [dk.simongray.drop_in_search.postings Cursor]
                   [dk.simongray.drop_in_search.segment Segment])))

;; k1, b and idf are those of BM25Similarity of Lucene
(def ^{:no-doc true :const true} k1
  "BM25's saturation: how soon repeating a term stops counting."
  1.2)

(def ^{:no-doc true :const true} b
  "BM25's length normalization: how much a long field is discounted."
  0.75)

(defn ^:no-doc idf
  "The inverse document frequency of a term found in `n` of `total`
  documents."
  ^double [^long total ^long n]
  (Math/log (+ 1 (/ (+ (- total n) 0.5) (+ n 0.5)))))

;; The constants of BM25F of the fields of an index, as arrays by field
;; number: the weight of each, and a and c of its length normalization
;; a + c length
(deftype Weights [weight a c])

;; Until a merge leaves them out, removed documents still count in the
;; statistics of BM25, as in Lucene
(defn ^:no-doc weights
  "The constants of BM25F of the fields of an index with the field
  `numbers` and the `segments`: each field's weight from `boosts`, 1 by
  default, and a and c of its length normalization, which fold in `b` and
  the field's average length."
  [numbers segments boosts b]
  (let [n               (count numbers)
        b               (double b)
        ^doubles weight (segment/make-doubles n)
        ^doubles a      (segment/make-doubles n)
        ^doubles c      (segment/make-doubles n)]
    (doseq [[field no] numbers]
      (let [[docs length] (->> segments
                               (keep #(get (.-field-stats ^Segment %) no))
                               (apply map + [0 0]))
            average       (if (pos? docs)
                            (/ (double length) docs)
                            1.0)]
        (aset weight no (double (get boosts field 1)))
        (aset a no (- 1.0 b))
        (aset c no (/ b average))))
    (->Weights weight a c)))

(defn ^:no-doc restricted
  "The `weights` with every field but the one numbered `no` left out, and
  that one at its weight, or 1 when that is 0, since a query names it.
  Without a `no`, every field is left out."
  [^Weights weights no]
  (let [^doubles weight (.-weight weights)
        ^doubles only   (segment/make-doubles (alength weight))]
    (when no
      (let [w (aget weight (int no))]
        (aset only (int no) (if (zero? w) 1.0 w))))
    (->Weights only (.-a weights) (.-c weights))))

(defn ^:no-doc every-field?
  "Whether the `weights` count every field."
  [^Weights weights]
  (let [^doubles weight (.-weight weights)]
    (every? #(pos? (aget weight (int %))) (range (alength weight)))))

;; BM25F, Robertson and Zaragoza 2009, section 3.6, equation 3.19: each
;; occurrence counts the weight of its field, divided by the field's
;; length normalization
(defn ^:no-doc occurrence
  "What an occurrence of a term in the field numbered `no` of the document
  `d` of `segment` counts, with the `weights` of the fields."
  ^double [^Segment segment ^Weights weights ^long d ^long no]
  (let [^doubles weight (.-weight weights)
        w               (aget weight no)]
    (if (zero? w)
      0.0
      (let [^doubles a     (.-a weights)
            ^doubles c     (.-c weights)
            ^ints lengths  (.-lengths segment)
            length         (aget lengths (+ (* d (.-width segment)) no))]
        (/ w (+ (aget a no) (* (aget c no) length)))))))

;; Each occurrence adds what it counts, one by one in the order of the
;; positions, which a product of the count would round differently
(defn- frequency
  "BM25F's frequency of a term in the document `d` of `segment`, with the
  `weights` of the fields, from the runs of its posting at the cursor
  `c`."
  ^double [segment weights ^long d ^Cursor c]
  (let [runs (.runs c)]
    (loop [r -1 left 0 sum 0.0 counts 0.0]
      (cond
        (pos? left)
        (recur r (dec left) (+ sum counts) counts)

        (< (inc r) runs)
        (let [r (inc r)]
          (recur r (.freq c r) sum (occurrence segment weights d (.field c r))))

        :else
        sum))))

(defn- add-frequencies!
  "Add to `acc`, an array of a value for each of the `candidates`, the
  BM25F frequency of each posting in `segment` of the term numbered `t`
  whose document is one of them, with the `weights`."
  [segment weights ^doubles acc ^ints candidates t]
  (let [^Cursor c (segment/cursor segment t)]
    (segment/each-held! c candidates
                        (fn [j]
                          (aset acc j (+ (aget acc j)
                                         (frequency segment weights
                                                    (aget candidates j) c)))))))

(defn- add-each-frequency!
  "Add to `acc`, an array of a value for each document of `segment`, the
  BM25F frequency of each posting of the term numbered `t`, with the
  `weights`."
  [segment weights ^doubles acc t]
  (let [^Cursor c (segment/cursor segment t)]
    (loop []
      (let [d (.nextDoc c)]
        (when-not (== d postings/no-more)
          (aset acc d (+ (aget acc d) (frequency segment weights d c)))
          (recur))))))

(defn- each-frequency
  "The hits of the documents of `segment` that hold the term numbered `t`
  in a field that the `weights` count, scored by their BM25F
  frequencies."
  [segment weights t]
  (let [^Cursor c        (segment/cursor segment t)
        n                (segment/doc-frequency segment t)
        ^ints docs       (segment/make-ints n)
        ^doubles scores  (segment/make-doubles n)]
    (dotimes [k n]
      (let [d (.nextDoc c)]
        (aset docs k (int d))
        (aset scores k (frequency segment weights d c))))
    (hits/collected scores docs)))

(defn ^:no-doc term-frequencies
  "The hits of the documents of `segment` among the `candidates`, or of
  all of them when nil, that hold the terms numbered `ts` in a field that
  the `weights` count, scored by their BM25F frequencies added up."
  [segment weights ts candidates]
  (cond
    (empty? ts)
    (hits/no-hits)

    candidates
    (let [acc (segment/make-doubles (alength ^ints candidates))]
      (doseq [t ts]
        (add-frequencies! segment weights acc candidates t))
      (hits/collected acc candidates))

    (= 1 (count ts))
    (each-frequency segment weights (first ts))

    :else
    (let [acc (segment/make-doubles (segment/size segment))]
      (doseq [t ts]
        (add-each-frequency! segment weights acc t))
      (hits/collected acc nil))))

(defn ^:no-doc saturated
  "The `hits` of BM25F frequencies as the scores of a term of the inverse
  document frequency `idf`, with the saturation `k1`."
  [{:keys [docs ^doubles scores] :as hits} idf k1]
  (let [idf          (double idf)
        k1           (double k1)
        n            (alength scores)
        ^doubles out (segment/make-doubles n)]
    (dotimes [i n]
      (let [f (aget scores i)]
        (aset out i (* idf (/ f (+ k1 f))))))
    {:docs docs :scores out}))

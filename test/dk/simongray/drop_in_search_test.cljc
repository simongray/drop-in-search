(ns dk.simongray.drop-in-search-test
  "The full-text index, the ranking of its queries by BM25F, and their
  options for a text."
  (:require #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [clojure.test :refer [deftest is testing]]
            [dk.simongray.drop-in-search :as search]
            [dk.simongray.drop-in-search.ciff :as ciff]
            [dk.simongray.drop-in-search.queries :as queries]))

(def docs
  [{:id     1
    :stored {:title "Clojure…"}
    :fields {:title       "Clojure and the value of simplicity"
             :description "A talk about data."}}
   {:id     2
    :fields {:title       "Rust"
             :description (str "Systems programming, and a little Clojure "
                               "for the value for value era.")}}
   {:id     3
    :fields {:title "Gardening" :description "Tomatoes."}}])

(def idx
  (search/index docs {:boosts {:title 3}}))

(defn- ids
  ([q]
   (ids q {}))
  ([q opts]
   (mapv :id (search/query idx q opts))))

(deftest the-index
  (testing "every word must occur; a word in the title weighs more"
    (is (= [1 2] (ids "clojure")))
    (is (= [1 2] (ids "clojure value")))
    (is (= [] (ids "clojure tomatoes")))
    (is (= #{1 2 3} (set (ids "clojure tomatoes" {:operator :or}))))
    (is (= [] (ids "")))
    (is (= [] (ids "   "))))
  (testing "the last word as a prefix, unless the query ends in a space"
    (is (= [1 2] (ids "cloj")))
    (is (= [] (ids "cloj ")))
    (is (= [] (ids "cloj" {:prefix? false})))
    (is (= [2] (ids "systems cloj")) "only the last word")
    (is (= [] (ids "clo" {:min-prefix 4})) "too short a word to complete")
    (is (= [1 2] (ids "cloj" {:min-prefix 4}))))
  (testing "words in quotes must occur in a row"
    (is (= [2] (ids "\"value for value\"")))
    (is (= [1 2] (ids "value")))
    (is (= [1] (ids "\"value of\"")))
    (is (= [2] (ids "value-for-value")) "a word with punctuation in it")
    (is (= [2] (ids "value-for-va")) "the last of its words as a prefix"))
  (is (= #{1 2 3} (set (search/ids idx))) "the ids of its documents")
  (testing "what is stored comes back; limit and filter apply"
    (is (= {:title "Clojure…"} (:stored (first (search/query idx "clojure")))))
    (is (nil? (:stored (second (search/query idx "clojure")))))
    (is (= 1 (count (search/query idx "clojure" {:limit 1}))))
    (is (= [2] (ids "clojure" {:filter #(= 2 (:id %))}))))
  (testing "the ranking is an option"
    (is (= [2] (ids "clojure" {:boosts {:title 0 :description 1}}))
        "a field boosted by zero does not match")
    (is (= [2] (ids "clojure" {:fields #{:description}})) "only the fields named")
    (is (= [1 2] (ids "clojure" {:fields #{:title :description}})))
    (is (= [2 1] (ids "clojure" {:rank (fn [r] (- (:id r)))}))
        "a rank function orders the results instead")
    (is (= (ids "clojure") (ids "clojure" {:k1 1.2 :b 0.75})) "the defaults")
    (is (not= (:score (first (search/query idx "clojure")))
              (:score (first (search/query idx "clojure" {:k1 2.0 :b 0.5})))))
    (is (= (take 1 (ids "clojure")) (ids "clojure" {:limit 1}))
        "a limit keeps the head of the full order"))
  (testing "documents replaced and removed"
    (let [idx (search/add idx {:id 1 :fields {:title "Haskell"}})]
      (is (= [2] (mapv :id (search/query idx "clojure"))))
      (is (= [1] (mapv :id (search/query idx "haskell"))))
      (is (= 3 (count (:docs idx))))
      (let [idx (search/remove idx 1)]
        (is (= 2 (count (:docs idx))))
        (is (= [] (search/query idx "haskell")))
        (is (= [2] (mapv :id (search/query idx "clojure")))))
      (is (= idx (search/remove idx :nobody)))))
  (testing "fields and queries given as terms are taken as is"
    (let [idx (search/index [{:id :a :fields {:body ["kaffe" "te"]}}])]
      (is (= [:a] (mapv :id (search/query idx ["kaffe"]))))
      (is (= [:a] (mapv :id (search/query idx "Kaffe"))))
      (is (= [] (search/query idx ["Kaffe"])) "no analysis, so no folding")))
  (testing "but the strings of a set or a list are analyzed each"
    (let [idx (search/index [{:id :a :fields {:keywords #{"Café" "Politics"}}}])]
      (is (= [:a] (mapv :id (search/query idx "cafe"))))))
  (testing "an index printed as EDN and read back"
    (let [thawed (edn/read-string (pr-str idx))]
      (is (= [1 2] (mapv :id (search/query thawed "cloj")))
          "its arrays come back as vectors, which a query makes arrays")
      (is (= idx (search/restore thawed)))
      (is (= [1 2] (mapv :id (search/query (search/restore thawed) "cloj"))))))
  (testing "every kind of white space is one, on both platforms"
    (is (= [2] (ids "value\u00A0for\u3000value"))
        "no-break and ideographic spaces"))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (search/add idx {:fields {:title "no id"}}))))

(deftest an-index-that-changes
  (let [doc     (fn [i]
                  {:id     i
                   :fields {:title (str "episode " i (if (even? i)
                                                       " clojure"
                                                       " rust"))}})
        kept    (remove #(zero? (mod % 3)) (range 300))
        changed (as-> (search/index) idx
                  (reduce #(search/add %1 (doc %2)) idx (range 300))
                  (reduce search/remove idx (range 0 300 3)))
        ids     #(set (map :id (search/query % "clojure ")))]
    (is (= (ids (search/index (map doc kept))) (ids changed))
        "one document at a time, as a new index of the same documents")
    (is (= (count kept) (count (:docs changed))))
    (is (= changed (search/restore (edn/read-string (pr-str changed))))
        "printed and read back with its removed documents")
    (is (contains? (ids (search/add changed (doc 6))) 6)
        "a removed document added again")))

(defn- error-type
  "The :type of what `f` throws, or nil."
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Exception :cljs :default) e
      (:type (ex-data e)))))

(defn- bytes-of
  [xs]
  #?(:clj  (byte-array (map unchecked-byte xs))
     :cljs (js/Uint8Array. (into-array xs))))

(defn- cut
  "The bytes `body` without their last `n`."
  [body n]
  #?(:clj  (java.util.Arrays/copyOf ^bytes body (- (alength ^bytes body) n))
     :cljs (.slice body 0 (- (.-length body) n))))

(deftest an-index-in-a-store
  (let [store (atom {})
        save! (fn [idx]
                (let [{:keys [put delete]} (ciff/plan idx (keys @store))]
                  (doseq [{:keys [name body]} put]
                    (swap! store assoc name body))
                  (swap! store #(apply dissoc % delete))
                  put))
        read  #(ciff/read-documents (for [[name body] @store]
                                        {:name name :body body}))]
    (is (= ["index.edn"] (mapv :name (take-last 1 (save! idx))))
        "the manifest last")
    (is (= idx (read)) "read back as it was")
    (is (= idx (ciff/read-documents
                (for [[name body] @store]
                  {:name name
                   :body (if (string? body)
                           #?(:clj  (.getBytes ^String body "UTF-8")
                              :cljs (.encode (js/TextEncoder.) body))
                           body)})))
        "with the manifest as bytes too, as a folder gives it")
    (is (= [1 2] (mapv :id (search/query (read) "cloj"))))
    (is (= (map :name (:put (ciff/plan idx)))
           (->> (search/index docs {:boosts {:title 3}})
                ciff/plan
                :put
                (map :name)))
        "a segment is named after what it holds")
    (let [changed (search/add idx {:id 4 :fields {:title "Clojure again"}})]
      (is (= 2 (count (save! changed))) "only the new segment and the manifest")
      (is (= changed (read)))
      (is (= (search/query changed "clojure")
             (search/query (read) "clojure"))))
    (let [removed (search/remove idx 2)]
      (is (= 1 (count (save! removed))) "a removal changes the manifest only")
      (is (= [1] (mapv :id (search/query (read) "clojure")))))
    (let [anew (search/index (take 2 docs))]
      (save! anew)
      (is (= 2 (count @store)) "the segments of the old index are deleted")
      (is (= anew (read))))
    (testing "documents that don't read"
      (let [manifest    {:name "index.edn" :body (get @store "index.edn")}
            [name body] (first (dissoc @store "index.edn"))
            read-of     #(error-type (fn [] (ciff/read-documents %)))]
        (is (= ::ciff/malformed (read-of [])) "no manifest")
        (is (= ::ciff/malformed (read-of [manifest])) "a segment missing")
        (is (= ::ciff/malformed
               (read-of [manifest {:name name :body (cut body 10)}]))
            "a segment cut short")
        (is (= ::ciff/malformed
               (read-of [manifest
                         {:name name
                          :body (bytes-of [255 255 255 255 15 8 1])}]))
            "a size far beyond the end, which allocates nothing")
        (is (= ::ciff/malformed
               (read-of [manifest {:name name :body "text"}])))
        (is (= ::ciff/malformed (read-of [{:name "index.edn" :body "{"}])))
        (is (= ::ciff/newer-format
               (read-of [{:name "index.edn" :body "{:version 99}"}])))))))

(deftest operators
  (is (= [3 1 2] (ids "clojure | tomatoes")))
  (is (= [1] (ids "clojure -rust")))
  (is (= [1] (ids "clojure NOT rust")))
  (is (= [1] (ids "clojure !\"value for value\"")))
  (is (= [2] (ids "(rust | gardening) -tomatoes")))
  (is (= [] (ids "-clojure")) "only exclusions find nothing")
  (is (= [1] (ids "title:clojure")))
  (is (= [2] (ids "description:clojure")))
  (is (= #{2 3} (set (ids "title:(rust | gardening)"))))
  (is (= [2] (ids "description:clojure" {:boosts {:description 0}}))
      "a field that a query names counts although its boost is 0")
  (let [idx (search/index docs {:aliases {"notes" :description}})]
    (is (= [2] (mapv :id (search/query idx "notes:clojure"))))))

(deftest an-index-in-an-app
  (let [idx (search/index docs {:aliases {"notes" :description}
                                :syntax  {:and #{"OG"}}})]
    (testing "with query-opts, search.queries reads a query as the index does"
      (is (= {:and [{:term "clojure" :field :description} {:term "rust"}]}
             (queries/parse "notes:clojure OG rust " (search/query-opts idx))))
      (is (= {:and [{:term "clojure" :field :title} {:term "rust"}]}
             (queries/parse "title:clojure OG rust " (search/query-opts idx)))
          "a field by its own name")
      (is (= [{:text "a "} {:text "Clojure" :match? true}]
             (queries/snippet "a Clojure"
                            "notes:clojure"
                            (search/query-opts idx {:width 20})))))
    (testing "a field holds any value, whose text counts"
      (let [idx (search/index [{:id 1 :fields {:year 2024 :tags #{:clojure :data}}}])]
        (is (= [1] (mapv :id (search/query idx "2024 "))))
        (is (= [1] (mapv :id (search/query idx "tags:clojure "))))))
    (testing "the next page"
      (let [idx  (search/index (for [i (range 25)]
                                 {:id i :fields {:title "same words"}}))
            page #(mapv :id (search/query idx "same" {:limit 10 :offset %}))]
        (is (= (range 10 20) (page 10)))
        (is (= (range 20 25) (page 20)))
        (is (= [] (page 30)))
        (is (= (range 5 25) (mapv :id (search/query idx "same" {:offset 5}))))))))

(deftest bm25f
  (let [idx   (search/index [{:id 1 :fields {:a "x y" :b "x y" :c "x y"}}
                             {:id 2 :fields {:a "x x" :b "y y" :c "x y"}}
                             {:id 3 :fields {:a "z z" :b "z z" :c "z z"}}])
        score (fn [q id]
                (some #(when (= id (:id %)) (:score %)) (search/query idx q)))]
    (is (< (Math/abs (- (score "x" 1) (score "x" 2))) 1e-9)
        "a word once in three fields counts as much as three times in two"))
  (let [idx   (search/index (concat (for [i (range 1 6)]
                                      {:id i :fields {:title "clojure"}})
                                    [{:id 6 :fields {:title "clojurians"}}]))
        score (fn [id]
                (some #(when (= id (:id %)) (:score %))
                      (search/query idx "cloj")))]
    (is (= (score 1) (score 6))
        "a prefix counts as one term, so the rarest completion doesn't win")))

(deftest typos
  (let [idx (search/index [{:id 1 :fields {:title "Clojure and the value of simplicity"}}
                           {:id 2 :fields {:title "Closure tables"}}
                           {:id 3 :fields {:title "Rust"}}])
        ids (fn [q & [opts]] (mapv :id (search/query idx q (or opts {}))))]
    (is (= #{1 2} (set (ids "clojre "))) "a word that no document holds")
    (is (= [3] (ids "rust ")) "but not one that a document holds")
    (is (= #{1 2 3} (set (ids "rust | clojre "))) "in any part of a query")
    (is (= #{1 2} (set (ids "clojrue"))) "or the word being typed")
    (is (= [] (ids "clojre " {:fuzzy false})))
    (is (= [1] (ids "clojre~1 " {:fuzzy false})) "but ~ still asks for typos")
    (is (= #{1 2} (set (ids "clojre " {:fuzzy true}))))
    (is (= [1] (ids "clojre " {:typo-lengths [3 7]}))
        "the lengths of words with one edit and with two are an option")
    (is (= [] (ids "clojre~ " {:typo-lengths [7 9]})))
    (is (= [1 2] (ids "clojure~ ")) "an exact match outranks one with typos")
    (is (= [1] (ids "clojure~0 ")))
    (is (= [1] (ids "clojre~1 ")) "one edit reaches clojure, two closure")
    (is (= [] (ids "ru~ ")) "no typos in a word of two letters")
    (is (= [3] (ids "rsut~ ")) "a swap of neighbours is one edit")
    (is (= #{1 2} (set (ids "clojre" {:fuzzy true})))
        "the word being typed matches with typos too")
    (is (= [1] (ids "cloj" {:fuzzy true})) "and what it starts")
    (is (= [] (ids "clojre -clojure~1" {:fuzzy 2})) "a :not with typos")
    (testing "which words matched with typos, and in which results"
      (is (= {:typos {"clojre" ["clojure" "closure"]}}
             (meta (search/query idx "clojre "))))
      (is (nil? (meta (search/query idx "rust "))) "none")
      (let [found (search/query idx "clojure~ ")]
        (is (= {:typos {"clojure" ["closure"]}} (meta found)))
        (is (= [nil {"clojure" ["closure"]}] (map :typos found))
            "a result that holds the word itself has none")))
    (is (= [{:text "Closure" :match? true} {:text " tables"}]
           (queries/snippet "Closure tables" "clojre " (search/query-opts idx)))
        "a snippet knows the words of the index")
    (is (queries/matches? "Closure tables" "clojre " (search/query-opts idx)))
    (is (not (queries/matches? "Closure tables"
                             "clojre "
                             (search/query-opts idx {:fuzzy false}))))))

(deftest hostile-queries
  (testing "a prefix stands for its completions that the most documents hold"
    (let [idx (search/index (concat (for [i (range 5)]
                                      {:id i :fields {:title "pxcommon"}})
                                    (for [i (range 5 50)]
                                      {:id i :fields {:title (str "px" i)}})))]
      (is (= 50 (count (search/query idx "px"))))
      (is (= (range 5) (map :id (search/query idx "px" {:max-completions 1}))))
      (is (= 1 (count (search/query idx "px1~ " {:max-expansions 1}))))))
  (testing "the limits are data"
    (is (= #{:max-completions :max-expansions}
           (set (keys search/default-limits))))
    (is (= [1 2] (ids "clojure tomatoes" {:max-terms 1}))
        "and those of parse count in a query too")))

(deftest scripts-without-spaces
  (let [idx (search/index [{:id 1 :fields {:title "[HSK 3] 爸爸不在家的中秋节"}}
                           {:id 2 :fields {:title "八百八的月饼礼盒值不值"}}
                           {:id 3 :fields {:title "第13回 本番データ消去、AIメモリ管理 ほか"}}
                           {:id 4 :fields {:title "今天天气很好 good"}}])
        ids (fn [q] (mapv :id (search/query idx q)))]
    (is (= [1] (ids "中秋")) "a word inside a sentence")
    (is (= [2] (ids "月饼")))
    (is (= [1] (ids "中秋节")) "the pairs of a longer word in a row")
    (is (= [3] (ids "データ")))
    (is (= [3] (ids "メモリ 管理")))
    (is (= [4] (ids "好")) "one character, the end of a run")
    (is (= [1] (ids "爸")) "one character, completed as a prefix")
    (is (= [4] (ids "\"天气很好 good\"")) "a phrase across a run and a word")
    (is (= [] (ids "\"很好天气\"")))))

(deftest limits-and-ties
  (let [idx (search/index (for [i (range 200)]
                            {:id i :fields {:title "same words"}}))]
    (is (= (range 10) (mapv :id (search/query idx "same" {:limit 10})))
        "equal scores in the order of numeric ids")
    (is (= 200 (count (search/query idx "same"
                                    {:limit #?(:clj  Long/MAX_VALUE
                                               :cljs js/Number.MAX_SAFE_INTEGER)}))))
    (is (= (range 150) (mapv :id (search/query idx "same" {:limit 150})))
        "a limit near the count sorts all")))

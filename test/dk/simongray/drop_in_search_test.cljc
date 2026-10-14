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

(defn- close?
  [x y]
  (< (Math/abs (- x y)) 1e-9))

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
    (is (= [2] (ids "clojure" {:filter-pred #(= 2 (:id %))}))))
  (testing "the ranking is an option"
    (is (= [2] (ids "clojure" {:boosts {:title 0 :description 1}}))
        "a field boosted by zero does not match")
    (is (= [2] (ids "clojure" {:fields #{:description}})) "only the fields named")
    (is (= [1 2] (ids "clojure" {:fields #{:title :description}})))
    (is (= [2 1] (ids "clojure" {:rank-fn (fn [r] (- (:id r)))}))
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
      (is (= idx (search/remove idx :nobody))))
    (is (= #{3} (set (search/ids (search/remove idx 1 2)))) "several at once")
    (is (= idx (search/add idx) (search/remove idx)) "none")
    (let [idx (search/index [{:id 1 :fields {:title "old"}}
                             {:id 1 :fields {:title "new"}}])]
      (is (= [1] (mapv :id (search/query idx "new "))))
      (is (= [] (search/query idx "old ")) "the last of one id in a call")))
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
          "its arrays come back as vectors and text, which a query reads")
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (search/restore
                    (update thawed :segments
                            (partial mapv #(dissoc % :postings)))))
          "but not those of an earlier version, without postings")
      (is (= idx (search/restore thawed)))
      (is (= (hash idx) (hash (search/restore thawed))) "and hashes the same")
      (is (= [1 2] (mapv :id (search/query (search/restore thawed) "cloj"))))
      (let [changed (search/add thawed {:id 4 :fields {:title "Haskell"}})]
        (is (= [4] (mapv :id (search/query changed "haskell")))
            "a change to it needs no restore"))))
  (testing "every kind of white space is one, on both platforms"
    (is (= [2] (ids "value\u00A0for\u3000value"))
        "no-break and ideographic spaces"))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (search/add idx {:fields {:title "no id"}})))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (search/index [{:id 1 :fields (zipmap (range 2148) (repeat "x"))}]))
      "more than 2147 fields, whose positions don't fit in an int")
  (is (= [] (search/query (search/index) "clojure")) "an empty index"))

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

(deftest segments
  (let [doc (fn [i] {:id i :fields {:title (str "word w" i)}})
        idx (search/index (map doc (range 10)))]
    (is (= (search/index (map doc (range 6 10)))
           (search/remove idx 0 1 2 3 4 5))
        "a segment of more removed documents than others is written again")
    (is (= idx (search/remove (search/add idx (doc 10)) 10))
        "and one that holds no others is left out")
    (let [big (search/index (map doc (range 4100)))]
      (is (= 1 (count (:segments big)))
          "the segments of more than 4096 documents merged into one")
      (is (= [0 4099] (sort (map :id (search/query big "w0 | w4099 "))))))))

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

(defn- size
  [body]
  #?(:clj  (alength ^bytes body)
     :cljs (.-length body)))

(defn- flipped
  "The bytes `body` with the bits of the one at `i` flipped."
  [body i]
  #?(:clj  (doto (aclone ^bytes body)
             (aset-byte i (unchecked-byte (bit-xor 0xFF (aget ^bytes body i)))))
     :cljs (doto (.slice body)
             (aset i (bit-xor 0xFF (aget body i))))))

(defn- read-outcome
  "What reading the `files` gives: :read for an index that answers a query,
  or else the :type of what is thrown, which is nil for an error of the
  platform."
  [files]
  (try
    (search/query (ciff/read-files files) "clojure value~ | \"a talk\"")
    :read
    (catch #?(:clj Exception :cljs :default) e
      (:type (ex-data e)))))

(deftest an-index-in-a-store
  (let [store (atom {})
        save! (fn [idx]
                (let [{:keys [put delete]} (ciff/plan idx (keys @store))]
                  (doseq [{:keys [name body]} put]
                    (swap! store assoc name body))
                  (swap! store #(apply dissoc % delete))
                  put))
        read  #(ciff/read-files (for [[name body] @store]
                                    {:name name :body body}))]
    (is (= ["index.edn"] (mapv :name (take-last 1 (save! idx))))
        "the manifest last")
    (is (= idx (read)) "read back as it was")
    (is (= idx (ciff/read-files
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
    (testing "files that don't read"
      (let [manifest    {:name "index.edn" :body (get @store "index.edn")}
            [name body] (first (dissoc @store "index.edn"))
            read-of     #(error-type (fn [] (ciff/read-files %)))]
        (is (= ::ciff/malformed (read-of [])) "no manifest")
        (is (= ::ciff/malformed (read-of [manifest])) "a segment missing")
        (is (= ::ciff/malformed
               (read-of [manifest {:name name :body (cut body 10)}]))
            "a segment cut short")
        (is (every? #(= ::ciff/malformed
                        (read-of [manifest {:name name :body (cut body %)}]))
                    (range 1 (inc (size body))))
            "anywhere")
        (is (every? #{:read ::ciff/malformed ::ciff/newer-format}
                    (for [i (range (size body))]
                      (read-outcome [manifest
                                     {:name name :body (flipped body i)}])))
            "a wrong byte anywhere, which still reads or throws one of these")
        (let [edited #(read-of [(update manifest :body
                                        (comp pr-str % edn/read-string))
                                {:name name :body body}])]
          (is (= ::ciff/malformed
                 (edited #(update % :segments into (:segments %))))
              "a segment listed twice")
          (is (= ::ciff/malformed (edited #(assoc % :fields {})))
              "a segment of fields that the index doesn't have")
          (is (= ::ciff/malformed
                 (edited #(assoc-in % [:segments 0 :deleted] #{99})))
              "a removed document that the segment doesn't have"))
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
  (is (= [] (ids "-clojure -rust")))
  (is (= [] (ids "-clojure | -rust")) "in any kind of group")
  (is (= [1 2] (ids "clojure (value | tomatoes)")) "a group among words")
  (is (= [1] (ids "data (clojure -rust)")))
  (is (= [] (ids "clojure tomatoes value -rust")) "nothing left to narrow")
  (is (= #{2 3} (set (ids {:or [{:term "rust"} {:term "tomatoes"}]})))
      "a query as data")
  (is (= [1] (ids "title:clojure")))
  (is (= [2] (ids "description:clojure")))
  (is (= #{2 3} (set (ids "title:(rust | gardening)"))))
  (is (= [1] (ids "title:\"value of\"")))
  (is (= [] (ids "description:\"value of\"")) "a phrase in a field")
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
    (testing "and snippet and matches? take the index itself"
      (is (= [{:text "a "} {:text "Clojure" :match? true}]
             (search/snippet idx "a Clojure" "notes:clojure")))
      (is (= [{:text "a Clojure"}]
             (search/snippet idx "a Clojure" "notes:clojure" {:field :title}))
          "but mark only the words for the field of the text")
      (is (= [{:text "a "} {:text "Clojure" :match? true}]
             (search/snippet idx "a Clojure" "notes:clojure" {:field :description})))
      (is (search/matches? idx "a Clojure" "notes:clojure"))
      (is (not (search/matches? idx "a Clojure" "notes:clojure" {:field :title})))
      (is (search/matches? idx "a Clojure" "clojure" {:field :title})
          "a word for no field in particular"))
    (testing "the size, and a document by its id"
      (is (= 3 (search/size idx)))
      (is (= {:id 1 :stored {:title "Clojure…"}} (search/document idx 1)))
      (is (= {:id 2 :stored nil} (search/document idx 2)) "one with nothing stored")
      (is (nil? (search/document idx 4)))
      (is (= {:id 1 :stored {:title "Clojure…"}}
             (search/document (edn/read-string (pr-str idx)) 1))
          "of an index read back from EDN")
      (is (= 0 (search/size (search/index))))
      (is (= () (search/ids (search/index))) "no ids, rather than nil"))
    (testing "a field holds any value, whose text counts"
      (let [idx (search/index [{:id 1 :fields {:year 2024 :tags #{:clojure :data}}}])]
        (is (= [1] (mapv :id (search/query idx "2024 "))))
        (is (= [1] (mapv :id (search/query idx "tags:clojure ")))))
      (let [idx (search/index [{:id 1 :fields {"title" "Clojure"}}])]
        (is (= [1] (mapv :id (search/query idx "title:clojure ")))
            "in a field named by a string")))
    (testing "the next page"
      (let [idx  (search/index (for [i (range 25)]
                                 {:id i :fields {:title "same words"}}))
            page #(mapv :id (search/query idx "same" {:limit 10 :offset %}))]
        (is (= (range 10 20) (page 10)))
        (is (= (range 20 25) (page 20)))
        (is (= [] (page 30)))
        (is (= (range 5 25) (mapv :id (search/query idx "same" {:offset 5}))))))))

(deftest unknown-options
  (let [bad? #(= :dk.simongray.drop-in-search/bad-options
                 (try (%) nil
                      (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
                        (:type (ex-data e)))))]
    (is (bad? #(search/query idx "clojure" {:limt 10})) "a typo")
    (is (bad? #(search/index docs {:boost {:title 3}})))
    (is (bad? #(search/snippet idx "a Clojure" "clojure" {:feild :title})))
    (is (bad? #(queries/parse "clojure" {:operater :or})))
    (is (bad? #(ciff/read-files [] {:term-f identity})))
    (is (bad? #(search/query idx "clojure" nil)) "nor anything but a map")
    (is (= [1 2] (mapv :id (search/query idx "clojure" (search/query-opts idx {:limit 5}))))
        "but every option of one function goes to another")))

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
        "a prefix counts as one term, so the rarest completion doesn't win"))
  (let [idx   (search/index [{:id 1 :fields {:title "clojure value"}}
                             {:id 2 :fields {:title "clojure closure"}}
                             {:id 3 :fields {:title "clojure lisp"}}])
        score (fn [q id]
                (some #(when (= id (:id %)) (:score %)) (search/query idx q)))]
    (is (close? (+ (score "clojure " 1) (score "value " 1))
                (score "clojure | value " 1))
        "the scores of the parts of an :or add up")
    (is (close? (score "clojure~ " 3) (score "clojure~ " 2))
        "a word with typos counts its best match in a document, not each"))
  (let [idx   (search/index [{:id 1 :fields {:title "x"}}
                             {:id 2 :fields {:title "x y z"}}
                             {:id 3 :fields {:title "y"}}
                             {:id 4 :fields {:title "y"}}])
        found #(mapv :id (search/query idx %1 %2))
        score (fn [q id opts]
                (some #(when (= id (:id %)) (:score %))
                      (search/query idx q opts)))]
    (is (= [1 2] (found "x " {})) "a match in a shorter field counts more")
    (is (close? (score "x " 1 {:b 0}) (score "x " 2 {:b 0}))
        "unless :b is 0")
    (is (> (score "x | y " 1 {}) (score "x | y " 3 {}))
        "a rarer word counts more")))

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
      (is (= {"clojre" ["clojure" "closure"]}
             (:typos (meta (search/query idx "clojre ")))))
      (is (nil? (:typos (meta (search/query idx "rust ")))) "none")
      (let [found (search/query idx "clojure~ ")]
        (is (= {"clojure" ["closure"]} (:typos (meta found))))
        (is (= [nil {"clojure" ["closure"]}] (map :typos found))
            "a result that holds the word itself has none"))
      (let [idx (search/index [{:id 1 :fields {:title "Clojure, not Closure"}}])]
        (is (= [nil] (map :typos (search/query idx "clojure~ "))))
        (is (= [nil] (map :typos (search/query idx "clojure " {:fuzzy true})))
            "nor one that holds a near word too")))
    (is (= [{:text "Closure" :match? true} {:text " tables"}]
           (queries/snippet "Closure tables" "clojre " (search/query-opts idx)))
        "a snippet knows the words of the index")
    (is (queries/matches? "Closure tables" "clojre " (search/query-opts idx)))
    (is (not (queries/matches? "Closure tables"
                             "clojre "
                             (search/query-opts idx {:fuzzy false}))))))

(defn- singular
  "The term `t` without the s of an English plural, a stemmer just good
  enough for a test."
  [t]
  (if (and (< 3 (count t)) (= \s (last t)))
    (subs t 0 (dec (count t)))
    t))

(deftest a-term-fn
  (let [docs [{:id 1 :fields {:body "A post about running shoes"}}
              {:id 2 :fields {:body "Two posts"}}]
        idx  (search/index docs {:term-fn singular :boosts {:body 2}})
        ids  (fn [idx q & [opts]] (sort (map :id (search/query idx q (or opts {})))))]
    (is (= [1 2] (ids idx "posts ") (ids idx "post ")) "posts finds post, and post finds posts")
    (is (= [1] (ids idx "\"running shoe\" ")) "in a phrase too")
    (is (= [1 2] (ids idx "pos")) "and the word being typed")
    (is (= [{:text "Two "} {:text "posts" :match? true}]
           (queries/snippet "Two posts" "post " (search/query-opts idx)))
        "a snippet marks the words that the terms come from")
    (is (queries/matches? "Two posts" "post " (search/query-opts idx)))
    (is (= [1 2] (ids (search/add idx {:id 3 :fields {:body "Other"}}) "posts "))
        "an index keeps its function when it changes")
    (testing "a function isn't printed, so it's given again to read an index back"
      (let [printed (edn/read-string (pr-str idx))]
        (is (= {:boosts {:body 2}} (:defaults printed)))
        (is (= [1 2] (ids (search/restore printed {:term-fn singular}) "posts ")))
        (is (= [] (ids (search/restore printed) "posts " {:fuzzy false}))
            "without it, the words of a query aren't those of the index"))
      (let [{:keys [put]} (ciff/plan idx)]
        (is (= [1 2] (ids (ciff/read-files put {:term-fn singular}) "posts ")))))))

(deftest the-total-of-a-page
  (let [many  (search/index (for [i (range 25)] {:id i :fields {:title (str "Clojure " i)}}))
        total #(:total (meta (search/query many "clojure " %)))]
    (is (= 25 (total {:limit 10})) "for 1 to 10 of 25")
    (is (= [25 5] ((juxt (comp :total meta) count) (search/query many "clojure " {:limit 10 :offset 20}))))
    (is (= 13 (total {:filter-pred #(even? (:id %))})) "of those that the filter keeps")
    (is (= 2 (total {:limit 5 :filter-pred #(< (:id %) 2)})) "when it had to read them all")
    (is (nil? (total {:limit 5 :filter-pred #(even? (:id %))}))
        "but not when the filter stopped once it filled the page")
    (is (= 13 (total {:limit 5 :filter-pred #(even? (:id %)) :rank-fn :id})))
    (is (= 0 (:total (meta (search/query many "rust ")))))))

(deftest dates
  (let [idx (search/index [{:id 1 :fields {:published #inst "2024-03-01T09:30:00Z"}}
                           {:id 2 :fields {:published #{#inst "2023-12-24T18:00:00Z"}}}])]
    (is (= [1] (mapv :id (search/query idx "2024-03 "))) "a date and time in UTC, the same on every platform")
    (is (= [2] (mapv :id (search/query idx "2023-12-24 "))) "in a collection too")
    (is (queries/matches? #inst "2024-03-01T09:30:00Z" "\"2024-03-01 09:30\" "))))

(deftest hostile-queries
  (testing "a prefix stands for its completions that the most documents hold"
    (let [idx (search/index (concat (for [i (range 5)]
                                      {:id i :fields {:title "pxcommon"}})
                                    (for [i (range 5 50)]
                                      {:id i :fields {:title (str "px" i)}})))]
      (is (= 50 (count (search/query idx "px"))))
      (is (= (range 5) (map :id (search/query idx "px" {:max-completions 1}))))
      (is (= 1 (count (search/query idx "px1~ " {:max-expansions 1}))))))
  (testing "the limits are data, those of parse too"
    (is (= #{:max-completions :max-expansions :max-terms :max-depth}
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
        "a limit near the count sorts all")
    (is (= [] (search/query idx "same" {:limit 0})))
    (is (= (range 100 110)
           (mapv :id (search/query idx "same" {:limit       10
                                              :filter-pred #(<= 100 (:id %))})))
        "a filter reads more of the best when too few of them pass")
    (is (= [4 6 8]
           (mapv :id (search/query idx "same" {:limit       3
                                              :offset      2
                                              :filter-pred #(even? (:id %))}))))
    (is (= [199 198]
           (mapv :id (search/query idx "same" {:limit   2
                                              :rank-fn #(- (:id %))})))))
  (let [idx (search/index (for [id ["b" :a "c" "a" :b 10 2.5]]
                            {:id id :fields {:title "same words"}}))]
    (is (= [2.5 10 :a "b" "c" "a" :b] (mapv :id (search/query idx "same")))
        "numbers first, then other ids by hash, the same on both platforms"))
  (let [idx   (search/index (for [i (range 300)]
                              {:id     (if (even? i) i (str "s" i))
                               :fields {:title "same words"}}))
        found #(mapv :id (search/query idx "same" %))]
    (is (= (take 10 (found {})) (found {:limit 10}))
        "a limit keeps the head of the order of numbers and other ids")))

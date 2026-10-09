(ns dk.simongray.drop-in-search.queries-test
  "The query language, and a query matched against one text."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.drop-in-search.queries :as queries]))

(deftest parsing
  (let [fields {"title" :title "notes" :description}
        parse  #(queries/parse % {:aliases fields})]
    (is (= {:and [{:term "value"} {:term "for"} {:term "va" :prefix? true}]}
           (parse "value for va")))
    (is (= {:and [{:phrase ["value" "for" "value"]} {:not {:term "rerun"}}]}
           (parse "\"value for value\" -rerun ")))
    (is (= {:and [{:term "pod" :prefix? true} {:term "network"}]}
           (parse "pod* network ")))
    (is (= {:or [{:and [{:term "a"} {:term "b"}]} {:term "c"}]}
           (parse "a b OR c ")) "AND binds tighter than OR")
    (is (= {:and [{:term "a"} {:or [{:term "b"} {:term "c"}]}]}
           (parse "a (b | c) ")))
    (is (= {:and [{:term "a"} {:not {:or [{:term "b"} {:term "c"}]}}]}
           (parse "a !(b OR c) ")))
    (is (= {:and [{:term "clojure" :field :title}
                  {:phrase ["x" "y"] :field :description}]}
           (parse "title:clojure notes=\"x y\"")))
    (is (= {:or [{:term "a" :field :title} {:term "b" :field :title}]}
           (parse "Title:(a | b)")) "field names are folded")
    (is (= {:phrase ["re" "invent"]} (parse "re:invent "))
        "a name that is no field is a word")
    (is (= {:and [{:term "part"} {:term "1"} {:term "intro"}]}
           (parse "Part 1 - Intro ")) "a dash on its own is no operator")
    (is (= {:and [{:term "a"} {:not {:phrase ["b" "c"]}}]}
           (parse "a -\"b c\"")) "a minus right before a phrase")
    (is (= {:and [{:term "at"} {:term "t"}]} (parse "AT&T ")))
    (is (= {:phrase ["天气" "气很" "很好"]} (parse "天气很好 "))
        "the pairs of a run of Chinese in a row")
    (is (= {:term "好" :prefix? true} (parse "好 "))
        "one character of Chinese is a prefix")
    (testing "nothing is an error"
      (is (= {:phrase ["value" "for"]} (parse "\"value for")))
      (is (nil? (parse "-")))
      (is (nil? (parse "NOT")))
      (is (= {:term "a"} (parse "a OR ")))
      (is (= {:term "a"} (parse ") a (")))
      (is (= {:term "a"} (parse "AND a ")))
      (is (nil? (parse ""))))
    (testing "the spellings and the operator between words are options"
      (is (= {:and [{:term "x"} {:term "y"}]}
             (queries/parse "x OG y " {:syntax {:and #{"OG" "&"}}})))
      (is (= {:and [{:term "x"} {:term "og"} {:term "y"}]}
             (queries/parse "x OG y ")))
      (is (= {:and [{:term "x"} {:not {:term "y"}}]}
             (queries/parse "x IKKE y " {:syntax {:not #{"IKKE"}}})))
      (is (= {:or [{:term "a"} {:and [{:term "b"} {:term "c"}]}]}
             (queries/parse "a b AND c " {:operator :or}))))))

(deftest typos
  (is (= {:and [{:term "clojre" :fuzzy :auto} {:term "value" :fuzzy 1}
                {:term "rust" :prefix? true}]}
         (queries/parse "clojre~ value~1 rust")))
  (is (= [{:text "Closure" :match? true} {:text " tables and "}
          {:text "Clojure" :match? true}]
         (queries/snippet "Closure tables and Clojure" "clojure~")))
  (is (queries/matches? "Closure tables" "clojre~"))
  (is (queries/matches? "Closure tables" "clojre " {:fuzzy true}))
  (is (queries/matches? "Closure tables" "clojre " {:known-fn (constantly false)})
      "a word that :known-fn doesn't know")
  (is (not (queries/matches? "Closure tables" "clojre ")) "without :known-fn")
  (is (= [nil 1 1 3 2] (map #(queries/distance %1 %2 3)
                           ["abc" "clojure" "clojure" "kitten" ""]
                           ["xyzwv" "clojre" "cljoure" "sitting" "ab"]))))

(deftest hostile-queries
  (testing "deep groups and long runs of NOT don't run out the stack"
    (is (= {:term "x"} (queries/parse (str (apply str (repeat 20000 "(")) "x "))))
    (is (= {:term "x"} (queries/parse (str (apply str (repeat 20000 "NOT ")) "x ")))
        "two NOT undo each other")
    (is (= {:not {:term "x"}}
           (queries/parse (str (apply str (repeat 20001 "NOT ")) "x ")))))
  (testing "only the first words of a long query count"
    (is (= 32 (count (:and (queries/parse (apply str (repeat 1000 "word ")))))))
    (is (= {:and [{:term "a"} {:term "b"}]}
           (queries/parse "a b c d" {:max-terms 2}))))
  (testing "the limits are data"
    (is (= #{:max-terms :max-depth} (set (keys queries/default-limits))))))

(deftest one-text
  (let [text "A little Clojure for the value for value era"]
    (is (queries/matches? text "clojure value"))
    (is (queries/matches? text "\"value for value\" -rust"))
    (is (queries/matches? text "rust | clojure"))
    (is (queries/matches? text "clo") "the last word as a prefix")
    (is (not (queries/matches? text "clo ")))
    (is (not (queries/matches? text "clojure -little")))
    (is (not (queries/matches? text "\"value for era\"")))
    (is (not (queries/matches? text "-rust")) "a :not alone holds for nothing")
    (is (queries/matches? text {:or [{:term "rust"} {:term "era"}]}))
    (is (queries/matches? text ["little" "era"]) "a vector of terms")
    (is (queries/matches? "今天天气很好" "天气"))))

(deftest snippets
  (let [text "The quick brown fox jumps over the lazy dog, and the fox is quick."]
    (is (= [{:text "The quick brown "} {:text "fox" :match? true}
            {:text " jumps over the lazy dog, and the "} {:text "fox" :match? true}
            {:text " is quick."}]
           (queries/snippet text "fox")))
    (is (= [{:text "…"} {:text "over the "} {:text "lazy" :match? true}
            {:text " dog, and the fox"} {:text "…"}]
           (queries/snippet text "lazy" {:width 30}))
        "a window around the match, cut at word boundaries")
    (is (= (queries/snippet text "fox") (queries/snippet text "fo"))
        "the last word as a prefix")
    (is (= [{:text text}] (queries/snippet text "cat"))
        "no match, the start of the text")
    (is (= [{:text "The quick brown fox jumps over the "} {:text "lazy" :match? true}
            {:text " dog, and the fox is quick."}]
           (queries/snippet text "lazy -fox"))
        "a word under a :not is no match"))
  (is (= [{:text "Café"}] (queries/snippet "Café" "tea")))
  (is (= [] (queries/snippet nil "tea")) "no text, no parts")
  (is (= [{:text "Café" :match? true}] (queries/snippet "Café" "cafe"))
      "folded like the index")
  (is (= [{:text "…"} {:text "supercalifragilistic" :match? true} {:text "…"}]
         (queries/snippet "see supercalifragilistic word" "super" {:width 8}))
      "a match wider than the window is shown whole")
  (is (= [{:text "A little "} {:text "value for value" :match? true}
          {:text " era, and value"}]
         (queries/snippet "A little value for value era, and value"
                        "\"value for value\""))
      "a phrase is one match")
  (is (= [{:text "…"} {:text "then "} {:text "rust" :match? true}
          {:text " and "} {:text "clojure" :match? true} {:text "…"}]
         (queries/snippet "clojure alone here, and then rust and clojure together"
                        "clojure rust"
                        {:width 25}))
      "the window that shows the most of the query wins")
  (is (= [{:text "今天"} {:text "天气很好" :match? true} {:text " good"}]
         (queries/snippet "今天天气很好 good" "天气很好"))
      "the pairs of characters are one match"))

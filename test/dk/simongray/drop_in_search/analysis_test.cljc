(ns dk.simongray.drop-in-search.analysis-test
  "Folding, words, and the terms of a text by position, with the scripts
  that have no spaces between words in pairs of characters."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.drop-in-search.analysis :as analysis]))

(deftest folding
  (is (= "cafe o strasse arhus" (analysis/fold "Café Ø Straße Århus")))
  (is (= "podcast film" (analysis/fold "Ｐｏｄｃａｓｔ ﬁlm"))
      "fullwidth letters and ligatures, by compatibility")
  (is (= "χαοσ" (analysis/fold "ΧΑΟΣ")) "final sigma is sigma")
  (is (= "istanbul lodz ss" (analysis/fold "İstanbul Łódź ẞ")))
  (is (= "ガ" (analysis/fold "ｶﾞ")) "halfwidth Katakana with its sound mark")
  (is (= "が" (analysis/fold "が")) "a kana keeps its sound mark")
  (is (= "한국어" (analysis/fold "한국어")) "Hangul syllables stay whole")
  (is (= "information" (analysis/fold "infor\u00ADmation")) "a soft hyphen")
  (is (= "" (analysis/fold nil))))

(deftest words
  (is (= ["Simon" "s" "Podcast" "2" "0" "comedy" "news"]
         (analysis/words "Simon's Podcast 2.0 comedy,news"))
      "punctuation splits words")
  (is (= ["AI" "メモリ管理" "abc" "今天"] (analysis/words "AIメモリ管理 abc今天"))
      "a change of script to or from one without spaces splits a word")
  (is (= ["ok"] (analysis/words "\u2764\uFE0F \uD83D\uDC68\u200D\uD83D\uDC69 ok"))
      "what folds to nothing is no word")
  (is (= [] (analysis/words nil))))

(deftest tokens
  (is (= ["podcasting" "2" "0" "the" "value" "for" "value" "era"]
         (analysis/tokens "Podcasting 2.0: the value-for-value era!")))
  (is (= ["爸爸" "爸不" "不在" "在家" "家的" "的中" "中秋" ["秋节" "节"]]
         (analysis/tokens "爸爸不在家的中秋节"))
      "the pairs of a run, and its last character with the last pair")
  (is (= ["ai" "メモ" "モリ" "リ管" ["管理" "理"]] (analysis/tokens "AIメモリ管理"))
      "pairs across the scripts of a run")
  (is (= [["天気" "気"]] (analysis/tokens "天気")))
  (is (= ["好" "ok"] (analysis/tokens "好 ok")) "a lone character")
  (is (= ["第" "13" "回"] (analysis/tokens "第13回")) "digits split a run")
  (is (= [["ガギ" "ギ"]] (analysis/tokens "ｶﾞｷﾞ")))
  (is (= ["학교" "교에" ["에서" "서"]] (analysis/tokens "학교에서")))
  (testing "Thai, Lao, Khmer and Myanmar, with the marks on their letters"
    (is (= ["ภา" "าษ" "ษา" "าไ" "ไท" ["ทย" "ย"]] (analysis/tokens "ภาษาไทย")))
    (is (= ["ยง่" "ง่า" ["าย" "ย"]] (analysis/tokens "ยง่าย")))
    (is (= 3 (count (filter vector? (analysis/tokens "ພາສາລາວ ភាសាខ្មែរ မြန်မာ"))))
        "a run each"))
  (is (= ["value" "for" "value"]
         (analysis/tokens "value\u00A0for\u3000value"))
      "every kind of white space")
  (is (= [] (analysis/tokens nil))))

(deftest spans
  (is (= [{:term "the" :start 0 :end 3 :position 0}
          {:term "cafe" :start 4 :end 8 :position 1}]
         (analysis/spans "The Café")))
  (is (= [{:term "中秋" :start 0 :end 2 :position 0}
          {:term "秋节" :start 1 :end 3 :position 1}
          {:term "节" :start 2 :end 3 :position 1}
          {:term "ok" :start 4 :end 6 :position 2}]
         (analysis/spans "中秋节 ok"))
      "overlapping pairs, and the last character at the last pair's position")
  (is (= [{:term "ガ" :start 0 :end 2 :position 0}] (analysis/spans "ｶﾞ"))
      "a character with its sound mark")
  (testing "the terms of spans are those of tokens"
    (doseq [s ["Podcasting 2.0: the era!" "爸爸不在家的中秋节 - A Mid" "ｶﾞｷﾞ 학교 ภาษา"]]
      (is (= (flatten (analysis/tokens s)) (map :term (analysis/spans s))) s))))

(deftest unspaced
  (is (analysis/unspaced? "中"))
  (is (analysis/unspaced? "ภา"))
  (is (not (analysis/unspaced? "a")))
  (is (not (analysis/unspaced? nil))))

(ns dk.simongray.drop-in-search.analysis
  "Text analysis for search on both platforms: folding, words, and the
  terms of a text by position.

  A word is a run of letters, marks and digits, and anything else splits
  words, e.g. \"Simon's\" is the words simon and s. A run in Korean, or in
  a script without spaces between words, such as Chinese, Japanese or
  Thai, has its pairs of characters, bigrams, as terms.

  The comments name the source of each part, and where it departs from it:

  - Unicode Standard Annex #15, Unicode Normalization Forms,
    https://www.unicode.org/reports/tr15/
  - Unicode Standard Annex #29, Unicode Text Segmentation, section 4,
    https://www.unicode.org/reports/tr29/
  - CaseFolding.txt of the Unicode Character Database,
    https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
  - Lucene's ASCIIFoldingFilter and CJKBigramFilter,
    https://lucene.apache.org/core/9_11_0/analysis/common/
  - Groonga's TokenBigram,
    https://groonga.org/docs/reference/tokenizers/token_bigram.html
  - Xapian's TermGenerator with FLAG_NGRAMS,
    https://xapian.org/docs/apidoc/html/classXapian_1_1TermGenerator.html"
  (:require [clojure.string :as str]))

;; The stroke letters of UTR #30 and Lucene's ASCIIFoldingFilter, and ß
;; and final sigma of CaseFolding.txt
(def ^:no-doc spellings
  "The letters that decomposition leaves whole, and how folding spells
  them out."
  {\æ "ae" \ø "o" \œ "oe" \ß "ss" \ð "d" \þ "th"
   \ł "l" \đ "d" \ħ "h" \ı "i" \ŋ "n" \ŧ "t" \ς "σ"})

;; Unicode, Blocks.txt: the five blocks of combining diacritical marks, the
;; variation selectors, the soft hyphen and the zero-width joiners
(def ^:no-doc marks
  "A pattern of what folding strips after decomposition: the diacritics,
  and the invisible characters that only change how a word looks."
  #"[\u00AD\u0300-\u036F\u1AB0-\u1AFF\u1DC0-\u1DFF\u200C\u200D\u2060\u20D0-\u20FF\uFE00-\uFE0F\uFE20-\uFE2F]")

(defn- highest
  "The highest code of a character of `s`, or -1 when `s` is empty."
  ^long [^String s]
  (let [n (count s)]
    (loop [i 0 top -1]
      (if (= i n)
        top
        (let [c #?(:clj (int (.charAt s i)) :cljs (.charCodeAt s i))]
          (recur (inc i) (if (< top c) (long c) top)))))))

;; A loop is far faster than the pattern on text without marks, and each
;; comparison takes two arguments so that Clojure compiles it inline
(defn- marked?
  "Whether `s` has a character that marks matches."
  [^String s]
  (let [n (count s)]
    (loop [i 0]
      (if (= i n)
        false
        (let [c #?(:clj (int (.charAt s i)) :cljs (.charCodeAt s i))]
          (if (or (= c 0x00AD)
                  (and (<= 0x0300 c) (<= c 0x036F))
                  (and (<= 0x1AB0 c) (<= c 0x1AFF))
                  (and (<= 0x1DC0 c) (<= c 0x1DFF))
                  (and (<= 0x200C c) (<= c 0x200D))
                  (= c 0x2060)
                  (and (<= 0x20D0 c) (<= c 0x20FF))
                  (and (<= 0xFE00 c) (<= c 0xFE0F))
                  (and (<= 0xFE20 c) (<= c 0xFE2F)))
            true
            (recur (inc i))))))))

(defn- lower
  [^String s]
  #?(:clj  (.toLowerCase s java.util.Locale/ROOT)
     :cljs (.toLowerCase s)))

;; UAX #15: NFKD folds the compatibility forms, e.g. fullwidth letters and
;; halfwidth Katakana, and NFC puts a kana and its sound mark, or the jamo
;; of a Hangul syllable, back together
(defn- decompose
  [s]
  #?(:clj  (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFKD)
     :cljs (.normalize s "NFKD")))

(defn- compose
  [s]
  #?(:clj  (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))

(defn- fold-unicode
  [s]
  (let [lowered  (lower (decompose s))
        stripped (if (marked? lowered)
                   (str/replace lowered marks "")
                   lowered)]
    (str/escape (compose stripped) spellings)))

(defn fold
  "The string `s` lower-cased, with its diacritics stripped and the forms
  of each letter made one, e.g. \"Café Ø Ｐｏｄ\" becomes \"cafe o pod\"."
  [s]
  (let [s (str s)]
    (if (< (highest s) 0x80)
      (lower s)
      (fold-unicode s))))

(def ^:no-doc word
  "The pattern of a word: a run of letters, marks and digits, with the
  invisible characters that folding strips."
  #"(?u)[\p{L}\p{M}\p{N}\u00AD\u200C\u200D\u2060]+")

;; UAX #29, section 4: these scripts need a dictionary to find words, and
;; WB999 breaks around each ideograph. Katakana counts with its sound and
;; repeat marks, as WB13 has it
(def ^:no-doc unspaced-letters
  "The class of the letters of Korean and of the scripts without spaces
  between words, for a pattern: Han, Hiragana, Katakana, Hangul, Bopomofo,
  Thai, Lao, Khmer and Myanmar."
  (str "\\p{sc=Han}\\p{sc=Hiragana}\\p{sc=Katakana}\\p{sc=Hangul}"
       "\\p{sc=Bopomofo}\\p{sc=Thai}\\p{sc=Lao}\\p{sc=Khmer}"
       "\\p{sc=Myanmar}\\u3031-\\u3035\\u3099-\\u309C\\u30FC\\uFF70"
       "\\uFF9E\\uFF9F"))

(def ^:no-doc parts
  "The pattern of the parts of a word: a run of letters of the scripts
  without spaces with their marks, or a run of anything else."
  (re-pattern (str "(?u)[" unspaced-letters "][" unspaced-letters "\\p{M}]*"
                   "|[^" unspaced-letters "]+")))

(def ^:no-doc unspaced-start
  "The pattern of a text that starts with a letter of a script without
  spaces between words."
  (re-pattern (str "(?u)^[" unspaced-letters "]")))

(def ^:no-doc characters
  "The pattern of a character with the marks that follow it, the sound
  marks of halfwidth Katakana among them."
  #"(?u)[^\p{M}\uFF9E\uFF9F][\p{M}\uFF9E\uFF9F]*")

(defn unspaced?
  "Whether `s` starts with a letter of Korean or of a script without
  spaces between words, such as Chinese, Japanese or Thai."
  [s]
  (boolean (re-find unspaced-start (str s))))

(defn- reduce-matches
  "Reduce the matches of `re` in `s` with `f`, from `init`. The `f` takes
  the accumulator, the text of the match and where it starts in `s`."
  [f init re ^String s]
  #?(:clj  (let [m (re-matcher re s)]
             (loop [acc init]
               (if (.find m)
                 (recur (f acc (.group m) (.start m)))
                 acc)))
     :cljs (let [global (js/RegExp. (.-source re) (str "g" (.-flags re)))]
             (loop [acc init]
               (if-let [found (.exec global s)]
                 (recur (f acc (aget found 0) (.-index found)))
                 acc)))))

;; On the JVM, loops over the general categories are several times faster
;; than the patterns word and characters, which they follow
(defn- reduce-words
  "Reduce the words of `s` with `f`, from `init`. The `f` takes the
  accumulator, the word and where it starts in `s`."
  [f init ^String s]
  #?(:clj  (let [n (.length s)]
             (loop [i 0 start -1 acc init]
               (if (< i n)
                 (let [c     (.codePointAt s (int i))
                       kind  (Character/getType c)
                       ;; the letters, marks and digits are kinds 1 to 11
                       word? (or (and (<= 1 kind) (<= kind 11))
                                 (= c 0x00AD)
                                 (and (<= 0x200C c) (<= c 0x200D))
                                 (= c 0x2060))
                       after (+ i (Character/charCount c))]
                   (cond
                     word?        (recur after (if (neg? start) i start) acc)
                     (neg? start) (recur after start acc)
                     :else        (recur after
                                         -1
                                         (f acc (subs s start i) start))))
                 (if (neg? start)
                   acc
                   (f acc (subs s start n) start)))))
     :cljs (reduce-matches f init word s)))

(defn- reduce-characters
  "Reduce the characters of `s` with `f`, from `init`, each with the marks
  that follow it. The `f` takes the accumulator, the character and where
  it starts in `s`."
  [f init ^String s]
  #?(:clj  (let [n (.length s)]
             (loop [i 0 start -1 acc init]
               (if (< i n)
                 (let [c     (.codePointAt s (int i))
                       kind  (Character/getType c)
                       ;; the marks are kinds 6 to 8
                       mark? (or (and (<= 6 kind) (<= kind 8))
                                 (= c 0xFF9E)
                                 (= c 0xFF9F))
                       after (+ i (Character/charCount c))]
                   (cond
                     mark?        (recur after start acc)
                     (neg? start) (recur after i acc)
                     :else        (recur after
                                         i
                                         (f acc (subs s start i) start))))
                 (if (neg? start)
                   acc
                   (f acc (subs s start n) start)))))
     :cljs (reduce-matches f init characters s)))

(defn- common-unspaced?
  "Whether `s` is one of the common Han, kana and Hangul characters, which
  fold to themselves."
  [^String s]
  (and (= 1 (count s))
       (let [c #?(:clj (int (.charAt s 0)) :cljs (.charCodeAt s 0))]
         (or (and (<= 0x4E00 c) (<= c 0x9FFF))
             (and (<= 0x3041 c) (<= c 0x3096))
             (and (<= 0x30A1 c) (<= c 0x30FA))
             (and (<= 0xAC00 c) (<= c 0xD7A3))))))

(defn- fold-character
  [c]
  (if (common-unspaced? c)
    c
    (fold c)))

(defn- conj-term
  [acc term]
  (if (= "" term)
    acc
    (conj! acc term)))

(defn- character-terms
  "The characters of `part` folded, each with its marks."
  [part]
  (persistent!
   (reduce-characters (fn [acc c _]
                        (conj-term acc (fold-character c)))
                      (transient [])
                      part)))

(defn- folded-characters
  "The characters of the `part` of a word that starts at `start` in its
  text, folded, as maps of :term, :start and :end."
  [part start]
  (persistent!
   (reduce-characters (fn [acc c at]
                        (let [term (fold-character c)
                              from (+ start at)]
                          (cond-> acc
                            (not= "" term)
                            (conj! {:term  term
                                    :start from
                                    :end   (+ from (count c))}))))
                      (transient [])
                      part)))

;; Lucene's CJKBigramFilter pairs the characters of a run across its
;; scripts, and leaves a lone character whole. Like Groonga's TokenBigram,
;; the last character is a term too, so that a query of one character can
;; find a run that ends in it; it shares the position of the last pair, as
;; the unigrams of CJKBigramFilter's outputUnigrams do
(defn- paired
  "The `characters` of a run in a script without spaces by position, with
  `pair-fn` making one of two neighbours: a pair at each position, the
  last with the last character too in a vector, or the one character."
  [pair-fn characters]
  (if (next characters)
    (let [pairs (mapv pair-fn characters (rest characters))]
      (conj (pop pairs) [(peek pairs) (peek characters)]))
    characters))

(defn- paired-span
  "The span of the pair of the character spans `a` and `b`."
  [a b]
  {:term  (str (:term a) (:term b))
   :start (:start a)
   :end   (:end b)})

(defn- conj-part-terms
  "The transient `acc` of terms by position, with those of the word `w`,
  which has a character of a script without spaces between words."
  [acc w]
  (reduce-matches (fn [acc part _]
                    (if (unspaced? part)
                      (reduce conj! acc (paired str (character-terms part)))
                      (conj-term acc (fold part))))
                  acc
                  parts
                  w))

(defn- reduce-parts
  "Reduce the parts of the word `w` with `f`, from `init`: its runs in a
  script without spaces, and its runs of anything else. The `f` takes the
  accumulator, the part and where it starts in `w`."
  [f init w]
  ;; a word below U+0E00, where Thai starts, is one part, which spares it
  ;; the pattern
  (if (< (highest w) 0x0E00)
    (f init w 0)
    (reduce-matches f init parts w)))

(defn- word-groups
  "The terms of the word `w` that starts at `start` in its text, as groups
  by position of maps of :term, :start and :end."
  [w start]
  (persistent!
   (reduce-parts (fn [acc part at]
                   (let [from (+ start at)]
                     (if (unspaced? part)
                       (->> (folded-characters part from)
                            (paired paired-span)
                            (map #(if (vector? %) % [%]))
                            (reduce conj! acc))
                       (let [term (fold part)]
                         (cond-> acc
                           (not= "" term)
                           (conj! [{:term  term
                                    :start from
                                    :end   (+ from (count part))}]))))))
                 (transient [])
                 w)))

(defn words
  "The words of `s` as they are written in it.

  A word in a script without spaces between words is the whole run, and a
  change to or from such a script splits a word, e.g. \"AIメモリ\" is two."
  [s]
  (persistent!
   (reduce-words (fn [acc w _]
                   (reduce-parts (fn [acc part _]
                                   (cond-> acc
                                     (not= "" (fold part)) (conj! part)))
                                 acc
                                 w))
                 (transient [])
                 (str s))))

(defn tokens
  "The terms of `s` by position, for an index or a query.

  Each is a folded word, or a vector of the terms at one position. A run
  in a script without spaces between words has a pair of characters at
  each position, and its last character shares the position of the last
  pair, e.g.

      (tokens \"Café 中秋节\")
      ;; => [\"cafe\" \"中秋\" [\"秋节\" \"节\"]]"
  [s]
  (let [add (fn [acc w _]
              (let [top (highest w)]
                ;; U+0E00 is Thai, the first script without spaces
                (cond
                  (< top 0x80)   (conj! acc (lower w))
                  (< top 0x0E00) (conj-term acc (fold-unicode w))
                  :else          (conj-part-terms acc w))))]
    (persistent! (reduce-words add (transient []) (str s)))))

(defn spans
  "The terms of `s` with where each is in `s`, for highlighting, as maps
  of :term, :start, :end and :position. The terms at one position have
  the same :position, e.g. the last pair of characters in a run of
  Chinese and its last character."
  [s]
  (let [add-group (fn [[acc position] group]
                    [(reduce #(conj! %1 (assoc %2 :position position))
                             acc
                             group)
                     (inc position)])
        add-word  (fn [[acc position :as placed] w start]
                    (if (< (highest w) 0x0E00)
                      (let [term (fold w)]
                        (if (= "" term)
                          placed
                          [(conj! acc {:term     term
                                       :start    start
                                       :end      (+ start (count w))
                                       :position position})
                           (inc position)]))
                      (reduce add-group placed (word-groups w start))))
        [acc _]   (reduce-words add-word [(transient []) 0] (str s))]
    (persistent! acc)))

(comment
  (fold "Café Ø Straße ΧΑΟΣ Ｐｏｄｃａｓｔ")
  (words "AIメモリ管理 don't stop")
  (tokens "Podcasting 2.0 爸爸不在家的中秋节")
  (spans "The 中秋节")
  #_.)

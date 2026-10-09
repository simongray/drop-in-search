(ns dk.simongray.drop-in-search.queries
  "The query language of full-text search, and a query matched against one
  text without an index, on both platforms.

  Parse reads a query as data, which dk.simongray.drop-in-search/query
  takes too. Snippet and matches? read a query the same way. To read it as
  a query of an index does, with the names of its fields and the words it
  holds, give them the options of dk.simongray.drop-in-search/query-opts:

      (snippet \"A little Clojure\" \"clojre \" (search/query-opts idx))
      ;; => [{:text \"A little \"} {:text \"Clojure\" :match? true}]

  Text is analyzed by search.analysis. The comments name the source of
  each part, and where it departs from it:

  - Lucene's simple query parser and FuzzyQuery,
    https://lucene.apache.org/core/9_11_1/core/
  - Elasticsearch's simple_query_string,
    https://www.elastic.co/docs/reference/query-languages/query-dsl/query-dsl-simple-query-string-query
  - SQLite FTS5's query syntax and its snippet function,
    https://www.sqlite.org/fts5.html"
  (:require [clojure.string :as str]
            [dk.simongray.drop-in-search.analysis :as analysis]))

;; The words of SQLite FTS5 and the signs of Lucene's simple query parser,
;; which Elasticsearch's simple_query_string shares, with & and ! for AND
;; and NOT as in many programming languages
(def syntax
  "The spellings of the operators of the query language that parse reads.

  A spelling of letters, e.g. AND, is an operator only as a word of its
  own, and a sign, e.g. &, inside a word too. A sign of :not counts only
  at the start of a word, and one of :field only after the name of a
  field. One of :prefix counts only at the end of a word, and so does one
  of :fuzzy, which a number of edits can follow, e.g. clojre~1."
  {:and    #{"AND" "&"}
   :or     #{"OR" "|"}
   :not    #{"NOT" "!" "-"}
   :field  #{":" "="}
   :prefix #{"*"}
   :fuzzy  #{"~"}})

(def default-limits
  "The limits of parse that its options don't set, which keep the cost of
  a query that anyone can type within bounds:

  - :max-terms, the words and phrases of a query that count, the first
  - :max-depth, the groups in groups of a query, where a deeper one is
    read without its parentheses"
  {:max-terms 32
   :max-depth 32})

(defn ^:no-doc spaced
  "The query `q` with every kind of whitespace as a space, since the regex
  \\s matches only ASCII whitespace on the JVM but all of it in JavaScript."
  [q]
  (str/replace q #"[\u00A0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000\uFEFF]" " "))

(defn- word-spelling?
  [s]
  (boolean (re-matches #"(?u)[\p{L}\p{N}]+" s)))

(defn- literal
  "The pattern of `s` that matches it as it is."
  [s]
  (str/replace s #"[\\^$.|?*+()\[\]{}\-/]" #(str "\\" %)))

(defn- lexeme-pattern
  "The pattern of the lexemes of a query in the spellings of `syntax`: a
  phrase in quotes, which an unclosed quote ends at the end, a
  parenthesis, a sign of :and or :or, or a word."
  [syntax]
  (let [signs (->> (concat (:and syntax) (:or syntax))
                   (remove word-spelling?)
                   (sort-by count >)
                   (map literal))
        sign  (str/join "|" signs)
        word  (if (seq signs)
                (str "(?:(?!" sign ")[^\\s\"()])+")
                "[^\\s\"()]+")]
    (re-pattern (str "\"[^\"]*\"?|[()]|"
                     (when (seq signs)
                       (str sign "|"))
                     word))))

(defn- raw-lexemes
  "The lexemes of the query `q` that `pattern` finds, as a lazy sequence of
  maps of :text, :start and :end, so that a reader can stop early."
  [pattern q]
  (letfn [(step [found from]
            (lazy-seq
             (when-let [[s & more] (seq found)]
               (let [start (long (str/index-of q s from))
                     end   (+ start (count s))]
                 (cons {:text s :start start :end end}
                       (step more end))))))]
    (step (re-seq pattern q) 0)))

(defn- leading-sign
  "The one of `signs` that `s` starts with and is longer than, or nil."
  [signs s]
  (some #(when (and (str/starts-with? s %)
                    (< (count %) (count s)))
           %)
        signs))

(defn- field-name
  "The name in `s` of one of `fields`, a map of folded names to fields,
  with the sign of :field after it in `syntax`, as a pair, or nil."
  [s syntax fields]
  (some (fn [sign]
          (when-let [at (str/index-of s sign)]
            (let [name (subs s 0 at)]
              (when (and (pos? at)
                         (contains? fields (analysis/fold name)))
                [name sign]))))
        (:field syntax)))

(defn- fuzzy-sign
  "The place in the word `s` where a sign of :fuzzy in `syntax` ends it,
  with the edits that follow the sign, as a pair, or nil. The edits are
  :auto without a number."
  [s syntax]
  (some (fn [sign]
          (when-let [at (str/last-index-of s sign)]
            (let [digits (subs s (+ at (count sign)))]
              (when (and (pos? at)
                         (re-matches #"[0-9]*" digits))
                [at (if (seq digits)
                      (parse-long digits)
                      :auto)]))))
        (:fuzzy syntax)))

(defn- word-lexemes
  "The lexemes of the word `raw` of a query in `syntax` that can name
  `fields`: a :not for a sign at its start, a :field for a field's name
  and a sign after it, and the :word, with :prefix? when it ends in a
  sign of one, and :fuzzy with the edits after a sign of :fuzzy."
  [{:keys [text start end] :as raw} syntax fields]
  (let [not-sign     (leading-sign (remove word-spelling? (:not syntax))
                                   text)
        after-not    (+ start (count not-sign))
        rest-text    (subs text (count not-sign))
        [name sign]  (field-name rest-text syntax fields)
        after-field  (+ after-not (count name) (count sign))
        word         (subs rest-text (+ (count name) (count sign)))
        prefix-sign  (some #(when (str/ends-with? word %) %) (:prefix syntax))
        word         (subs word 0 (- (count word) (count prefix-sign)))
        [at edits]   (when-not prefix-sign
                       (fuzzy-sign word syntax))
        word         (cond-> word at (subs 0 at))]
    (cond-> []
      not-sign    (conj {:kind :not :start start :end after-not})
      name        (conj {:kind  :field
                         :field (get fields (analysis/fold name))
                         :start after-not
                         :end   after-field})
      (seq word)  (conj (cond-> {:kind    :word
                                 :text    word
                                 :prefix? (some? prefix-sign)
                                 :start   after-field
                                 :end     end}
                          edits (assoc :fuzzy edits))))))

(defn- lexemes-of
  "The lexemes of the `raw` lexeme of a query in `syntax` that can name
  `fields`."
  [{:keys [text start end] :as raw} syntax fields]
  (let [operator (some #(when (contains? (get syntax %) text) %)
                       [:and :or :not])
        at       {:start start :end end}]
    (cond
      (str/starts-with? text "\"")
      [(assoc at :kind :phrase :text (str/replace text "\"" ""))]

      (= "(" text)
      [(assoc at :kind :open)]

      (= ")" text)
      [(assoc at :kind :close)]

      (and operator
           (or (not= :not operator)
               (word-spelling? text)))
      [(assoc at :kind operator)]

      ;; it's a :not only right before a phrase or a group, see lexemes
      (= :not operator)
      [(assoc at :kind :not-sign)]

      :else
      (word-lexemes raw syntax fields))))

(defn- lexemes
  "The lexemes of the query `q` in `syntax` that can name `fields`, as
  maps of :kind, :start and :end.

  The kinds are :open and :close, :and, :or and :not, a :field with the
  field, and a :phrase or :word with its :text. The last word has
  :typing? when nothing follows it. Reading stops after `max-terms`
  words and phrases."
  [q syntax fields max-terms]
  (let [term? #(contains? #{:word :phrase} (:kind %))
        read  (fn [[found n] raw]
                (if (<= (long max-terms) (long n))
                  (reduced [found n])
                  (let [more (lexemes-of raw syntax fields)]
                    [(into found more) (+ n (count (filter term? more)))])))
        [found] (reduce read [[] 0] (raw-lexemes (lexeme-pattern syntax) q))
        ;; a sign of :not on its own is a :not right before a phrase or a
        ;; group, e.g. -"a b", and else a dash, e.g. in "Part 1 - Intro"
        bound (fn [i {:keys [kind end] :as lexeme}]
                (let [after (get found (inc i))]
                  (cond
                    (not= :not-sign kind)
                    [lexeme]

                    (and (= end (:start after))
                         (contains? #{:phrase :open} (:kind after)))
                    [(assoc lexeme :kind :not)]

                    :else
                    [])))
        found (into [] (comp (map-indexed bound) cat) found)
        last  (peek found)]
    (if (and (= :word (:kind last))
             (= (count q) (:end last)))
      (conj (pop found) (assoc last :typing? true))
      found)))

(defn- word-query
  "The query of the word or phrase `lexeme`, with the options in `ctx`: a
  term, or a phrase of the words of a text with punctuation in it, or of
  the pairs of characters of a run of Chinese, or nil for no words.

  A word that ends in a sign of :prefix, or the last word while it's being
  typed, is a prefix, as is one character of Chinese, which a pair can
  only start. A word with :fuzzy is a term with the edits it allows."
  [{:keys [text prefix? typing? fuzzy] :as lexeme}
   {:keys [complete? min-prefix]}]
  (let [tokens  (analysis/tokens text)
        terms   (mapv #(if (vector? %) (first %) %) tokens)
        prefix? (or prefix?
                    (and typing?
                         complete?
                         (not fuzzy)
                         (<= (long min-prefix) (count (peek terms)))))]
    (case (count terms)
      0 nil
      1 (let [term  (first terms)
              lone? (and (string? (first tokens))
                         (analysis/unspaced? term))]
          (cond-> {:term term}
            (or prefix? lone?)        (assoc :prefix? true)
            (and fuzzy
                 (not (analysis/unspaced? term))) (assoc :fuzzy fuzzy)))
      (cond-> {:phrase terms}
        prefix? (assoc :prefix? true)))))

(defn ^:no-doc kind
  "The kind of the query `node`: :and, :or, :not, :term or :phrase, or nil."
  [node]
  (some #(when (contains? node %) %) [:and :or :not :term :phrase]))

(defn ^:no-doc map-leaves
  "The query `node` with `f` applied to each of its terms and phrases,
  those under a :not too."
  [f node]
  (let [k (kind node)]
    (case k
      (:and :or)      (update node k #(mapv (partial map-leaves f) %))
      :not            (update node :not #(map-leaves f %))
      (:term :phrase) (f node)
      node)))

;; Lucene's BooleanClause: a clause of an :and or :or that a match holds,
;; and a MUST_NOT clause, a :not, that leaves matches out
(defn ^:no-doc clauses
  "The `queries` of an :and or :or as a pair of those that a match holds
  and those that a :not leaves out."
  [queries]
  [(remove :not queries) (keep :not queries)])

(defn ^:no-doc leaves
  "The terms and phrases of the query `node` that a match holds, those
  under a :not left out."
  [node]
  (let [k (kind node)]
    (case k
      (:and :or)      (into [] (mapcat leaves) (get node k))
      (:term :phrase) [node]
      [])))

(defn- in-field
  "The query `node` with `field` on each of its terms and phrases that
  names no field of its own."
  [node field]
  (map-leaves #(cond-> % (not (:field %)) (assoc :field field)) node))

(defn- combine
  "The `queries` joined by `op`, :and or :or, as one query, or nil when
  there are none."
  [op queries]
  (case (count queries)
    0 nil
    1 (first queries)
    {op (into []
              (mapcat #(if (contains? % op) (get % op) [%]))
              queries)}))

(defn- operand-start?
  [lexeme]
  (contains? #{:word :phrase :open :not :field} (:kind lexeme)))

(declare parse-or)

(defn- parse-atom
  "The query of the term, phrase or group at `i` in the lexemes of `ctx`,
  as a pair of the query, or nil, and where the lexemes go on."
  [ctx i]
  (let [lexeme   (get-in ctx [:lexemes i])
        adjacent (get-in ctx [:lexemes (inc i)])]
    (case (:kind lexeme)
      :field  (if (and (= (:end lexeme) (:start adjacent))
                       (contains? #{:word :phrase :open} (:kind adjacent)))
                (let [[node j] (parse-atom ctx (inc i))]
                  [(some-> node (in-field (:field lexeme))) j])
                [nil (inc i)])
      ;; a group deeper than :max-depth is read without its parentheses,
      ;; so that no query can run the stack out
      :open   (if (< (long (:depth ctx)) (long (:max-depth ctx)))
                (let [[node j] (parse-or (update ctx :depth inc) (inc i))
                      closing  (get-in ctx [:lexemes j])]
                  [node (if (= :close (:kind closing)) (inc j) j)])
                [nil (inc i)])
      :phrase [(word-query lexeme ctx) (inc i)]
      :word   [(word-query lexeme ctx) (inc i)]
      [nil i])))

(defn- parse-unary
  "The query at `i` in the lexemes of `ctx` with the :not before it, as a
  pair of the query, or nil, and where the lexemes go on. Two :not undo
  each other."
  [ctx i]
  (let [nots     (count (take-while #(= :not (:kind %))
                                    (subvec (:lexemes ctx) i)))
        [node j] (parse-atom ctx (+ i nots))]
    [(if (and node (odd? nots)) {:not node} node) j]))

;; NOT binds tighter than AND, and AND than OR, as in SQLite FTS5. Words
;; without an operator between them take the default one at its level
(defn- parse-joined
  "The queries at `i` in the lexemes of `ctx` that `parse-operand` reads,
  joined by `op`, :and or :or, as a pair of the query, or nil, and where
  the lexemes go on."
  [op parse-operand ctx i]
  (loop [[node j] (parse-operand ctx i) queries []]
    (let [queries (cond-> queries node (conj node))
          lexeme  (get-in ctx [:lexemes j])]
      (cond
        (= op (:kind lexeme))
        (recur (parse-operand ctx (inc j)) queries)

        (and (= op (:operator ctx))
             (operand-start? lexeme))
        (recur (parse-operand ctx j) queries)

        :else
        [(combine op queries) j]))))

(defn- parse-and
  [ctx i]
  (parse-joined :and parse-unary ctx i))

(defn- parse-or
  [ctx i]
  (parse-joined :or parse-and ctx i))

(defn parse
  "The query `q`, a string, as data, read with the `opts` below, or nil
  when it asks for nothing.

  Words must all occur, and the last is a prefix to complete unless `q`
  ends in a space or a quote. The operators are those of syntax: AND, OR
  and NOT, or & | ! and - before a word, with parentheses, \"a phrase\",
  a prefix*, a word with typos~ and field:word or field=word for a word
  in one field:

      (parse \"title:clojure -rust\" {:aliases {\"title\" :title}})
      ;; => {:and [{:term \"clojure\" :field :title}
      ;;           {:not {:term \"rust\" :prefix? true}}]}

  A query as data is a map of :and or :or with a vector of queries, of
  :not with a query, of :term with a term, or of :phrase with a vector
  of terms in a row. A term or phrase has :prefix? when its last term is
  a prefix, and :field when it's in that field only.

  The `opts` are:

  - :aliases, the names that a query can give fields, as a map of a name
    to a field
  - :syntax, the spellings of operators over those of syntax, e.g. OG
    for AND in Danish with {:and #{\"OG\" \"&\"}}
  - :operator, :and or :or between words without one, :and by default
  - :prefix?, false to take the last word whole
  - :min-prefix, the fewest characters of a last word to complete, 1 by
    default
  - :max-terms and :max-depth, the limits of default-limits

  Nothing is an error, and a sign or a parenthesis that makes no sense
  counts for nothing."
  ([q]
   (parse q {}))
  ([q opts]
   {:pre [(map? opts)]}
   (let [{:keys [aliases operator prefix? min-prefix max-terms max-depth]
          :or   {operator :and prefix? true min-prefix 1}}
         (merge default-limits opts)
         spellings (merge syntax (:syntax opts))
         fields    (update-keys (or aliases {}) analysis/fold)
         q         (spaced (str q))
         ctx       {:lexemes    (lexemes q spellings fields max-terms)
                    :operator   operator
                    :complete?  prefix?
                    :min-prefix min-prefix
                    :depth      0
                    :max-depth  max-depth}
         n         (count (:lexemes ctx))]
     (loop [i 0 queries []]
       (if (< i n)
         (let [[node j] (parse-or ctx i)]
           ;; a lexeme that starts nothing, e.g. a stray ), is skipped
           (recur (if (= i j) (inc i) (long j))
                  (cond-> queries node (conj node))))
         (combine operator queries))))))

(defn- codes
  "The codes of the characters of `s`, as an array."
  [^String s]
  (let [n   (count s)
        arr (int-array n)]
    (dotimes [i n]
      (aset arr i #?(:clj (int (.charAt s i)) :cljs (.charCodeAt s i))))
    arr))

;; The optimal string alignment distance, Damerau-Levenshtein with a swap
;; of neighbours as one edit, as Lucene's FuzzyQuery counts transpositions
(defn- fill-row!
  "Fill `row` with the edits from the first `i` codes of `x` to each start
  of `y`, from the two rows before it, `one` and `two`, and give the
  fewest of them."
  [^ints x ^ints y i ^ints one ^ints two ^ints row]
  (let [i (long i)
        m (alength y)
        c (aget x (dec i))]
    (aset row 0 (int i))
    (loop [j 1 lowest i]
      (if (> j m)
        lowest
        (let [d        (aget y (dec j))
              deleted  (inc (aget one j))
              inserted (inc (aget row (dec j)))
              changed  (+ (aget one (dec j)) (if (= c d) 0 1))
              swapped? (and (> i 1)
                            (> j 1)
                            (= c (aget y (- j 2)))
                            (= (aget x (- i 2)) d))
              v        (cond-> (-> deleted (min inserted) (min changed))
                         swapped? (min (inc (aget two (- j 2)))))]
          (aset row j (int v))
          (recur (inc j) (min lowest v)))))))

(defn ^:no-doc distance
  "The edits that turn `a` into `b`, a swap of two neighbours counted as
  one, or nil when there are more than `most`."
  [a b most]
  (let [^ints x (codes (str a))
        ^ints y (codes (str b))
        n       (alength x)
        m       (alength y)
        most    (long most)
        start   (let [row (int-array (inc m))]
                  (dotimes [j (inc m)]
                    (aset row j (int j)))
                  row)]
    (when (<= (Math/abs (- n m)) most)
      (loop [i 1 ^ints two (int-array (inc m)) ^ints one start]
        (if (> i n)
          (let [d (aget one m)]
            (when (<= d most)
              d))
          (let [row    (int-array (inc m))
                lowest (long (fill-row! x y i one two row))]
            (when (<= lowest most)
              (recur (inc i) one row))))))))

(defn ^:no-doc edits
  "The edits that the term `leaf` allows, those of its :fuzzy, at most 2."
  [{:keys [fuzzy] :as leaf}]
  (if (number? fuzzy)
    (min 2 (long fuzzy))
    0))

;; The AUTO:3,6 of Elasticsearch's fuzziness, and the min_len_1typo and
;; min_len_2typo of Typesense
(defn- auto-edits
  "The edits that `term` allows by its length, with the shortest `lengths`
  of a word with one edit and with two, [3 6] by default."
  [term lengths]
  (let [[one two] (or lengths [3 6])
        n         (count term)]
    (cond
      (< n (long one)) 0
      (< n (long two)) 1
      :else            2)))

(defn- with-edits
  "The term `leaf` with the edits that it allows as its :fuzzy: those of
  its own, or of `fuzzy` when that's true or a number, or else when
  `unknown?` picks it and `fuzzy` is nil. Its length decides the edits of
  true and :auto by the `lengths` of auto-edits."
  [{:keys [term] :as leaf} fuzzy unknown? lengths]
  (let [wanted (cond
                 (:fuzzy leaf)    (:fuzzy leaf)
                 (true? fuzzy)    :auto
                 (number? fuzzy)  fuzzy
                 (and (nil? fuzzy)
                      (unknown? leaf)) :auto)]
    (cond-> leaf
      wanted (assoc :fuzzy (if (= :auto wanted)
                             (auto-edits term lengths)
                             wanted)))))

(defn- with-fuzzy
  "The query `node` with the edits that each of its terms allows, as
  with-edits gives them with `fuzzy`, `unknown?` and `lengths`, but none
  for a script without spaces."
  [node fuzzy unknown? lengths]
  (map-leaves #(if (and (:term %) (not (analysis/unspaced? (:term %))))
                 (with-edits % fuzzy unknown? lengths)
                 %)
              node))

(defn- as-query
  "The query `q` as data, read with the `opts`: a string parsed, a vector
  of terms taken as they are, or the data itself."
  [q opts]
  (cond
    (string? q) (parse q opts)
    (vector? q) (combine (:operator opts :and) (mapv #(hash-map :term %) q))
    :else       q))

(defn ^:no-doc fuzzy-query
  "The query `q` as as-query reads it with the `opts` of snippet, with the
  edits that each of its terms allows by their :fuzzy, :typo-lengths and
  :known?."
  [q {:keys [fuzzy typo-lengths known?] :as opts}]
  (some-> (as-query q opts)
          (with-fuzzy fuzzy
                      (if known?
                        (complement known?)
                        (constantly false))
                      typo-lengths)))

(defn- term-matches?
  [term prefix? found]
  (if prefix?
    (str/starts-with? found term)
    (= term found)))

(defn- leaf-matches?
  "Whether the term `found` is one that the term `leaf` matches: itself,
  one that starts with it for a prefix, or for a fuzzy term one that has
  its first character and is within its edits."
  [{:keys [term prefix?] :as leaf} found]
  (let [most (long (edits leaf))]
    (boolean
     (or (term-matches? term prefix? found)
         (and (pos? most)
              (= (first term) (first found))
              (some? (distance term found most)))))))

(defn- phrase-end
  "Where the `phrase` that starts with the span `first-span` ends, with
  the last term as a prefix if `prefix?`, from the spans `by-position`,
  or nil when it doesn't follow."
  [by-position first-span phrase prefix?]
  (let [last-i (dec (count phrase))]
    (loop [i 1 end (:end first-span)]
      (if (> i last-i)
        end
        (let [term     (nth phrase i)
              p?       (and prefix? (= i last-i))
              position (+ (long (:position first-span)) i)
              found    (some #(when (term-matches? term p? (:term %)) %)
                             (get by-position position))]
          (when found
            (recur (inc i) (:end found))))))))

(defn- hits
  "The places in the text of `spans` where the terms and phrases of
  `leaves` match, as maps of :start, :end and :leaf, the number of the
  one that matches, in order."
  [spans leaves]
  (let [by-position (delay (group-by :position spans))
        end-of      (fn [span {:keys [term phrase prefix?] :as leaf}]
                      (cond
                        term
                        (when (leaf-matches? leaf (:term span))
                          (:end span))

                        (term-matches? (first phrase)
                                       (and prefix? (= 1 (count phrase)))
                                       (:term span))
                        (phrase-end @by-position span phrase prefix?)))
        add-hits    (fn [acc span]
                      (reduce-kv (fn [acc i leaf]
                                   (if-let [end (end-of span leaf)]
                                     (conj! acc {:start (:start span)
                                                 :end   end
                                                 :leaf  i})
                                     acc))
                                 acc
                                 leaves))]
    (persistent! (reduce add-hits (transient []) spans))))

(defn- merged
  "The `hits`, maps of :start and :end in order, with those that overlap
  or touch made one, e.g. the pairs of characters of a run of Chinese."
  [hits]
  (reduce (fn [acc {:keys [start end] :as hit}]
            (let [last-hit (peek acc)]
              (if (and last-hit (<= start (:end last-hit)))
                (conj (pop acc) (update last-hit :end max end))
                (conj acc hit))))
          []
          (map #(select-keys % [:start :end]) hits)))

(defn- first-at-least
  "The index of the first of the sorted numbers `xs` that is at least `x`,
  or the count of `xs` when there is none."
  [xs x]
  (loop [lo 0 hi (count xs)]
    (if (< lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (if (< (long (nth xs mid)) (long x))
          (recur (inc mid) hi)
          (recur lo mid)))
      lo)))

(def ^:no-doc most-windows
  "The most windows that snippet weighs, those at the first matches."
  100)

;; SQLite FTS5's snippet function: the window that holds the most of the
;; query's phrases, of those that start at one
(defn- window
  "The window of about `width` characters of a text of `length` with
  `spans` that shows the most of the terms and phrases of `hits`, the
  first on a tie, as a pair of where it starts and ends."
  [spans hits width length]
  (let [starts     (mapv :start spans)
        ends       (mapv :end spans)
        hit-starts (mapv :start hits)
        place      (fn [at end-at-least]
                     (let [i    (first-at-least starts at)
                           from (if (< i (count starts)) (nth starts i) at)
                           to   (min length (max (+ from width) end-at-least))
                           j    (dec (first-at-least ends (inc to)))
                           to   (if (and (< to length) (<= 0 j))
                                  (max (nth ends j) from)
                                  to)]
                       [from to]))
        ;; the hits are in order, so those in a window are a range of them
        shown      (fn [[from to]]
                     (let [i (first-at-least hit-starts from)
                           j (first-at-least hit-starts (inc (long to)))]
                       (->> (subvec hits i (max i j))
                            (into #{} (comp (filter #(<= (:end %) to))
                                            (map :leaf)))
                            count)))
        tries  (cons (place 0 0)
                     (for [{:keys [start end]} (take most-windows hits)]
                       (place (max 0 (- start (quot width 3))) end)))]
    (reduce (fn [best-window w]
              (let [score (shown w)]
                (if (> score (:score best-window -1))
                  {:window w :score score}
                  best-window)))
            nil
            tries)))

(defn snippet
  "A window of `text` around the words that match the query `q`, as parts
  for a result list to render, with the `opts` below.

  It's a vector of maps of :text, with :match? on the matches, and an
  ellipsis where the window cuts the text. The text is plain, so escape
  it to put it into HTML. Of the windows at the start and at each match,
  the one with the most of the query's words and phrases wins, the first
  of them on a tie. The query is a string that parse reads, the same
  query as data, or a vector of terms taken as they are. The `opts` are
  those of parse, and these:

  - :width, the number of characters shown, 160 by default
  - :fuzzy, which words match words with typos too: by default those that
    :known? doesn't know, true for all, a number for all with at most that
    many edits, and false for none but those marked with ~ in the query
  - :typo-lengths, the shortest words with one edit and with two, [3 6]
    by default, as the AUTO:3,6 of Elasticsearch
  - :known?, a predicate of a term of a query, e.g. {:term \"clojure\"},
    that tells whether the documents of an index hold it; without it, no
    word is unknown"
  ([text q]
   (snippet text q {}))
  ([text q {:keys [width] :or {width 160} :as opts}]
   {:pre [(map? opts)]}
   (let [text       (str text)
         spans      (analysis/spans text)
         query      (fuzzy-query q opts)
         found      (hits spans (leaves query))
         length     (count text)
         [from to]  (:window (window spans found width length))
         inside     (filter #(and (<= from (:start %)) (<= (:end %) to))
                            found)
         piece      (fn [a b] {:text (subs text a b)})
         parts      (loop [pos  from
                           hits (merged inside)
                           acc  (if (pos? from) [{:text "…"}] [])]
                      (if-let [{:keys [start end]} (first hits)]
                        (let [acc (cond-> acc
                                    (< pos start) (conj (piece pos start)))]
                          (recur end
                                 (rest hits)
                                 (conj acc (assoc (piece start end)
                                                  :match? true))))
                        (cond-> acc
                          (< pos to) (conj (piece pos to)))))]
     (cond-> parts
       (< to length) (conj {:text "…"})))))

(defn- satisfied?
  "Whether the query `node` holds when `found?` tells which of its terms
  and phrases a text has, as a query of an index matches a document: all
  of :and, any of :or, but none of those under a :not, and nothing for a
  :not alone."
  [found? node]
  (let [k (kind node)]
    (case k
      (:and :or)      (let [[positive negative] (clauses (get node k))
                            holds?              #(satisfied? found? %)
                            all-or-any          ({:and every? :or some} k)]
                        (boolean (and (seq positive)
                                      (all-or-any holds? positive)
                                      (not-any? holds? negative))))
      (:term :phrase) (found? node)
      false)))

(defn matches?
  "Whether `text` matches the query `q`, read with the `opts` of snippet,
  as one field of a document would."
  ([text q]
   (matches? text q {}))
  ([text q opts]
   {:pre [(map? opts)]}
   (let [text   (str text)
         terms  (delay (into #{}
                             (mapcat #(if (vector? %) % [%]))
                             (analysis/tokens text)))
         spans  (delay (analysis/spans text))
         found? (fn [{:keys [term phrase prefix?] :as leaf}]
                  (cond
                    phrase
                    (boolean (seq (hits @spans [leaf])))

                    (or prefix? (pos? (edits leaf)))
                    (boolean (some #(leaf-matches? leaf %) @terms))

                    :else
                    (contains? @terms term)))]
     (satisfied? found? (fuzzy-query q opts)))))

(comment
  (parse "title:clojure -rust | \"value for va" {:aliases {"title" :title}})
  (parse "a b OR c (d | e) -f g*")
  (parse "clojre~ value~1 rust")
  (snippet "Systems programming, and a little Clojure" "clojure")
  (snippet "Closure tables" "clojre " {:fuzzy true})
  (matches? "Systems programming, and a little Clojure" "clojure -rust")
  #_.)

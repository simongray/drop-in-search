(ns dk.simongray.drop-in-search.bench
  "Measurements of an index of a corpus of real documents, on the JVM and
  in Node.

  The corpus has a search document to a line, in EDN. It reports the heap
  that the index holds, the time to build it, to query it and to add and
  remove a document, and the size of an index of its first 20,000
  documents as EDN and as CIFF, with the time to write and read each.
  Given a path, it writes the results of many queries there, or compares
  them with those written there before a change:

      clojure -M:bench corpus.edn [results.edn]
      clojure -M:cljs release bench
      node --expose-gc --max-old-space-size=8192 target/bench.js \\
        corpus.edn [results.edn]

  With memory in place of the corpus, then the corpus, it prints the heap
  of each part of the index instead, which the load of a machine doesn't
  change as it changes the times.

  Measure Node with a release build, as an app runs it, since the
  functions of a build for development are several times slower."
  (:require #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            #?(:clj [clojure.java.io :as io])
            [clojure.string :as str]
            [clojure.walk :as walk]
            [dk.simongray.drop-in-search :as search]
            [dk.simongray.drop-in-search.analysis :as analysis]
            [dk.simongray.drop-in-search.ciff :as ciff]
            [dk.simongray.drop-in-search.segment :as segment]
            #?(:cljs ["fs" :as fs]))
  #?(:clj (:import [java.lang.management ManagementFactory])))

(def opts
  "The options of the index of podcast-clj."
  {:boosts  {:title 3 :show 2 :author 2 :people 2 :chapters 2 :keywords 2
             :categories 1 :description 1 :transcript 1}
   :aliases {"notes" :description}})

(def typical
  "Typical queries of podcasts: a word, two words, a phrase, a prefix, a
  word with a typo and a word in a field."
  ["bitcoin "
   "climate change "
   "\"climate change\" "
   "interv"
   "podcsat "
   "title:interview "])

;; A document read from EDN differs from one made in code, so each is made
;; again as code makes it: a reader builds a vector as a transient, which
;; leaves it an empty node of 32 for its root, while a literal shares one.
;; In JavaScript, a reader makes a keyword anew each time, while code
;; shares one, and V8 keeps a substring of 13 characters or more as a
;; slice of the string it's from, which would hold on to the whole corpus
(def keywords
  "The keywords of the documents read, one of each, as code shares them."
  (atom {}))

(defn- as-in-code
  "The value `x` of a document read from EDN as code would make it."
  [x]
  (cond
    (vector? x)  (apply vector x)
    (keyword? x) #?(:clj  x
                    :cljs (or (get @keywords x)
                              (do (swap! keywords assoc x x) x)))
    (string? x)  #?(:clj  x
                    :cljs (js/JSON.parse (js/JSON.stringify x)))
    :else        x))

(defn read-corpus
  "The documents of the corpus at `path`, one in EDN to a line."
  [path]
  (let [xf (comp (remove str/blank?)
                 (map edn/read-string)
                 (map (partial walk/postwalk as-in-code)))]
    #?(:clj  (with-open [r (io/reader path)]
               (into [] xf (line-seq r)))
       :cljs (into [] xf (str/split-lines (.readFileSync fs path "utf8"))))))

;; V8 keeps the contents of typed arrays outside its heap
(defn heap-used
  "The bytes of the heap in use after a full garbage collection, with
  those of typed arrays in JavaScript."
  []
  #?(:clj  (do (dotimes [_ 3]
                 (System/gc)
                 (Thread/sleep 200))
               (.getUsed (.getHeapMemoryUsage
                          (ManagementFactory/getMemoryMXBean))))
     :cljs (do (dotimes [_ 3]
                 (js/gc))
               (let [^js m (js/process.memoryUsage)]
                 (+ (.-heapUsed m) (.-arrayBuffers m))))))

(defn now
  "The time in milliseconds, for intervals."
  []
  #?(:clj  (/ (System/nanoTime) 1e6)
     :cljs (js/performance.now)))

(defn timed
  "The value of calling `f` and the milliseconds it took, as a pair."
  [f]
  (let [start (now)
        v     (f)]
    [v (- (now) start)]))

(defn- round-ms
  "The mean milliseconds of calls of `f` over a quarter of a second or a
  thousand calls, whichever comes first."
  [f]
  (let [start (now)]
    (loop [n 1]
      (f)
      (let [elapsed (- (now) start)]
        (if (or (< 250 elapsed) (<= 1000 n))
          (/ elapsed n)
          (recur (inc n)))))))

(defn mean-ms
  "The mean milliseconds that calling `f` takes once warmed up, the least
  of five rounds, since the JIT and the garbage collector make the first
  and some of the others slower."
  [f]
  (round-ms f)
  (apply min (repeatedly 5 #(round-ms f))))

(defn- fixed
  [x digits]
  #?(:clj  (String/format java.util.Locale/ROOT
                          (str "%." digits "f")
                          (object-array [(double x)]))
     :cljs (.toFixed ^js x digits)))

(defn- mb
  [bytes]
  (str (fixed (/ bytes 1e6) 1) " MB"))

(defn- fields-of
  "The fields of the deftype `x` as pairs of a name and a value."
  [x]
  #?(:clj  (for [^java.lang.reflect.Field f (.getDeclaredFields (class x))
                 :when (not (java.lang.reflect.Modifier/isStatic
                             (.getModifiers f)))]
             [(.getName f) (.get f x)])
     :cljs (for [k (js-keys x)]
             [k (unchecked-get x k)])))

(defn- array-bytes
  "The bytes of the elements of `x` when it's an array of numbers, or nil."
  [x]
  #?(:clj  (let [c (class x)]
             (when (and (.isArray c) (.isPrimitive (.getComponentType c)))
               (* (count x)
                  (case (str (.getComponentType c))
                    "byte" 1 "short" 2 "int" 4 "float" 4 8))))
     :cljs (when (js/ArrayBuffer.isView x)
             (.-byteLength ^js x))))

(defn part-sizes
  "The bytes of the arrays of numbers of the segments of `idx`, by the
  name of the field of a segment that holds them."
  [idx]
  (apply merge-with +
         (for [segment (:segments idx)
               [k v]   (fields-of segment)
               :let    [n (array-bytes v)]
               :when   n]
           {k n})))

(defn- swapped
  "The word `w` with its second and third characters swapped, a typo."
  [w]
  (str (subs w 0 1) (subs w 2 3) (subs w 1 2) (subs w 3)))

(defn- probes-of
  "Queries of many kinds of the words of the document `doc`, as pairs of a
  query and its options."
  [doc]
  (let [words  #(filterv (fn [w] (<= 3 (count w)))
                         (analysis/words (get-in doc [:fields %] "")))
        [a b c] (words :description)
        [t u]   (words :title)
        long-w  (first (filter #(<= 5 (count %)) (words :description)))]
    (for [q    (cond-> []
                 a      (conj (str a " ")
                              (subs a 0 3)
                              (str "notes:" a " ")
                              (str a "~1 "))
                 b      (conj (str a " " b " ")
                              (str "\"" a " " b "\"")
                              (str a " | " b " ")
                              (str a " -" b " ")
                              (str a "-" (subs b 0 2)))
                 c      (conj (str "\"" a " " b " " c "\""))
                 t      (conj (str "title:" t " ")
                              (str t " " a))
                 u      (conj (str "title:\"" t " " u "\"")
                              (str "(" t " | " u ") -" a " "))
                 long-w (conj (str (swapped long-w) " ")))
          o    [{} {:fields #{:title}} {:fuzzy true} {:b 0.3 :k1 2.0}]
          :when (or (= {} o) (str/ends-with? q " "))]
      [q o])))

(defn probes
  "Queries of many kinds of the words of the documents `docs`, of one
  in so many of them."
  [docs]
  (into [] (comp (take-nth 1499) (mapcat probes-of)) docs))

(defn- bits
  "The bits of the double `x`, as a pair of ints, which hash as they are
  on both platforms."
  [x]
  #?(:clj  (let [l (Double/doubleToLongBits x)]
             [(unsigned-bit-shift-right l 32) (bit-and l 0xFFFFFFFF)])
     :cljs (let [^js view (js/DataView. (js/ArrayBuffer. 8))]
             (.setFloat64 view 0 x)
             [(.getUint32 view 0) (.getUint32 view 4)])))

(defn outcome
  "The results of the query `q` of `idx` with the options `o`, as the
  first twenty and a hash of them all, with their total and typos."
  [idx [q o]]
  (let [found (search/query idx q o)]
    {:q     q
     :o     o
     :meta  (meta found)
     :top   (mapv (juxt :id :score :typos) (take 20 found))
     :count (count found)
     :hash  (hash (mapv (juxt :id (comp bits :score) :typos) found))}))

(defn snippets
  "The snippets of the description of each of the `docs` for the queries
  of its words, and whether it matches them, in `idx`."
  [idx docs]
  (for [doc   docs
        [q _] (take 6 (probes-of doc))
        :let  [text (get-in doc [:fields :description] "")]]
    {:q        q
     :snippet  (search/snippet idx text q)
     :matches? (search/matches? idx text q)}))

(defn- write!
  [path s]
  #?(:clj  (spit path s)
     :cljs (.writeFileSync fs path s)))

(defn- read-text
  [path]
  #?(:clj  (when (.exists (io/file path)) (slurp path))
     :cljs (when (.existsSync fs path) (.readFileSync fs path "utf8"))))

(defn check!
  "Write the `outcomes` to `path` as EDN, or when the file is there,
  compare them with those it holds and print those that differ."
  [path outcomes]
  (if-let [text (read-text path)]
    (let [before (edn/read-string text)
          differ (remove (fn [[x y]] (= x y)) (map vector before outcomes))]
      (println (count outcomes) "outcomes,"
               (if (= (count before) (count outcomes))
                 (str (count differ) " differ")
                 (str "but " (count before) " before")))
      (doseq [[x y] (take 5 differ)]
        (println "before" (pr-str x))
        (println "after " (pr-str y))))
    (do (write! path (pr-str (vec outcomes)))
        (println (count outcomes) "outcomes written to" path))))

(defn- byte-size
  "The bytes of `body`, a string in UTF-8, or bytes."
  [body]
  #?(:clj  (let [^bytes bs (if (string? body)
                             (.getBytes ^String body "UTF-8")
                             body)]
             (alength bs))
     :cljs (.-length ^js (if (string? body)
                           (.encode ^js (js/TextEncoder.) body)
                           body))))

(defn- mean-of
  "The mean milliseconds of `n` calls of `f`, after one more."
  [n f]
  (f)
  (/ (second (timed #(dotimes [_ n] (f)))) n))

(defn saving
  "Print the size of an index of the `docs` as EDN and as CIFF, and the
  time to write it and to read it back."
  [docs]
  (let [idx   (search/index docs opts)
        edn   (pr-str idx)
        files (:put (ciff/plan idx))
        row   (fn [format size write read]
                (println format (mb size)
                         "write" (fixed (/ (mean-of 3 write) 1000) 2) "s"
                         "read" (fixed (/ (mean-of 3 read) 1000) 2) "s"))]
    (println "saving    " (count docs) "documents")
    (row "  EDN " (byte-size edn)
         #(pr-str idx)
         #(search/restore (edn/read-string edn)))
    (row "  CIFF" (reduce + (map (comp byte-size :body) files))
         #(ciff/plan idx)
         #(ciff/read-files files))))

(defn run
  "Measure an index of the corpus at `path`, and check the results of its
  queries with the file at `results`, if given."
  [path results]
  (let [base           (heap-used)
        docs           (atom (read-corpus path))
        _              (saving (subvec @docs 0 20000))
        n              (count @docs)
        probes         (probes @docs)
        texts          (vec (take-nth 4999 @docs))
        new-doc        (assoc (nth @docs 1000) :id "a new document")
        old-id         (:id (nth @docs 2000))
        [idx build-ms] (timed #(search/index @docs opts))
        _              (reset! docs nil)
        held           (- (heap-used) base)]
    (println "documents " n "in" (count (:segments idx)) "segments")
    (println "build     " (fixed (/ build-ms 1000) 1) "s")
    (println "heap      " (mb held))
    (println "  arrays  " (mb (reduce + (vals (part-sizes idx)))))
    ;; the names of the fields are those of the JVM, not of a release build
    #?(:clj (doseq [[k v] (sort-by val > (part-sizes idx))]
              (println "   " k (mb v))))
    (doseq [q typical]
      (println "query     " (fixed (mean-ms #(search/query idx q {:limit 10}))
                                   2)
               "ms" (pr-str q)
               (str "(" (:total (meta (search/query idx q))) ")")))
    (println "add       " (fixed (mean-ms #(search/add idx new-doc)) 2) "ms")
    (println "remove    " (fixed (mean-ms #(search/remove idx old-id)) 2) "ms")
    (when results
      (check! results (concat (map #(outcome idx %) probes)
                              (snippets idx texts))))))

(defn- stripped
  "The `idx` with each of its segments made again of its parts as `f`, a
  function of a map of them, changes them."
  [idx f]
  (update idx :segments
          (partial mapv #(segment/segment-of
                          (f (assoc (segment/parts %) :name "stripped"))))))

(defn memory
  "Print the heap that each part of an index of the corpus at `path`
  holds, by taking them away one at a time: what is stored, the map of
  ids, the ids, the terms, the postings and their arrays, and the rest."
  [path]
  (let [base  (heap-used)
        idx   (atom (search/index (read-corpus path) opts))
        parts [["stored   " #(stripped % (fn [m] (assoc m :stored [])))]
               ["map of ids" #(assoc % :docs {})]
               ["ids      " #(stripped % (fn [m] (assoc m :ids [])))]
               ["terms    " #(stripped % (fn [m]
                                           (assoc m
                                                  :term-text  ""
                                                  :text-start [0])))]
               ["postings " #(stripped % (fn [m]
                                           (assoc m
                                                  :postings   []
                                                  :doc-start  []
                                                  :term-start []
                                                  :lengths    [])))]]]
    (println "heap      " (mb (- (heap-used) base)))
    (reduce (fn [before [part f]]
              (swap! idx f)
              (let [after (heap-used)]
                (println "  " part (mb (- before after)))
                after))
            (heap-used)
            parts)
    (println "   the rest " (mb (- (heap-used) base)))))

(defn- main*
  [[path results]]
  (if (= "memory" path)
    (memory (or results "dev-resources/corpus/podcast-documents.edn"))
    (run (or path "dev-resources/corpus/podcast-documents.edn") results)))

#?(:clj
   (defn -main
     "Measure an index of the corpus, with the `args` of the namespace."
     [& args]
     (main* args)
     (shutdown-agents))

   :cljs
   (defn main
     "Measure an index of the corpus, with the `args` of the namespace."
     [& args]
     (main* args)))

(comment
  (def docs (read-corpus "dev-resources/corpus/podcast-documents.edn"))
  (def idx (time (search/index docs opts)))
  (part-sizes idx)
  (count (probes docs))
  #_.)

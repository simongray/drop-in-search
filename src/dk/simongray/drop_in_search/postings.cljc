(ns dk.simongray.drop-in-search.postings
  "The postings of the terms of a segment, compressed into bytes as Lucene
  compresses them, and a cursor that reads them as Lucene's PostingsEnum
  does, https://lucene.apache.org/core/10_0_0/core/

  A posting is a document that holds a term, with its runs: the fields
  that hold the term, each with the count of its positions there. The
  postings of a term come in blocks of 128, and each block has its skip
  data in front of it, as in Lucene912PostingsFormat:

  - the gap from the last document of the block before to its own last
  - the bytes of its postings, and of their positions
  - its postings, each the gap from the document before and its runs
  - the positions of each run in turn, the first in its field and the
    rest as gaps, but none of a run in a field without positions

  Each number is a VInt, as in the VInt blocks of Lucene99PostingsFormat.
  A run is one VInt of the gap from the field of the run before, whether
  another run follows, and its count less one up to 6, or else 7 and the
  count less 8 in a VInt of its own. A run of up to seven positions in a
  field less than eight after the one before then takes one byte."
  #?(:clj (:import [java.util Arrays])))

(def ^{:no-doc true :const true} positions-per-field
  "The block of positions that each field of a document gets.

  Each position of a term in a document is offset by its field's number
  times this. Two positions one apart are then in the same field, except
  at the end of a field this long. A longer field is indexed up to this
  length."
  1000000)

(def ^{:no-doc true :const true} block-size
  "The postings in a block of a term, which a cursor can skip past as a
  whole."
  128)

;; Lucene's DocIdSetIterator.NO_MORE_DOCS
(def ^{:no-doc true :const true} no-more
  "The document of a cursor that is past the last posting of its term."
  2147483647)

;; V8 makes a typed array far slower than a plain one, and collects many
;; of them slowly, while a query makes arrays for each cursor, and for the
;; positions of each posting of a phrase
(defn- make-ints
  "A new array of `n` ints, all 0, which in JavaScript is a plain array."
  [n]
  #?(:clj  (int-array n)
     :cljs (.fill (make-array n) 0)))

(defn ^:no-doc make-bytes
  "A new array of `n` bytes, all 0."
  [n]
  #?(:clj  (byte-array n)
     :cljs (js/Uint8Array. n)))

(defn ^:no-doc byte-count
  "The number of bytes of the array `bs`."
  ^long [bs]
  #?(:clj  (alength ^bytes bs)
     :cljs (.-length bs)))

(defn ^:no-doc byte-at
  "The byte at `i` of the bytes `bs`, from 0 to 255."
  ^long [bs ^long i]
  #?(:clj  (bit-and (aget ^bytes bs i) 0xFF)
     :cljs (aget bs i)))

(defn- put-byte!
  [bs ^long i ^long x]
  #?(:clj  (aset ^bytes bs i (unchecked-byte x))
     :cljs (aset bs i x)))

(defn- copy-bytes!
  "Copy the bytes of the array `src` from `from` to `to` into the array
  `dst` at `at`."
  [src from to dst at]
  #?(:clj  (System/arraycopy src (int from) dst (int at)
                             (int (- (long to) (long from))))
     :cljs (.set dst (.subarray src from to) at)))

(defn ^:no-doc trimmed-bytes
  "The first `n` bytes of the array `bs`, as an array of their own."
  [bs ^long n]
  #?(:clj  (Arrays/copyOf ^bytes bs (int n))
     :cljs (.slice bs 0 n)))

;; protobuf's Base 128 varints, which Lucene calls VInts: seven bits to a
;; byte, the lowest first, with the high bit set on all but the last
(defn- vint
  "The VInt that starts at `i` of the bytes `bs`."
  ^long [bs ^long i]
  (loop [i i x 0 shift 0]
    (let [b (byte-at bs i)]
      (if (< b 128)
        (+ x (bit-shift-left b shift))
        (recur (inc i) (+ x (bit-shift-left (- b 128) shift)) (+ shift 7))))))

(defn- after-vint
  "Where the VInt that starts at `i` of the bytes `bs` ends."
  ^long [bs ^long i]
  (if (< (byte-at bs i) 128)
    (inc i)
    (recur bs (inc i))))

(defn- after-vints
  "Where the `n` VInts that start at `i` of the bytes `bs` end."
  ^long [bs ^long i ^long n]
  (loop [i i n n]
    (if (pos? n)
      (recur (inc i) (if (< (byte-at bs i) 128) (dec n) n))
      i)))

;; The writers and the cursor keep what changes in mutable fields, and an
;; interface gives their methods primitive arguments on the JVM. In
;; JavaScript, the same methods are on Object.
#?(:clj
   (definterface IOut
     (^long size [])
     (grow [^long more])
     (putVInt [^long x])
     (putBytes [src ^long from ^long to])
     (putAll [out])
     (backing [])
     (clear [])
     (toBytes [])))

;; Bytes that grow as they're written, as Lucene's ByteBuffersDataOutput
(deftype Out [^:unsynchronized-mutable ^:mutable buf
              ^:unsynchronized-mutable ^:mutable ^long n]
  #?(:clj IOut :cljs Object)
  (size [_]
    n)

  (grow [_ more]
    (let [bigger (make-bytes (max (* 2 (byte-count buf)) (+ n more)))]
      (copy-bytes! buf 0 n bigger 0)
      (set! buf bigger)))

  (putVInt [this x]
    (when (< (- (byte-count buf) n) 5)
      (.grow this 5))
    (loop [x x]
      (if (< x 128)
        (do (put-byte! buf n x)
            (set! n (inc n)))
        (do (put-byte! buf n (+ 128 (bit-and x 127)))
            (set! n (inc n))
            (recur (unsigned-bit-shift-right x 7))))))

  (putBytes [this src from to]
    (when (< (- (byte-count buf) n) (- to from))
      (.grow this (- to from)))
    (copy-bytes! src from to buf n)
    (set! n (+ n (- to from))))

  (putAll [this out]
    (.putBytes this (.backing ^Out out) 0 (.size ^Out out)))

  (backing [_]
    buf)

  (clear [_]
    (set! n 0))

  (toBytes [_]
    (trimmed-bytes buf n)))

(defn ^:no-doc out
  "Empty bytes to write to."
  []
  (->Out (make-bytes 1024) 0))

#?(:clj
   (definterface IWriter
     (add [^long doc positions ^long from ^long to])
     (addBytes [^long doc bs ^long runs-from ^long runs-to
                ^long positions-from ^long positions-to])
     (^long runsOf [positions ^long from ^long to])
     (added [^long doc])
     (flush [])
     (endTerm [])))

;; Each block is written into bytes of its own first, since its skip data
;; goes in front of it
(deftype Writer [^Out out ^Out block-postings ^Out block-positions
                 ^ints positioned ^ints fields ^ints counts
                 ^:unsynchronized-mutable ^:mutable ^long prev
                 ^:unsynchronized-mutable ^:mutable ^long base
                 ^:unsynchronized-mutable ^:mutable ^long n]
  #?(:clj IWriter :cljs Object)
  ;; the posting of the document `doc` at the positions of the index in
  ;; order in `positions` from `from` to `to`, those of the fields without
  ;; positions only counted
  (add [this doc positions from to]
    (let [^ints positions positions
          runs            (.runsOf this positions from to)]
      (.putVInt block-postings (- doc prev))
      (loop [r 0 field -1 j from]
        (when (< r runs)
          (let [f    (aget fields r)
                c    (aget counts r)
                more (if (< (inc r) runs) 8 0)]
            (.putVInt block-postings
                      (+ (* 16 (- f field 1)) more (dec (min c 8))))
            (when (<= 8 c)
              (.putVInt block-postings (- c 8)))
            (when (pos? (aget positioned f))
              (let [offset (* f positions-per-field)]
                (loop [k 0 q 0]
                  (when (< k c)
                    (let [p (- (aget positions (+ j k)) offset)]
                      (.putVInt block-positions (- p q))
                      (recur (inc k) p))))))
            (recur (inc r) f (+ j c)))))
      (.added this doc)))

  ;; a posting's runs and positions are the same in any segment, so a
  ;; merge copies their bytes as they are
  (addBytes [this doc bs runs-from runs-to positions-from positions-to]
    (.putVInt block-postings (- doc prev))
    (.putBytes block-postings bs runs-from runs-to)
    (.putBytes block-positions bs positions-from positions-to)
    (.added this doc))

  (added [this doc]
    (set! prev doc)
    (set! n (inc n))
    (when (== n block-size)
      (.flush this)))

  ;; the runs of positions in order are those of each field in turn
  (runsOf [_ positions from to]
    (let [^ints positions positions]
      (loop [i from r -1 field -1]
        (if (< i to)
          (let [f (quot (aget positions i) positions-per-field)]
            (if (== f field)
              (do (aset counts r (inc (aget counts r)))
                  (recur (inc i) r field))
              (let [r (inc r)]
                (aset fields r (int f))
                (aset counts r 1)
                (recur (inc i) r f))))
          (inc r)))))

  (flush [_]
    (when (pos? n)
      (.putVInt out (- prev base))
      (.putVInt out (.size block-postings))
      (.putVInt out (.size block-positions))
      (.putAll out block-postings)
      (.putAll out block-positions)
      (.clear block-postings)
      (.clear block-positions)
      (set! base prev)
      (set! n 0)))

  ;; the end of the postings of a term, after which those of the next
  ;; term follow
  (endTerm [this]
    (.flush this)
    (set! prev 0)
    (set! base 0)))

(defn ^:no-doc writer
  "A writer of the postings of terms of a segment of `width` fields, one
  term after another, into `out`, bytes to write to. The `positioned`
  array has a 1 for each field with positions, and a 0 for one without."
  [out width positioned]
  (->Writer out (->Out (make-bytes 1024) 0) (->Out (make-bytes 1024) 0)
            positioned (make-ints width) (make-ints width) 0 0 0))

#?(:clj
   (definterface ICursor
     (^long nextDoc [])
     (^long advance [^long target])
     (^long runs [])
     (^long field [^long r])
     (^long freq [^long r])
     (^long tf [])
     (positions [])
     (^long readPositions [out ^long at])
     (copyTo [w ^long doc])
     (^long readVInt [])
     (nextBlock [])
     (readRuns [])))

;; The positions of a posting are read only when they're asked for, so a
;; cursor counts those it passes, to skip them when it must, as Lucene's
;; posPendingCount does
(deftype Cursor [buf ^ints positioned ^ints fields ^ints counts
                 ^:unsynchronized-mutable ^:mutable ^long at
                 ^:unsynchronized-mutable ^:mutable ^long left
                 ^:unsynchronized-mutable ^:mutable ^long block-left
                 ^:unsynchronized-mutable ^:mutable ^long block-last
                 ^:unsynchronized-mutable ^:mutable ^long block-end
                 ^:unsynchronized-mutable ^:mutable ^long prev
                 ^:unsynchronized-mutable ^:mutable ^long doc
                 ^:unsynchronized-mutable ^:mutable ^long n-runs
                 ^:unsynchronized-mutable ^:mutable ^long term-freq
                 ^:unsynchronized-mutable ^:mutable ^long runs-at
                 ^:unsynchronized-mutable ^:mutable ^long pos-at
                 ^:unsynchronized-mutable ^:mutable ^long pending
                 ^:unsynchronized-mutable ^:mutable ^long unread]
  #?(:clj ICursor :cljs Object)
  ;; the document of the next posting, or no-more
  (nextDoc [this]
    (cond
      (pos? block-left)
      (do (set! prev (+ prev (.readVInt this)))
          (set! pending (+ pending unread))
          (set! runs-at at)
          (.readRuns this)
          (set! block-left (dec block-left))
          (set! left (dec left))
          (set! doc prev)
          doc)

      (pos? left)
      (do (.nextBlock this)
          (.nextDoc this))

      :else
      (do (set! doc no-more)
          doc)))

  ;; the first document of at least `target`, past the blocks whose last
  ;; document is less, or no-more
  (advance [this target]
    (loop []
      (cond
        (<= target doc)
        doc

        (and (pos? block-left) (<= target block-last))
        (do (.nextDoc this)
            (recur))

        :else
        (do (set! left (- left block-left))
            (set! block-left 0)
            (if (pos? left)
              (do (.nextBlock this)
                  (recur))
              (do (set! doc no-more)
                  doc))))))

  ;; the runs of the posting, the field and the count of each, and the
  ;; count of them all
  (runs [_]
    n-runs)

  (field [_ r]
    (aget fields r))

  (freq [_ r]
    (aget counts r))

  (tf [_]
    term-freq)

  ;; the positions of the posting in the fields with positions, as those
  ;; of the index, in an array of their own, or into `out` from `at`,
  ;; giving where they end there, once
  (positions [this]
    (let [out (make-ints unread)]
      (.readPositions this out 0)
      out))

  (readPositions [_ out at]
    (set! pos-at (after-vints buf pos-at pending))
    (set! pending 0)
    (let [^ints out out]
      (loop [i pos-at r 0 k 0 j at q 0]
        (cond
          (or (zero? unread) (== r n-runs))
          (do (set! pos-at i)
              (set! unread 0)
              j)

          (or (== k (aget counts r))
              (zero? (aget positioned (aget fields r))))
          (recur i (inc r) 0 j 0)

          :else
          (let [q (+ q (vint buf i))]
            (aset out j (int (+ (* (aget fields r) positions-per-field) q)))
            (recur (after-vint buf i) r (inc k) (inc j) q))))))

  ;; the posting written with the writer `w` as one of the document `doc`
  (copyTo [_ w doc]
    (set! pos-at (after-vints buf pos-at pending))
    (set! pending 0)
    (let [end (after-vints buf pos-at unread)]
      (.addBytes ^Writer w doc buf runs-at at pos-at end)
      (set! pos-at end)
      (set! unread 0)))

  (readVInt [_]
    (let [b (byte-at buf at)]
      (if (< b 128)
        (do (set! at (inc at))
            b)
        (let [x (vint buf at)]
          (set! at (after-vint buf at))
          x))))

  (nextBlock [this]
    (set! at block-end)
    (set! prev block-last)
    (set! block-last (+ block-last (.readVInt this)))
    (let [m (.readVInt this)
          k (.readVInt this)]
      (set! block-end (+ at m k))
      (set! pos-at (+ at m)))
    (set! pending 0)
    (set! unread 0)
    (set! block-left (min block-size left)))

  (readRuns [this]
    (loop [r 0 f -1 total 0 placed 0]
      (let [code   (.readVInt this)
            f      (+ f 1 (bit-shift-right code 4))
            c      (inc (bit-and code 7))
            c      (if (== 8 c) (+ c (.readVInt this)) c)
            placed (+ placed (* c (aget positioned f)))]
        (aset fields r (int f))
        (aset counts r (int c))
        (if (pos? (bit-and code 8))
          (recur (inc r) f (+ total c) placed)
          (do (set! n-runs (inc r))
              (set! term-freq (+ total c))
              (set! unread placed)))))))

(defn ^:no-doc cursor
  "A cursor before the first of the `n` postings of a term that start at
  `at` of the bytes `bs`, of a segment of `width` fields, with a 1 in the
  array `positioned` for each with positions, and a 0 for one without."
  [bs at n width positioned]
  (->Cursor bs positioned (make-ints width) (make-ints width)
            at n 0 0 at 0 -1 0 0 0 0 0 0))

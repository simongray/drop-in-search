drop-in-search
==============

[![Clojars Project](https://img.shields.io/clojars/v/dk.simongray/drop-in-search.svg)](https://clojars.org/dk.simongray/drop-in-search)

This is a Clojure and ClojureScript library for adding full-text search
to your app, e.g. to search the posts of a blog, the episodes of a
podcast app or the records of a CMS. It's a search engine without a
server or a UI: you turn your data into documents, and it gives you the
best matches to show however you like.

- **Ranking.** Results are ranked by
  [BM25F](https://www.elastic.co/blog/practical-bm25-part-2-the-bm25-algorithm-and-its-variables),
  and a match in one field can count for more than a match in another.
- **Queries.** Your users can type AND, OR and NOT, "phrases", prefixes
  and field:word, as in SQLite or Lucene. A word with a typo still finds
  the words it's close to.
- **Many languages.** Case and diacritics are ignored, and Chinese,
  Japanese, Korean and Thai work without a dictionary.
- **Snippets.** It marks where a text matches a query, e.g. to highlight
  the matches in a list of results.
- **Saving.** An index prints as EDN, and a large one can be saved in
  the Common Index File Format, e.g. to a folder or to
  [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API).

The same code runs on the JVM, in Node and in the browser, and gives the
same results on each.

> This library was spun out of [podcast-clj](https://github.com/simongray/podcast-clj),
> a library for making podcast software, which uses it to search episodes
> and shows. Like podcast-clj, it was developed with assistance from
> frontier LLMs.

Getting started
---------------

It requires Clojure 1.11+ and Java 11+. Add it from
[Clojars](https://clojars.org/dk.simongray/drop-in-search) to the `:deps`
in your `deps.edn`:

```clojure
dk.simongray/drop-in-search {:mvn/version "0.3.0"}
```

For unreleased changes, depend on the latest commit on `master` instead.
In shadow-cljs, a Git dependency only works through `deps.edn`, so set
`:deps true` in your `shadow-cljs.edn` too.

Searching
---------

Make a document of each of your records, with an `:id`, the `:fields` to
search, and the `:stored` data that comes back with a result. Then index
the documents and query the index:

```clojure
(require '[dk.simongray.drop-in-search :as search])

(def posts
  [{:slug  "values"
    :title "The value of values in Clojure"
    :body  "Immutable data makes a program easier to reason about."
    :tags  #{"clojure" "data"}}
   {:slug  "borrowing"
    :title "Fighting the borrow checker"
    :body  "Rust's ownership rules, seen by someone who mostly writes Clojure."
    :tags  #{"rust"}}])

(defn document
  "The search document of the blog post `post`."
  [post]
  {:id     (:slug post)
   :fields (select-keys post [:title :body :tags])
   :stored (select-keys post [:title])})

(def idx
  (search/index (map document posts) {:boosts {:title 3}}))

(search/query idx "clojure -rust")
;; => [{:id     "values"
;;      :score  0.13…
;;      :stored {:title "The value of values in Clojure"}}]
```

The `:boosts` make a match in the title count three times as much as one
in the other fields, and `-rust` leaves out the posts that mention Rust.
A word with a typo matches the words it's close to, which the result
lists under `:typos`:

```clojure
(search/query idx "borow")
;; => [{:id     "borrowing"
;;      :score  0.19…
;;      :stored {:title "Fighting the borrow checker"}
;;      :typos  {"borow" ["borrow"]}}]
```

The index is a value, and nothing runs in the background. When a post
changes, `(search/add idx (document post))` gives an index with its new
version, and `(search/remove idx "values")` gives one without it. To
count the documents, use `search/size`, and to get one by its id, with
its stored data, use `search/document`.

Snippets
--------

To show where a result matches, cut a window of its text around the
matches with `search/snippet`, which reads the query as `search/query`
does:

```clojure
(search/snippet idx (:body (second posts)) "clojure")
;; => [{:text "Rust's ownership rules, seen by someone who mostly writes "}
;;     {:text "Clojure" :match? true}
;;     {:text "."}]
```

When the query names a field, e.g. `title:clojure`, give the field that
the text comes from as `:field`, so that only the words meant for that
field are marked. To tell whether a text matches a query at all, use
`search/matches?`.

More like this
--------------

To find the documents most like another, e.g. related posts to list under
a post, give the document to `search/more-like-this`:

```clojure
(def related
  (search/more-like-this idx (document (first posts))))

(mapv :id related)
;; => ["borrowing"]

(:terms (meta related))
;; => [["clojure" 0.8] ["the" 0.6…]]
```

It looks for the terms that set the document apart, those that it holds
often and few other documents hold, as Lucene's
[MoreLikeThis](https://lucene.apache.org/core/9_11_1/queries/org/apache/lucene/queries/mlt/MoreLikeThis.html)
does. The document itself isn't among the results, and their metadata
lists the terms, e.g. to show why a result came up. In a larger index,
leave out words like "the" with `:stop-words`, or with `:max-doc-freq`,
which leaves out the words of more than a share of the documents.

It also takes a text, or several documents, e.g. the posts that someone
read last. Those given as `:unlike` count against them, as in the
[Rocchio algorithm](https://nlp.stanford.edu/IR-book/html/htmledition/the-rocchio71-algorithm-1.html),
and a `:weight-fn` can make some count for more, e.g. the most recent.

When the documents come in groups, e.g. posts by author, give a
`:group-fn` that takes a document's id and returns its group. Then
`:max-group-freq` leaves out the words that most groups use, so that one
prolific author can't make a word common. In `search/query` too,
`:max-per-group` caps the results of each group. The docstring of
`search/more-like-this` lists all its options.

Options
-------

The functions that build an index or read a query take a map of options
as their last argument, e.g. `:limit` and `:offset` for a page of
results. The metadata of the page has the `:total` of the matches, for
"1 to 10 of 243", unless a `:filter-pred` or `:max-per-group` let the
page fill up before every match was read.

The options of `search/index` are the defaults of every query of that
index, e.g. the `:boosts` above. All the functions take the same set of
options, so one map can go to each, and an option that isn't in it
throws, e.g. `:limt` for `:limit`. The docstrings of `search/query` and
`search.queries/parse` list them all.

Stemming
--------

The library doesn't stem words, but you can bring your own
[stemmer](https://nlp.stanford.edu/IR-book/html/htmledition/stemming-and-lemmatization-1.html)
as the `:term-fn` of the index. It's a function of a term that gives the
term to index and search for instead, and it applies to the documents,
the queries and the snippets alike, so that "checkers" finds "checker":

```clojure
(require '[clojure.string :as str])

(defn singular
  "The term `t` without the s of an English plural, a stand-in for a real
  stemmer."
  [t]
  (cond-> t
    (and (< 3 (count t)) (str/ends-with? t "s")) (subs 0 (dec (count t)))))

(def stemmed
  (search/index (map document posts) {:term-fn singular}))

(mapv :id (search/query stemmed "checkers" {:fuzzy false}))
;; => ["borrowing"]
```

Here `:fuzzy false` turns off the typos, so that only the stem finds the
post. A function can't be saved with an index, so give the same
`:term-fn` to `search/restore` and `ciff/read-files` when you read one
back. Without it, the words of new documents and of queries aren't
stemmed, while those already in the index are.

Memory
------

An index keeps its postings, the documents and positions of each word,
compressed as
[Lucene's postings format](https://lucene.apache.org/core/10_0_0/core/org/apache/lucene/codecs/lucene912/Lucene912PostingsFormat.html)
does: in blocks of 128 documents, each with the skip data that lets a
query pass it, and mostly a byte for each number. For 152,006 podcast
shows and episodes, with 23 million words in their fields, measured on
an Apple M1 with JDK 24 and Node 24 on 14 October 2026:

| | Heap | Stored data | Build |
|---|---|---|---|
| JVM | 209 MB | 78 MB | 13 s |
| Node | 317 MB | 190 MB | 37 s |

The postings take 63 MB of each, and the ids and the terms most of the
rest. The stored data of six fields to a document takes the most in
Node, so store only what a list of results shows, e.g. a title and an
id to find the rest by.

To save more, index long fields without the positions of their words,
e.g. the notes of an episode, by giving `search/index` a set of them as
`:without-positions`. Most of the postings are positions, and a word in
such a field still counts as often as it occurs. A phrase then finds its
words anywhere in the field, as
[Xapian](https://trac.xapian.org/ticket/56) treats a phrase where it has
no positions, and stays a phrase in the other fields. The CIFF files of
such an index are of a newer version, which 0.2 doesn't read.

You can also index only the first words of a long field, e.g. with
`{:transcript 10000}` as `:max-field-terms`, as Lucene's
[LimitTokenCountFilter](https://lucene.apache.org/core/9_11_1/analysis/common/org/apache/lucene/analysis/miscellaneous/LimitTokenCountFilter.html)
does. The words after them are left out, as if the field ended there.

Saving an index
---------------

An index prints as EDN, and `search/restore` reads it back:

```clojure
(require '[clojure.edn :as edn])

(def saved (pr-str idx))

(search/restore (edn/read-string saved))
```

A large index is better saved in the
[Common Index File Format](https://github.com/osirrc/ciff) (CIFF), with
the `dk.simongray.drop-in-search.ciff` namespace, required as `ciff`.
Then `ciff/plan` gives you a manifest in EDN and a file for each
[segment](https://lucene.apache.org/core/9_11_1/core/org/apache/lucene/codecs/lucene99/package-summary.html#Segments)
of the index, along with the names of the files to delete. A segment's
file is named after what it holds, so a change only writes the segments
that are new. Keep the files in a folder, in IndexedDB or anywhere else,
and read them back with `ciff/read-files`.

Switch to CIFF once writing the whole index after each change takes too
long, or reading the EDN back in JavaScript does. For the first 20,000
of the podcast shows and episodes above:

| | Size | Write | Read |
|---|---|---|---|
| EDN on the JVM | 22 MB | 0.5 s | 0.4 s |
| CIFF on the JVM | 25 MB | 0.6 s | 0.4 s |
| EDN in Node | 22 MB | 0.4 s | 2.4 s |
| CIFF in Node | 25 MB | 0.9 s | 1.4 s |

A CIFF index records the version of its format, and `ciff/read-files`
throws with the `:type` `:dk.simongray.drop-in-search.ciff/newer-format`
for one that's newer than it reads. The EDN of an index has no version,
so index the documents again after a release that changes its shape.

Principles
----------

- **Standards first.** The ranking is BM25F as Robertson and Zaragoza
  define it, and the query language takes its words from
  [SQLite FTS5](https://www.sqlite.org/fts5.html#full_text_query_syntax)
  and its symbols from
  [Lucene's simple query parser](https://lucene.apache.org/core/9_11_1/queryparser/org/apache/lucene/queryparser/simple/SimpleQueryParser.html).
  Other search engines can read an index saved as CIFF, and the code
  cites its sources.
- **Fields count together.** A word's matches are weighted by field and
  added up before they're scored, so a word that appears once in each of
  three fields counts as much as one that appears three times in one.
- **The same words everywhere.** Text splits into words at anything that
  isn't a letter or a digit. The library doesn't use the
  [word segmenters](https://en.wikipedia.org/wiki/Text_segmentation#Word_segmentation)
  of Java and JavaScript, since they split Chinese differently, so an
  index finds the same words on every device. A date is indexed as its
  date and time in UTC, e.g. 2024-03-01 09:30:00.
- **No stemming.** Instead, the last word of a query completes as a
  prefix, and a `:term-fn` can bring a stemmer of your own.
- **Typos per word.** By default, only a word that no document holds
  matches the words close to it, rather than every word of a query that
  finds too little, so the words that are spelled right stay exact.
- **Any query is safe.** Nothing a user types is an error, and limits on
  the words, groups, prefixes and typos of a query keep its cost bounded.
- **Convention over configuration.** The operators and the limits have
  sensible defaults in public vars, and options change them, e.g. OG for
  AND in Danish.
- **One codebase.** The library is written in `.cljc`, with no
  dependencies but Clojure.

Development
-----------

```bash
clojure -X:test                       # the tests on the JVM
npm install                           # once, for ClojureScript
clojure -M:cljs compile test          # the tests in Node
clojure -M:cljs compile browser-test  # the tests in a browser
node test/browser-run.mjs             # run them in a headless Chrome
clojure -M:bench corpus.edn           # measure an index of a corpus
```

License
-------

The drop-in-search project is licensed under the [MIT licence](LICENSE).

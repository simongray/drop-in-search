drop-in-search
==============

[![Clojars Project](https://img.shields.io/clojars/v/dk.simongray/drop-in-search.svg)](https://clojars.org/dk.simongray/drop-in-search)

This is a Clojure and ClojureScript library for adding full-text search
to your app, e.g. to search the posts of a blog, the episodes of a
podcast app, or the records of a CMS. It's a search engine without a
server or a UI: you make documents of your own data, and it gives you the
best matches, which you show however you like.

- **Ranking.** It ranks the results by
  [BM25F](https://www.elastic.co/blog/practical-bm25-part-2-the-bm25-algorithm-and-its-variables),
  and a match in one field can count more than a match in another.
- **Queries.** Your users can type AND, OR and NOT, "phrases", prefixes
  and field:word, as in SQLite or Lucene, and a word with a typo still
  finds the words it's close to.
- **Many languages.** Case and diacritics don't count, and words in
  Chinese, Japanese, Korean and Thai are found without a dictionary.
- **Snippets.** It marks where a text matches a query, e.g. for a list of
  results.
- **Saving.** An index prints as EDN, and a large one can be saved in the
  Common Index File Format, e.g. in a folder or
  [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API).

The same code runs on the JVM, in Node and in the browser, with the same
results on each.

> This library was spun out of [podcast-clj](https://github.com/simongray/podcast-clj),
> a library for making podcast software, which uses it to search episodes
> and shows. Like podcast-clj, it was developed with assistance from
> frontier LLMs.

Getting started
---------------

It requires Clojure 1.11+ and Java 11+. For the latest release, add it
from [Clojars](https://clojars.org/dk.simongray/drop-in-search) to the
`:deps` in your `deps.edn`:

```clojure
dk.simongray/drop-in-search {:mvn/version "0.2.0"}
```

For changes that aren't released yet, use the SHA of the latest commit on
`master` instead:

```clojure
dk.simongray/drop-in-search
{:git/url "https://github.com/simongray/drop-in-search"
 :git/sha "…"}
```

For ClojureScript, shadow-cljs only reads Git dependencies from
`deps.edn`, so also set `:deps true` in your `shadow-cljs.edn`.

Then make a document of each of your records. A document has an `:id`,
the `:fields` to search, and the `:stored` data that comes back in a
result:

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

The `:boosts` make a match in the title count three times as much as a
match in the other fields, and `-rust` leaves out the posts that mention
Rust. A word with a typo also matches the words it's close to, and the
result lists them under `:typos`:

```clojure
(search/query idx "borow")
;; => [{:id     "borrowing"
;;      :score  0.19…
;;      :stored {:title "Fighting the borrow checker"}
;;      :typos  {"borow" ["borrow"]}}]
```

The index is a value, and nothing runs in the background. When a post
changes, `(search/add idx (document post))` gives an index with the new
version of it, and `(search/remove idx "values")` gives one without the
post. The number of documents is `(search/size idx)`, and
`(search/document idx "values")` gives one by its id, with what's
stored.

To show where a result matches, `search/snippet` cuts a window of the
text around the matches, and reads the query as `search/query` does:

```clojure
(search/snippet idx (:body (second posts)) "clojure")
;; => [{:text "Rust's ownership rules, seen by someone who mostly writes "}
;;     {:text "Clojure" :match? true}
;;     {:text "."}]
```

For a query that names a field, e.g. `title:clojure`, give the field
that the text is from as `:field`, so that only the words for that
field are marked. To tell whether a text matches a query at all, use
`search/matches?`.

Options
-------

The functions that build an index or read a query take an optional map
of options as their last argument, e.g. `:limit` and `:offset` for a page
of results. The page has the `:total` of the matches in its metadata, for
"1 to 10 of 243", unless a `:filter-pred` filled the page before it read
them all. The options of `search/index` are the defaults of every
query of that index, e.g. the `:boosts` above. The docstrings of
`search/query` and `search.queries/parse` list them all. Every function
takes the same set of options, so you can pass one map to each, and an
option outside it, e.g. `:limt` for `:limit`, throws.

The library doesn't stem, but you can bring your own
[stemmer](https://nlp.stanford.edu/IR-book/html/htmledition/stemming-and-lemmatization-1.html)
as the `:term-fn` of the index, a function of each term that gives the
term to index and to look for in its place. It applies to the words of
the documents, of every query, and of a snippet, so that "posts" finds
"post":

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

A function can't be saved with the index, so give the same `:term-fn`
to `search/restore` and `ciff/read-files` when you read it back. Without
it, the words of the documents you add and of your queries stay as they
are, unlike those already in the index.

Saving an index
---------------

An index prints as EDN, and `search/restore` reads it back:

```clojure
(require '[clojure.edn :as edn])

(def saved (pr-str idx))

(search/restore (edn/read-string saved))
```

For a large index, require `dk.simongray.drop-in-search.ciff` as `ciff`.
Then `ciff/plan` gives you a manifest in EDN, a file for each
[segment](https://lucene.apache.org/core/9_11_1/core/org/apache/lucene/codecs/lucene99/package-summary.html#Segments)
of the index in the
[Common Index File Format](https://github.com/osirrc/ciff), and the names
of the files to delete. A segment's file is named after what it holds, so
a change only adds the segments that are new. You can keep the files in a
folder, in IndexedDB or anywhere else, and `ciff/read-files` reads them
back.

Switch to CIFF once reading the EDN back takes too long. For 20,000
posts of 120 words, measured on an Apple M1 with JDK 24 and Node 24 on
12 October 2026:

| | Size | Write | Read |
|---|---|---|---|
| EDN on the JVM | 45 MB | 0.6 s | 1.9 s |
| CIFF on the JVM | 19 MB | 0.2 s | 0.2 s |
| EDN in Node | 44 MB | 2.2 s | 5.3 s |
| CIFF in Node | 18 MB | 0.7 s | 0.5 s |

Principles
----------

- **Standards first.** The ranking is BM25F as Robertson and Zaragoza
  define it, and the query language takes the words of
  [SQLite FTS5](https://www.sqlite.org/fts5.html#full_text_query_syntax)
  and the signs of
  [Lucene's simple query parser](https://lucene.apache.org/core/9_11_1/queryparser/org/apache/lucene/queryparser/simple/SimpleQueryParser.html).
  Other search engines can read a saved index, and the code cites its
  sources.
- **Fields count together.** A word's matches in each field are weighted
  and added up before they count, so a word that appears once in each of
  three fields counts as much as one that appears three times in one
  field.
- **The same words everywhere.** Words split at anything that isn't a
  letter or a digit. The
  [word segmenters](https://en.wikipedia.org/wiki/Text_segmentation#Word_segmentation)
  of Java and JavaScript split Chinese differently, so the library
  doesn't use them, and an index finds the same words on every device. A
  date counts as its date and time in UTC, e.g. 2024-03-01 09:30:00.
- **No stemming.** The last word of a query completes as a prefix
  instead, and a `:term-fn` brings a stemmer of your own.
- **Typos per word.** By default, only a word that no document holds
  also matches words with typos, rather than every word of a query that
  finds too little, so the words that are spelled right stay exact.
- **Any query is safe.** Nothing a user types is an error, and limits on
  the words, groups, prefixes and typos of a query keep its cost bounded.
- **Convention over configuration.** The operators and the limits have
  sensible defaults in public vars, and options change them, e.g. OG for
  AND in Danish.
- **One codebase.** The library is written in `.cljc`, with no
  dependencies but Clojure.

Versions
--------

Until 1.0, a release that changes the API gets a new minor version, e.g.
from 0.4.2 to 0.5.0, and one that only fixes gets a new patch version,
e.g. from 0.4.2 to 0.4.3. A function that's renamed or removed goes
without a deprecated alias, and the commit of each release says what
changed. The tests run in CI on Java 11 and 25, in Node and in a
headless Chrome.

A CIFF index records the version of its format, and `ciff/read-files`
throws with the `:type` `:dk.simongray.drop-in-search.ciff/newer-format`
for one that's newer than it reads. The EDN of an index has no version,
so index the documents again after a release that changes its shape.

Development
-----------

```bash
clojure -X:test                       # the tests on the JVM
npm install                           # once, for ClojureScript
clojure -M:cljs compile test          # the tests in Node
clojure -M:cljs compile browser-test  # the tests in a browser
node test/browser-run.mjs             # run them in a headless Chrome
```

License
-------

The drop-in-search project is licensed under the [MIT licence](LICENSE).

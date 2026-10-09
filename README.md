drop-in-search
==============

This is a Clojure and ClojureScript library for adding full-text search
to your app, e.g. to search the posts of a blog, the episodes of a
podcast app, or the records of a CMS. It's a search engine without a
server or a UI: you make documents of your own data, and it gives you the
best matches, which you show however you like.

- **Ranking.** It ranks the results by BM25F, and a match in one field
  can count more than a match in another.
- **Queries.** Your users can type AND, OR and NOT, "phrases", prefixes
  and field:word, as in SQLite or Lucene, and a word with a typo still
  finds the words it's close to.
- **Many languages.** Case and diacritics don't count, and Chinese,
  Japanese, Korean and Thai are found without a dictionary.
- **Snippets.** It shows where a text matches a query, for a list of
  results.
- **Saving.** An index prints as EDN, and a large one is saved in the
  Common Index File Format, wherever you keep your documents.

The same code runs on the JVM, in Node and in the browser, and gives the
same results on all of them.

> This library was spun out of [podcast-clj](https://github.com/simongray/podcast-clj),
> a library for making podcast software, where it searches episodes and
> shows. Like podcast-clj, it was developed with assistance from frontier
> LLMs.

Getting started
---------------

Add it to the `:deps` in your `deps.edn` as a Git dependency, with the SHA
of the latest commit on `master` (requires Clojure 1.11+ & Java 11+):

```clojure
io.github.simongray/drop-in-search {:git/sha "…"}
```

For ClojureScript, also set `:deps true` in your `shadow-cljs.edn`, since
shadow-cljs only reads Git dependencies from `deps.edn`.

Then make a document of each of your records. A document has an `:id`,
the `:fields` to search, and what a result gives back, under `:stored`:

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
Rust. A word with a typo finds the words it's close to, and the result
says which:

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
post.

To show where a result matches, `snippet` cuts a window of the text
around the matches. Give it the options of the index, so that it reads
the query as the index does:

```clojure
(require '[dk.simongray.drop-in-search.queries :as search.queries])

(search.queries/snippet (:body (second posts))
                        "clojure"
                        (search/query-opts idx))
;; => [{:text "Rust's ownership rules, seen by someone who mostly writes "}
;;     {:text "Clojure" :match? true}
;;     {:text "."}]
```

Options
-------

The functions take an optional map of options as their last argument,
e.g. `:limit` and `:offset` for a page of results. The options of
`search/index` are the defaults of every query of that index, e.g. the
`:boosts` above. The docstring of `search/query` lists them all.

Saving an index
---------------

An index prints as EDN, and `search/restore` reads it back:

```clojure
(require '[clojure.edn :as edn])

(def saved (pr-str idx))

(search/restore (edn/read-string saved))
```

For a large index, `ciff/plan` gives you a manifest and a document for
each segment of the index, in the
[Common Index File Format](https://github.com/osirrc/ciff), and the names
of the documents to delete. A segment's document is named after what it
holds, so a change only adds the segments that are new. You can keep the
documents in a folder, in IndexedDB or anywhere else, and
`ciff/read-documents` reads them back.

Principles
----------

- **Standards first.** The ranking is BM25F, as Robertson and Zaragoza
  define it, the query language takes the words of SQLite FTS5 and the
  signs of Lucene's simple query parser, and other search engines can read
  a saved index. The code cites its sources.
- **Fields count together.** A word's matches in each field are weighed
  and added up before they count, so a word that appears once in each of
  three fields doesn't outrank one repeated in the title.
- **The same words everywhere.** Words split at anything that isn't a
  letter or a digit. The platforms' word segmenters split Chinese
  differently, so they're left out, and an index finds the same words on
  every device.
- **No language model.** There's no stemming. The last word of a query
  completes as a prefix instead, and a field given as a vector of terms is
  taken as it is, so you can bring your own stemmer.
- **Typos per word.** Only a word that no document holds matches words
  with typos, rather than the whole query when it finds too little, so
  the words that are spelled right stay exact.
- **Any query is safe.** Nothing a user types is an error, and limits on
  the words, groups, prefixes and typos of a query keep its cost bounded.
- **Convention over configuration.** The operators and the limits are
  public vars with sensible defaults, and options change them, e.g. OG
  for AND in Danish.
- **One codebase.** The library is written in `.cljc`, with no
  dependencies but Clojure.

Development
-----------

```bash
clojure -X:test               # the tests on the JVM
npm install                   # once, for the Node tests
clojure -M:cljs compile test  # the tests in Node
```

License
-------

The drop-in-search project is licensed under the [MIT licence](LICENSE).

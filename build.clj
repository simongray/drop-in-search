(ns build
  "Build and release the library:

      clojure -T:build jar       ; target/drop-in-search-<version>.jar with
                                 ; its pom
      clojure -T:build install   ; into the local Maven repository
      clojure -T:build deploy    ; to Clojars, with CLOJARS_USERNAME and
                                 ; CLOJARS_PASSWORD (a deploy token) set"
  (:require [clojure.tools.build.api :as b]))

(def lib
  "The Maven coordinate of the library."
  'dk.simongray/drop-in-search)

(def version
  "The version to build; bump it with each release on GitHub."
  "0.1.0")

(def class-dir
  "Where the jar's contents are staged."
  "target/classes")

(def jar-file
  "The jar to build."
  (format "target/%s-%s.jar" (name lib) version))

(def basis
  "The project basis, computed on first use."
  (delay (b/create-basis {:project "deps.edn"})))

(def scm-url
  "The repository, for the pom."
  "https://github.com/simongray/drop-in-search")

(defn clean
  "Remove everything built."
  [_]
  (b/delete {:path "target"}))

(defn jar
  "Write the pom and build the jar from the sources in src."
  [_]
  (b/delete {:path class-dir})
  (b/write-pom {:class-dir class-dir
                :lib       lib
                :version   version
                :basis     @basis
                :src-dirs  ["src"]
                :scm       {:url                 scm-url
                            :connection          "scm:git:git://github.com/simongray/drop-in-search.git"
                            :developerConnection "scm:git:ssh://git@github.com/simongray/drop-in-search.git"
                            :tag                 (str "v" version)}
                :pom-data  [[:description "A Clojure(Script) library for adding full-text search to your app."]
                            [:url scm-url]
                            [:licenses
                             [:license
                              [:name "MIT License"]
                              [:url "https://opensource.org/license/mit"]]]]})
  (b/copy-dir {:src-dirs   ["src"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))

(defn install
  "Build the jar and install it in the local Maven repository."
  [_]
  (jar nil)
  (b/install {:basis     @basis
              :lib       lib
              :version   version
              :jar-file  jar-file
              :class-dir class-dir}))

(defn deploy
  "Build the jar and deploy it to Clojars."
  [_]
  (jar nil)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   {:installer :remote
    :artifact  (b/resolve-path jar-file)
    :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}))

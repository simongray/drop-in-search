(ns dk.simongray.drop-in-search.browser-runner
  "The runner of the tests in a browser: the report of cljs.test on the
  console, where test/browser-run.mjs reads it from a headless Chrome."
  {:dev/always true}
  (:require [cljs.test :as ct]
            [shadow.test :as st]
            [shadow.test.env :as env]))

(defn ^:export init
  "Run every test of the build, with the report on the console."
  []
  (enable-console-print!)
  (-> (env/get-test-data)
      (env/reset-test-data!))
  (st/run-all-tests (ct/empty-env)))

(ns test.sociabl.alltests
  (:require [sociabl.testing :as t]
	    [test.sociabl.serverconn_test :as t1]
	    [test.sociabl.modelclient_test :as t2]))

(defn -main [&args]
  (t1/test-all)
  (t2/test-all)
  (t/test-recap))

(set! *main-cli-fn* -main)
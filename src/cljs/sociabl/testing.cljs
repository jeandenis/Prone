(ns sociabl.testing)

(def successful-test-count (atom 0))
(def failed-test-count (atom 0))

(defn is?
  [message expected actual]
  (if (= expected actual)
    (do (prn (str "success: " message)) 
	(swap! successful-test-count + 1))
    (do (prn (str "failure: " message " " expected " " actual))
	(swap! failed-test-count + 1))))

(defn test-recap
  []
  (if (> @failed-test-count 0)
      (prn (str @failed-test-count " test(s) failed our of " 
		(+ @failed-test-count @successful-test-count) " tests."))
      (prn (str "All " @successful-test-count 
		" test(s) passed successfully."))))

(defn do-nothing [] nil)
    
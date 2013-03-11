(ns sociabl.logging)

(def sys 
     (try*
      (js* "require && require(\"sys\")")
      (catch e nil)))

(def suppress? (atom false))

(defn suppress
  [b]
  (reset! suppress? b))

(defn error
  [s]
  (if (not @suppress?)
    (.log (js* "console") "error: " s))
  nil)

(defn info
  [s]
  (if (not @suppress?)
    (.log (js* "console") "info: " s))
  nil)

(defn inspect
  [o]
  (if-not (nil? sys)
    (.inspect sys o)
    o))
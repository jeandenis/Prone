(ns sociabl.modelclient
    (:require [goog.string :as gstring]
	      [goog.object :as gobject]
	      [sociabl.logging :as l]))

(declare get-node-)
(declare process-all-queued-messages)
(declare update-model)

;; TODO: replace with clojurescript implementation, if it ever exists
(defn subvec 
  ([v start]
     (vec (drop start v)))
  ([v start end]
     (vec (drop start (take end v)))))

(defn create-model-client
  [uid]
  {:uid uid
   :version 0
   :message-queue []
   :callbacks nil
   :reset-callbacks nil
   :root nil})

(defn valid-key?
  [key]
  (or (string? key)
      (keyword? key)))

(defn valid-node-value?
  [value]
  (or (string? value)
      (number? value)
      (map? value)
      (vector? value)
      (nil? value)
      (true? value)
      (false? value)))


(defn get-node
  [model-client path]
  (get-node- (:root model-client) path))

(defn leaf?
  [node]
  (let [node-type (:type node)]
    (or (= "node" node-type)
	(= "array" node-type)
	(= "channel" node-type))))

(defn mapnode?
  [node]
  (= "map" (:type node)))

(defn get-node-
  [node path]
  (if (or (nil? node) (nil? path))
    nil
    (if (empty? path) 
      (if (leaf? node)
	node
	nil)
      (if (and (mapnode? node) (valid-key? (first path)))
	(get-node- (get (:value node) (first path)) (rest path))
	nil))))

(declare add-callback-)
(declare remove-callback-)
(declare get-all-callbacks-)

 (defn add-reset-callback
   [model-client callback]
   (let [reset-callbacks (if (nil? (:reset-callbacks model-client))
 			  (js* "{}") (:reset-callbacks model-client))]
     (aset reset-callbacks callback callback)
     (assoc model-client :reset-callbacks reset-callbacks)))

(defn remove-reset-callback
  [model-client callback]
  (let [reset-callbacks (:reset-callbacks model-client)]
    (if-not (nil? reset-callbacks)
      (let [reset-callbacks (gobject/clone reset-callbacks)]
	(js* "delete ~{reset-callbacks}[~{callback}]")
	(assoc model-client :reset-callbacks reset-callbacks))
      model-client)))

(defn add-callback
  [model-client path callback]
  (let [with-callback (add-callback- (:callbacks model-client) path callback)]
    (if-not (nil? with-callback)
      (assoc model-client :callbacks with-callback))))

(defn remove-callback
  [model-client path callback]
  (let [without-callback (remove-callback- (:callbacks model-client) 
					   path callback)]
    (if-not (nil? without-callback)
      (assoc model-client :callbacks
	     without-callback))))

(defn get-all-callbacks
  [model-client path]
  (get-all-callbacks- (:callbacks model-client) path))

(defn add-callback-
  [node path callback]
  (if (or (nil? path) (empty? path))
    (if (nil? node) 
      (let [callbacks (js* "{}")]
	(aset callbacks callback callback)
	{:callbacks callbacks
	 :value {}})
      (let [callbacks (gobject/clone (:callbacks node))]
	(aset callbacks callback callback)
	{:callbacks callbacks
	 :value (:value node)}))
    (if (valid-key? (first path))
      (if (nil? node)
	(let [with-callback (add-callback- nil (vec (rest path)) callback)]
	  (if-not (nil? with-callback)
	    {:callbacks (js* "{}")
	     :value {(first path) with-callback}}))
	(let [with-callback 
	      (add-callback- (get (:value node) (first path))
			     (vec (rest path)) callback)]
	  (if-not (nil? with-callback)
	    {:callbacks (:callbacks node)
	     :value (assoc (:value node) (first path) 
			   with-callback)}))))))

(defn remove-callback-
  [node path callback]
  (if (and (or (nil? path) (empty? path)) (not (nil? node)))
    (let [callbacks (gobject/clone (:callbacks node))]
      (js* "delete ~{callbacks}[~{callback}]")
      {:callbacks callbacks
       :value (:value node)})
    (if (valid-key? (first path))
      (if (not (nil? node))
	(let [without-callback
	      (remove-callback- (get (:value node) (first path))
				(vec (rest path)) callback)]
	  (if-not (nil? without-callback)
	    {:callbacks (:callbacks node)
	     :value (assoc (:value node) (first path) 
			   without-callback)}))))))

(defn get-all-callbacks-
  [node path] 
  (if (or (nil? path) (empty? path))
    (vec (gobject/getValues (:callbacks node)))
    (vec (concat (vec (gobject/getValues (:callbacks node)))
		 (get-all-callbacks- (get (:value node) (first path))
				     (rest path))))))
  
(defn handle-model-update-message
  [model-client model-update-message]
  (let [conjed-message-queue 
	(if (nil? (some #(= (:version model-update-message) (:version %)) 
			(:message-queue model-client)))
	  (conj (:message-queue model-client) model-update-message)
	  (:message-queue model-client))
	sorted-message-queue (vec
			      (sort-by #(gstring/toNumber (:version %)) 
				       conjed-message-queue))
	processed-state (process-all-queued-messages 
		(:version model-client) sorted-message-queue 
		(:root model-client) (:callbacks model-client)
		(:reset-callbacks model-client))]
    (if-not (nil? processed-state)
      (let [[version message-queue root] processed-state]
	{:uid (:uid model-client)
	 :version version
	 :message-queue message-queue
	 :root root
	 :reset-callbacks (:reset-callbacks model-client)
	 :callbacks (:callbacks model-client)})
      nil)))

(defn process-all-queued-messages
  [version message-queue root callbacks reset-callbacks]
  (if (empty? message-queue) 
    [version [] root]
    (let [msg (first message-queue)
	  message-version (:version msg)
	  message-type (:type msg)]
      (if (= message-version (+ version 1))
	(cond 
	 (= "ModelUpdateMessage" message-type)
	 (let [old-value (get-node- root (:path msg))
	       updated-root
	       (update-model root (:path msg) (:node-change msg))
	       callbacks-to-execute (get-all-callbacks- callbacks 
							(:path msg))]
	   (if-not (nil? updated-root)
	     (do
	       (doall (map #(try* 
			     (% (:node-change msg) (:path msg) old-value) 
			     (catch e nil)) callbacks-to-execute))
	       (process-all-queued-messages 
		message-version (rest message-queue) updated-root callbacks
		reset-callbacks))
	     nil))
	 (= "ModelResetMessage" message-type)
	 (let [updated-values
	       (process-all-queued-messages message-version (rest message-queue)
					    (:value msg) callbacks 
					    reset-callbacks)]
	   (if (and (not (nil? updated-values))
		    (not (nil? reset-callbacks)))
	     (doall (map #(try* (% (nth updated-values 2))
				(catch e nil))
			 (vec (gobject/getValues reset-callbacks)))))
	   updated-values)
	 :else nil) 
	(if (> message-version version) 
	  [version message-queue root]
	  nil)))))

(defn array-node-change?
  [node-change]
  (let [type (:type node-change)]
    (or (= "arrayi" type)
	(= "arrayo" type)
	(= "arrayd" type))))

(defn update-model
  [node path node-change]
  (let [node-change-type (:type node-change)]
    (if (and (empty? path) (not (nil? node-change)) )
      (cond 
       (and (= "node" node-change-type)
	    (contains? node-change :value node-change)
	    (valid-node-value? (:value node-change)))
       (if (nil? node)
	 {:type "node" :value (:value node-change)}
	 (if (= "node" (:type node))
	   {:type "node" :value (:value node-change)}
	   nil))
       (and (= "channel" node-change-type)
	    (contains? node-change :value node-change)
	    (valid-node-value? (:value node-change)))
       (if (nil? node)
	 {:type "channel" :value [(:value node-change)] :max 100}
	 (if (= "channel" (:type node))
	   {:type "channel" 
	    :value (vec (take-last (:max node) 
				   (conj (:value node) (:value node-change))))
	    :max (:max node)}
	   nil))
       (array-node-change? node-change)
       (if (nil? node) ;; TODO add check for all needed elements of node-change
	 (if (and (= 0 (:index node-change)) (= "arrayi" node-change-type)
		  (contains? node-change :value node-change)
		  (valid-node-value? (:value node-change)))
	   {:type "array" :value [(:value node-change)]}
	   nil)
	 (if (= "array" (:type node))
	   (let [array-index (:index node-change)
		 old-array (:value node)]
	     (if (and (not (nil? array-index)) (>= array-index 0))
	       (cond
		(and (= "arrayi" node-change-type)
		     (<= array-index (count old-array))
		     (contains? node-change :value node-change)
		     (valid-node-value? (:value node-change)))
		{:type "array" :value
		 (concat  (subvec old-array 0 array-index) 
			  [(:value node-change)]
			  (subvec old-array array-index))}
		(and (= "arrayo" node-change-type)
		     (< array-index (count old-array))
		     (contains? node-change :value node-change)
		     (valid-node-value? (:value node-change)))
		{:type "array" :value
		 (concat (subvec old-array 0 array-index)
			 [(:value node-change)]
			 (subvec old-array (+ array-index 1)))}
		(and (= "arrayd" node-change-type)
		     (< array-index (count old-array)))
		{:type "array" :value
		 (concat (subvec old-array 0 array-index)
			 (subvec old-array (+ array-index 1)))}
		:else nil)
	       nil))
	   nil))
       (= "mapd" node-change-type)
       ;; TODO add check for all needed elements of node-change
       (if (and (not (nil? node)) (mapnode? node))
	 (assoc node :value (dissoc (:value node) (:value node-change)))
	 nil)
       :else nil)
      (if (and (not (empty? node-change))
	       (not (nil? node-change)))
	(let [current-path (first path)]
	  (if (valid-key? current-path)
	    (if (nil? node)
	      (let [updated-root
		    (update-model nil (vec (rest path)) node-change)]
		(if-not (nil? updated-root)
		  {:type "map" :value {current-path updated-root}}))
	      (if (mapnode? node)
		(let [updated-root
		      (update-model (get (:value node) current-path)
				    (vec (rest path)) node-change)]
		  (if-not (nil? updated-root)
		    {:type "map" :value (assoc (:value node) current-path
					      updated-root)}))
		nil) nil) nil)) nil) nil)))

(defn create-manager
  [on-create-model-client]
  (let [model-clients (atom {})]
    {:model-clients model-clients
     :on-create-model-client on-create-model-client}))

(defn fix-paths
  [message]
  (if (contains? message :path)
    (assoc message 
      :path (doall (vec (map (fn [string-or-keyword]
				  (if (string? string-or-keyword)
				    (keyword string-or-keyword)
				    string-or-keyword)) (:path message)))))
    message))

(defn handle-unwrapped-message
  [manager message]
  (let [message (fix-paths message)]
    (if-not (nil? (:uid message))
      (let [model-clients (:model-clients manager)
	    model-client (get @model-clients (:uid message))
	    model-client 
	    (if-not (nil? model-client) model-client
		    (do
		      (let [model-client (create-model-client (:uid message))]
			(if (and (= "ModelResetMessage" (:type message))
				 (not (nil? (get-in message [:value :value
							     :class :value]))))
			  ((:on-create-model-client manager) model-client
			   (get-in message [:value :value :class :value]))
			  model-client))))]
	(let [new-model-client (handle-model-update-message
				model-client message)]
	  (if-not (nil? new-model-client)
	    (assoc manager :model-clients
		   (reset! model-clients (assoc @model-clients (:uid message)
						new-model-client)))
	    (l/error (str "model update failed: "
			  model-client message)))))
      (l/error (str "message has no uid: " message)))))

(defn get-model-client
  [manager uid]
  (get (deref (:model-clients manager)) uid))

(ns test.sociabl.modelclient_test
 (:require [sociabl.testing :as t]
	   [sociabl.logging :as l]
	   [sociabl.modelclient :as sc]))

(defn test-subvec
  []
  (t/is? "subvec from start" [1 2 3] (sc/subvec [1 2 3] 0))
  (t/is? "subvec from before start" [1 2 3] (sc/subvec [1 2 3] -1))
  (t/is? "subvec from way before start" [1 2 3] (sc/subvec [1 2 3] -1000))
  (t/is? "subvec from end" [3] (sc/subvec [1 2 3] 2))
  (t/is? "subvec from beyond end" [] (sc/subvec [1 2 3] 3))
  (t/is? "subvec from way beyond end" [] (sc/subvec [1 2 3] 1000))
  (t/is? "subvec in middle" [2 3 4] (sc/subvec [1 2 3 4 5] 1 4))
  (t/is? "subvec in middle 2" [2 3] (sc/subvec [1 2 3 4 5] 1 3))
  (t/is? "subvec in middle with extreme start" 
	 [1 2 3] (sc/subvec [1 2 3 4 5] 0 3))
  (t/is? "subvec in middle with extreme start 2" 
	 [1 2 3] (sc/subvec [1 2 3 4 5] -1 3))
  (t/is? "subvec in middle with extreme start 3" 
	 [1 2 3] (sc/subvec [1 2 3 4 5] -1000 3))
  (t/is? "subvec in middle with extreme start" 
	 [3 4 5] (sc/subvec [1 2 3 4 5] 2 5))
  (t/is? "subvec in middle with extreme start" 
	 [3 4 5] (sc/subvec [1 2 3 4 5] 2 6))
  (t/is? "subvec in middle with extreme start" 
	 [3 4 5] (sc/subvec [1 2 3 4 5] 2 1000))
  (t/is? "subvec with start after end" 
	 [] (sc/subvec [1 2 3 4 5] 3 1)))

(defn test-reset-and-message-queueing
  []
  (let [mc (sc/create-model-client)
	updated-mc (sc/handle-model-update-message 
		    mc {:version 1 :type "ModelResetMessage"
			:value {:type "node" :value "a"}})
	failed-updated-mc (sc/handle-model-update-message 
			   updated-mc {:version 1 :type "ModelResetMessage"
				       :value {:type "node" :value "b"}})
	queued-updated-mc (sc/handle-model-update-message 
			   updated-mc {:version 4 :type "ModelResetMessage"
				       :value {:type "node" :value "e"}})
	repeat-message-mc (sc/handle-model-update-message 
			   queued-updated-mc 
			   {:version 4 :type "ModelResetMessage"
			    :value {:type "node" :value "f"}})
	updated-mc-2 (sc/handle-model-update-message 
		      repeat-message-mc {:version 2 :type "ModelResetMessage"
					 :value {:type "node" :value "c"}})
	queued-updated-mc-2 (sc/handle-model-update-message 
			     updated-mc-2 {:version 3 :type "ModelResetMessage"
					   :value {:type "node" :value "d"}})]
    (t/is? "reset model normal (version)" 1 (:version updated-mc))
    (t/is? "reset model normal (data)" 
	   {:type "node", :value "a"} (:root updated-mc))
    (t/is? "reset model but wrong message version" nil failed-updated-mc)
    (t/is? "reset model queued future update (version)" 
	   1 (:version queued-updated-mc))
    (t/is? "reset model queued future update (data)" 
	   {:type "node", :value "a"} (:root queued-updated-mc))
    (t/is? "reset model normal 2 (version)" 
	   2 (:version updated-mc-2))
    (t/is? "reset model normal 2 (data)"
	   {:type "node", :value "c"} (:root updated-mc-2))
    (t/is? "reset model process queued messages, ignored dup msg (version)" 
	   4 (:version queued-updated-mc-2))
    (t/is? "reset model process queued messages, ignored dup msg (data)"
	   {:type "node", :value "e"} (:root queued-updated-mc-2))))

(defn test-update-node
  []
  (let [mc (sc/create-model-client)
	updated-mc (sc/handle-model-update-message 
		    mc {:version 1 :type "ModelUpdateMessage" 
			:node-change {:type "node"
				       :value "a"}})
	updated-mc- (sc/handle-model-update-message
		     mc {:version 1 :type "ModelUpdateMessage" 
			 :node-change {:type "node"}})
	updated-mc-2 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage"
				  :node-change {:type "node"
						 :value 77}})
	updated-mc-3 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage"
				  :node-change {:type "node"
						 :value {:foo "bar"}}})
	updated-mc-4 (sc/handle-model-update-message
		      mc {:version 1 :type "ModelUpdateMessage"
			  :node-change {:type "node"
					 :value [1 "b" 3 true false]}})
	updated-mc-5 (sc/handle-model-update-message
		      mc {:version 1 :type "ModelUpdateMessage"
			  :node-change {:type "node"
					 :value true}})
	updated-mc-6 (sc/handle-model-update-message
		      mc {:version 1 :type "ModelUpdateMessage"
			  :node-change {:type "node"
					 :value false}})
	updated-mc-7 (sc/handle-model-update-message
		      mc {:version 1 :type "ModelUpdateMessage"
			  :node-change {:type "node"
					 :value nil}})
	mc2 (sc/create-model-client)
	updated-mc2 (sc/handle-model-update-message 
		     mc2 {:version 1 :type "ModelUpdateMessage" 
			  :node-change {:type
					 "channel"
					 :value "a"}})
	failed-update-1 (sc/handle-model-update-message
			 updated-mc2 {:version 2 :type "ModelUpdateMessage"
				      :node-change {:type "node"
						     :value (js* "{}")}})
	failed-update-2 (sc/handle-model-update-message
			 updated-mc2 {:version 2 :type "ModelUpdateMessage"
				      :node-change {:type "node"
						     :value (js* "[]")}})
	failed-update-3 (sc/handle-model-update-message
			 updated-mc2 {:version 2 :type "ModelUpdateMessage"
				       :node-change {:type "node"
						     :value "c"}})]
    (t/is? "update root node for an empty model"
	   {:type "node" :value "a"} (:root updated-mc))
    (t/is? "update node but no value in update"
	   nil (:root updated-mc-))
    (t/is? "update root node for an existing model with int"
	   {:type "node" :value 77} (:root updated-mc-2))
    (t/is? "update root node for an existing model with map"
	   {:type "node" :value {:foo "bar"}} (:root updated-mc-3))
    (t/is? "update root node for an existing model with array"
	   {:type "node" :value [1 "b" 3 true false]} (:root updated-mc-4))
    (t/is? "update root node for an existing model with true"
	   {:type "node" :value true} (:root updated-mc-5))
    (t/is? "update root node for an existing model with false"
	   {:type "node" :value false} (:root updated-mc-6))
    (t/is? "update root node for an existing model with nil"
	   {:type "node" :value nil} (:root updated-mc-7))
    (t/is? "no update because existing node was a java object"
	   nil failed-update-1)
    (t/is? "no update because existing node was a java array"
	   nil failed-update-2)
    (t/is? "no update because existing node was not a node"
	   nil failed-update-3)))

(defn test-channel-node
  []
  (let [mc (sc/create-model-client)
	updated-mc (sc/handle-model-update-message 
		    mc {:version 1 :type "ModelUpdateMessage" 
			:node-change {:type "channel"
				      :value "a"}})
	updated-mc- (sc/handle-model-update-message 
		    mc {:version 1 :type "ModelUpdateMessage" 
			:node-change {:type "channel"}})
	updated-mc-2 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage"
				  :node-change {:type "channel"
						:value 2}})
	mc2 (sc/create-model-client)
	mc2-updated (sc/handle-model-update-message 
		     mc2 {:version 1 :type "ModelResetMessage"
			  :value {:type "channel"
				  :max 100
				  :value 
				  (vec (concat ["x"] (repeat 100 {})))}})
	updated-mc2 (sc/handle-model-update-message
		     mc2-updated {:version 2 :type "ModelUpdateMessage"
				  :node-change {:type "channel"
						:value 2}})
	]
    (t/is? "update root channel for an empty model"
	   {:type "channel" :value ["a"], :max 100} (:root updated-mc))
    (t/is? "update channel but no value in update"
	   nil (:root updated-mc-))
    (t/is? "updated root channel for an existing model"
	   {:type "channel" :value ["a" 2], :max 100} (:root updated-mc-2))
    (t/is? "updated channel past max size and got rid of old items"
	   {:type "channel" :max 100
	    :value (vec (concat (vec (repeat 99 {})) [2]))} 
	   (:root updated-mc2))))

(defn test-array-node
  []
  (let [mc (sc/create-model-client)
	updated-mc (sc/handle-model-update-message 
		    mc {:version 1 :type "ModelUpdateMessage" 
			:node-change {:type "arrayi"
				      :index 0
				      :value "a"}})
	updated-mc-arrayi-no-index (sc/handle-model-update-message
				    mc {:version 1 :type "ModelUpdateMessage" 
					:node-change {:type "arrayi"
						      :value "a"}})
	updated-mc-arrayi-no-value (sc/handle-model-update-message
				    mc {:version 1 :type "ModelUpdateMessage" 
					:node-change {:type "arrayi"
						      :index 0}})
	updated-mc-2 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage"
				  :node-change {:type "arrayi"
						:index 1
						:value 1}})
	updated-mc-2- (sc/handle-model-update-message
		       updated-mc {:version 2 :type "ModelUpdateMessage"
				   :node-change {:type "arrayi"
						 :index 0
						 :value 1}})
	updated-mc-2-- (sc/handle-model-update-message
			updated-mc-2 {:version 3 :type "ModelUpdateMessage"
				      :node-change {:type "arrayi"
						    :index 1
						    :value "x"}})
	updated-mc-3 (sc/handle-model-update-message 
		      mc {:version 1 :type "ModelUpdateMessage" 
			  :node-change {:type "arrayo"
					:index 0
					:value "a"}})
	updated-mc-4 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage"
				  :node-change {:type "arrayi"
						:index -1
						:value 1}})
	updated-mc-5 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage"
				  :node-change {:type "arrayi"
						:index 2
						:value 1}})
	updated-mc-6 (sc/handle-model-update-message
		     updated-mc-2-- {:version 4 :type "ModelUpdateMessage"
				   :node-change {:type "arrayo"
						 :index 0 
						 :value "c"}})
	updated-mc-arrayo-no-index (sc/handle-model-update-message
				    updated-mc-2-- 
				    {:version 4 :type "ModelUpdateMessage"
				     :node-change {:type "arrayo"
						   :value "c"}})
	updated-mc-arrayo-no-value (sc/handle-model-update-message
				    updated-mc-2-- 
				    {:version 4 :type "ModelUpdateMessage"
				     :node-change {:type "arrayo"
						   :index 0}})
	updated-mc-7 (sc/handle-model-update-message
		      updated-mc-2-- {:version 4 :type "ModelUpdateMessage"
				    :node-change {:type "arrayo"
						  :index 1
						  :value "c"}})
	updated-mc-8 (sc/handle-model-update-message
		      updated-mc-7 {:version 2 :type "ModelUpdateMessage"
				    :node-change {:type "arrayo"
						  :index 3
						  :value "y"}})
	updated-mc-9 (sc/handle-model-update-message
		      updated-mc-2-- {:version 4 :type "ModelUpdateMessage"
				      :node-change {:type "arrayd"
						    :index 0}})
	updated-mc-arrayd-no-index (sc/handle-model-update-message
				    updated-mc-2-- 
				    {:version 4 :type "ModelUpdateMessage"
				     :node-change {:type "arrayd"}})
	updated-mc-10 (sc/handle-model-update-message
		      updated-mc-2-- {:version 4 :type "ModelUpdateMessage"
				      :node-change {:type "arrayd"
						    :index 1}})
	updated-mc-11 (sc/handle-model-update-message
		       updated-mc-9 {:version 2 :type "ModelUpdateMessage"
				     :node-change {:type "arrayd"
						   :index 2}})
	]
    (t/is? "updated root array for an empty model"
	   {:type "array" :value ["a"]} (:root updated-mc))
    (t/is? "updated arrayi but no index" nil updated-mc-arrayi-no-index)
    (t/is? "updated arrayi but no value" nil updated-mc-arrayi-no-value)
    (t/is? "updated root array for an existing model"
	   {:type "array" :value ["a" 1]} (:root updated-mc-2))
    (t/is? "updated root array for an existing model 2"
	   {:type "array" :value [1 "a"]} (:root updated-mc-2-))
    (t/is? "updated root array for an existing model 3"
	   {:type "array" :value ["a" "x" 1]} (:root updated-mc-2--))
    (t/is? "can't update root with overwrite" nil updated-mc-3)
    (t/is? "can't update array with index less than zero"
	   nil updated-mc-4)
    (t/is? "can't update root with array insert past size" nil updated-mc-5)
    (t/is? "overwrote array element"
	   {:type "array" :value ["c" "x" 1]} (:root updated-mc-6))
    (t/is? "updated arrayo but no index" nil updated-mc-arrayo-no-index)
    (t/is? "updated arrayo but no value" nil updated-mc-arrayo-no-value)
    (t/is? "overwrote array element 2"
	   {:type "array" :value ["a" "c" 1]} (:root updated-mc-7))	
    (t/is? "can't update root with array overwrite past size" nil updated-mc-8)
    (t/is? "array delete" 
	   {:type "array" :value ["x" 1]} (:root updated-mc-9))	
    (t/is? "array delete no index" nil updated-mc-arrayd-no-index)
    (t/is? "array delete 2" 
	   {:type "array" :value ["a" 1]} (:root updated-mc-10))	
    (t/is? "can't update root with array delete past size" nil updated-mc-11)))

(defn test-map-node
  []
  (let [mc (sc/create-model-client)
	updated-mc (sc/handle-model-update-message 
		    mc {:version 1 :type "ModelUpdateMessage" :path ["a" "b" "c"]
			:node-change {:type "node"
				      :value "x"}})
	updated-mc- (sc/handle-model-update-message
		     updated-mc {:version 2 :type "ModelUpdateMessage" :path []
				 :node-change {:type "mapd" :value "a"}})
	updated-mc-2 (sc/handle-model-update-message
		      updated-mc- {:version 3 :type "ModelUpdateMessage" :path ["a"]
				  :node-change {:type "node" :value "z"}})
	updated-mc-3 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage" :path ["a" "d"]
				  :node-change {:type "node" :value "y"}})
	updated-mc-4 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage" :path ["a" "b" "c"]
				  :node-change {:type "mapd" :value "z"}})
	updated-mc-5 (sc/handle-model-update-message
		      updated-mc {:version 2 :type "ModelUpdateMessage" :path ["a" {} "c"]
				  :node-change {:type "node" :value "h"}})]
    (t/is? "updated root map for an empty model"
	  {:type "map" :value 
	   {"a" {:type "map" :value 
		 {"b" {:type "map" :value 
		       {"c" {:type "node" :value "x"}}}}}}} (:root updated-mc))
    (t/is? "updated map for an existing model after deleting map entry"
	   {:type "map" :value {"a" {:type "node" :value "z"}}} 
	   (:root updated-mc-2))
    (t/is? "updated map for an existing model 2"
	   {:type "map" :value 
	    {"a" {:type "map" :value 
		  {"b" {:type "map" :value 
			{"c" {:type "node" :value "x"}}}
		   "d" {:type "node" :value "y"}}}}}
	   (:root updated-mc-3))
    (t/is? "can't delete a map entry for a non-map node" nil updated-mc-4)
    (t/is? "cant update node with object in path" nil updated-mc-5)))

(defn test-get
  []
    (let [mc (sc/create-model-client)
	updated-mc 
	  (sc/handle-model-update-message 
	   mc 
	   {:version 1 :type "ModelResetMessage"
	    :value {:type "map" :value 
		    {"a" {:type "map" :value 
			  {"b" {:type "map" :value 
				{"c" {:type "node" :value "x"}}}
			   "d" {:type "array" :value ["a" "b" "c"]}
			   "e" {:type "channel" 
				:max 100 :value ["x" "y" "z"]}}}}}})]
      (t/is? "get node" {:type "node" :value "x"} 
	     (sc/get-node updated-mc ["a" "b" "c"]))
      (t/is? "get array" {:type "array" :value ["a" "b" "c"]}
	     (sc/get-node updated-mc ["a" "d"]))
      (t/is? "get channel" {:type "channel" :max 100 :value ["x" "y" "z"]}
	     (sc/get-node updated-mc ["a" "e"]))
      (t/is? "get node but non-leaf node" nil (sc/get-node updated-mc ["a"]))
      (t/is? "get node but non-existing path" nil 
	     (sc/get-node updated-mc ["a" "b" "c" "d"]))
      (t/is? "get node but non-existing path 2" nil 
	     (sc/get-node updated-mc ["z"]))))

(defn test-unknown-message-type
  [] 
 (let [mc (sc/create-model-client)
       updated-mc (sc/handle-model-update-message 
		    mc {:version 1 :type :unknown})]
   (t/is? "handle unknown message type" nil updated-mc)))

(defn test-add-remove-callbacks
  []
  (let [result (atom 0)
	mc (sc/create-model-client)
	callback #(swap! result + 1)
	callback2 #(swap! result + 256)
	callback3 #(swap! result + 4096)
	callback4 #(swap! result + 64)
	callback5 #(swap! result + 4)
	cb-1 (sc/get-all-callbacks mc ["a" "b" "c"])
	mc-2 (sc/add-callback mc ["a" "b" "c"] callback)
	cb-2 (sc/get-all-callbacks mc-2 ["a" "b" "c"])
	mc-3 (sc/remove-callback mc-2 ["a" "b" "c"] callback)
	cb-3 (sc/get-all-callbacks mc-3 ["a" "b" "c"])
	mc-4 (sc/add-callback mc-2 ["a" "b"] callback2)
	cb-4 (sc/get-all-callbacks mc-4 ["a" "b" "c"])
	cb-5 (sc/get-all-callbacks mc-4 ["a" "b"])
	cb-6 (sc/get-all-callbacks mc-4 ["a"])
	mc-5 (sc/add-callback mc-4 ["a" "b"] callback3)
	cb-7 (sc/get-all-callbacks mc-5 ["a" "b"])
	mc-5 (sc/add-callback mc-5 ["a" "b" "d"] callback4)
	cb-8 (sc/get-all-callbacks mc-5 ["a" "b" "d"])
	mc-6 (sc/remove-callback mc-5 ["a" "b"] callback3)
	cb-9 (sc/get-all-callbacks mc-6 ["a" "b" "d"])
	mc-7 (sc/remove-callback mc-5 ["a" "b"] callback5)
	cb-10 (sc/get-all-callbacks mc-7 ["a" "b" "d"])
	mc-8 (let [mc-tmp (sc/remove-callback mc-2 ["z"] callback)]
	       (if (nil? mc-tmp) mc-2))
	cb-11 (sc/get-all-callbacks mc-8 ["a" "b" "c"])
	mc-9 (sc/add-callback mc ["a" {}] callback)
	mc-10 (sc/remove-callback mc ["a" {}] callback)
	]
    (doall (map #(%) cb-1))
    (t/is? "no callbacks right after model client creation" 0 @result)
    (doall (map #(%) cb-2))
    (t/is? "call callback that matches exactly" 1 @result)
    (doall (map #(%) cb-3))
    (t/is? "no callbacks after remove callback" 1 @result)
    (doall (map #(%) cb-4))
    (t/is? "call multiple callbacks" 258 @result)
    (doall (map #(%) cb-5))
    (t/is? "ignore callbacks that are too specific" 514 @result)
    (doall (map #(%) cb-6))
    (t/is? "ignore all callbacks that are too specific" 514 @result)
    (doall (map #(%) cb-7))
    (t/is? "call multiple callbacks at the same level" 4866 @result)
    (doall (map #(%) cb-8))
    (t/is? "call callback for new branch" 9282 @result)
    (doall (map #(%) cb-9))
    (t/is? "remove callback higher in hierarchy" 9602 @result)
    (doall (map #(%) cb-10))
    (t/is? "remove callback that does not exist" 14018 @result)
    (doall (map #(%) cb-11))
    (t/is? "remove callback with bad path" 14019 @result)
    (t/is? "add callback fails with non-string/number keys" nil mc-9)
    (t/is? "remove callback fails with non-string/number keys" nil mc-10)
    (t/is? (str "get all callbacks returns an empty vector "
		"with non-string/number path") [] (sc/get-all-callbacks 
						   mc ["a" {}]))))

(defn test-callbacks
  []
  (let [result (atom 0)
	callback (fn [node-change path old-value]
		   (if (and (= {:type "node" :value "x"} node-change)
			    (= ["a" "b" "c"] path)
			    (nil? old-value))
		     (swap! result + 1)
		     (if (and (= {:type "node" :value "z"} node-change)
			      (= ["a" "b" "c"] path)
			      (= {:type "node" :value "x"} old-value))
		       (swap! result + 4))))
	callback- (fn [node-change path old-value]
		    (if (and (= {:type "node" :value "z"} node-change)
			     (= ["a" "b" "c"] path)
			     (= {:type "node" :value "x"} old-value))
		      (swap! result + 1024)))
	callback2 (fn [node-change path old-value]
		    (if (and (= {:type "node" :value "y"} node-change)
			     (= ["a" "b" "d"] path)
			     (nil? old-value))
		      (swap! result + 16)))
	mc (sc/create-model-client)
	mc (sc/add-callback mc ["a"] callback)
	mc (sc/handle-model-update-message 
		    mc {:version 1 :type "ModelUpdateMessage" :path ["a" "b" "c"]
			:node-change {:type "node"
				      :value "x"}})]
    (t/is? "successfully called callback with correct value on initial insert"
	   1 @result)
    (let [mc (sc/remove-callback mc ["a"] callback)
	  mc (sc/add-callback mc ["a" "b" "d"] callback2)
	  mc (sc/handle-model-update-message 
	      mc {:version 2 :type "ModelUpdateMessage" :path ["a" "b" "d"]
		  :node-change {:type "node"
				:value "y"}})]
      (t/is? "successfully called callback on existing model"
	     17 @result))
    (let [mc (sc/add-callback mc ["a"] callback-)
	  mc (sc/handle-model-update-message
	      mc {:version 2 :type "ModelUpdateMessage" :path ["a" "b" "c"]
		  :node-change {:type "node" :value "z"}})]
      (t/is? "succesfully updated existing model and callback got old value"
	     1045 @result))))

(defn test-manager
  []
  (let [manager (sc/create-manager)]
    (l/suppress true)
    (sc/handle-unwrapped-message manager {:version 1 :type "ModelResetMessage"
					  :value {:type "node" :value "a"}})
    (l/suppress false)
    (t/is? "manager: message has no uid" nil (sc/get-model-client manager nil))
    (sc/handle-unwrapped-message manager {:uid 7899 :version 1 :type "ModelResetMessage"
					  :value {:type "node" :value "a"}})
    (t/is? "manager: successful update" 
	   {:uid 7899 :version 1 :message-queue []
	    :root {:type "node" :value "a"} :callbacks nil
	    :reset-callbacks nil}
	   (sc/get-model-client manager 7899))))

(defn test-reset-callback
  []
  (let [result (atom 0)
	mc (sc/create-model-client)
	reset-callback1 (fn [mc-]
			  (swap! result + 1))
	mc- (sc/add-reset-callback mc reset-callback1)
	updated-mc (sc/handle-model-update-message 
		    mc- {:version 1 :type "ModelResetMessage"
			:value {:type "node" :value "a"}})]
    (t/is? "reset callback: version correct" 1 (:version updated-mc))
    (t/is? "reset callback: message went through" 
	   {:type "node", :value "a"} (:root updated-mc))
    (t/is? "reset callback: callback invoked" 1 @result)
    (let [updated-mc-2 (sc/handle-model-update-message
			updated-mc {:version 2 :type "ModelResetMessage"
				    :value {:type "node" :value "b"}})]
      (t/is? "reset callback: version correct (2)" 2 (:version updated-mc-2))
      (t/is? "reset callback: message went through (2)" 
	     {:type "node", :value "b"} (:root updated-mc-2))
      (t/is? "reset callback: callback invoked (2)" 2 @result)
      (let [_ (sc/handle-model-update-message
			updated-mc-2 {:version 2 :type "ModelResetMessage"
				    :value {:type "node" :value "c"}})]
	      (t/is? (str "reset callback: reset-callback not called "
			  "on failed reset") 2 @result)))
    (let [reset-callback2 (fn [mc-]
			    (swap! result + 100))
	  mc-- (sc/add-reset-callback updated-mc reset-callback2)
	  updated-mc-3 (sc/handle-model-update-message
			updated-mc {:version 2 :type "ModelResetMessage"
				    :value {:type "node" :value "z"}})]
      (t/is? "reset callback (mult cb): version correct" 
	     2 (:version updated-mc-3))
      (t/is? "reset callback (mult cb): message went through" 
	     {:type "node", :value "z"} (:root updated-mc-3))
      (t/is? "reset callback (mult cb): callbacks invoked" 103 @result)
      (let [mc--- (sc/remove-reset-callback updated-mc-3 
					    reset-callback1)
	    updated-mc-4 (sc/handle-model-update-message
			  mc--- {:version 3 :type "ModelResetMessage"
				      :value {:type "node" :value "y"}})]
	(t/is? "reset callback (remove cb): version correct" 
	       3 (:version updated-mc-4))
	(t/is? "reset callback (remove cb): message went through" 
	       {:type "node", :value "y"} (:root updated-mc-4))
	(t/is? "reset callback (remove cb): callbacks invoked" 203 @result)))))

(defn test-all
  []
  (test-subvec)
  (test-reset-and-message-queueing)
  (test-update-node)
  (test-channel-node)
  (test-array-node)
  (test-map-node)
  (test-get)
  (test-unknown-message-type)
  (test-add-remove-callbacks)
  (test-callbacks)
  (test-manager)
  (test-reset-callback))
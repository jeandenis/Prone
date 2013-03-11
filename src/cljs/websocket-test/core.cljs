(ns websocket-test.core
  (:require [sociabl.webclient :as wc]
	    [sociabl.modelclient :as mc]
	    [sociabl.serverconn :as sc]
	    [sociabl.logging :as l]))

(def jq (fn [] window.jQuery))

(def sociabl-global (atom nil))

(def gnumber (atom 0))
(def gword (atom ""))

(declare run-tests run-tests-2)

(defn on-ready
  []
  (-> ((jq) "a#increment")
      (.click 
       (fn []
	 (let [server-connection (:server-connection @sociabl-global)
	       msg {:to (:recipient-id @server-connection)
		    :datatype "Sociabl.Parsers.WebsocketTestMessage"
			   :msg {:type "IncrementNumber"}}]
	   (sc/send 
	    server-connection msg 
	    (fn [rsp] 
	      (if (= rsp {:datatype "Sociabl.Actor.ActorDefaultResponse"
			  :rsp {:type "SuccessResponse"}})
		(-> ((jq) "body")
		    (.append 
		     (str "<div>success: increment number<div>")))
		(-> ((jq) "body")
		    (.append 
		     (str "<div>failure: "
			  "increment number</div>")))))))
	 false)))
  (-> ((jq) "a#resetnumber")
      (.click 
       (fn []
	 (let [server-connection (:server-connection @sociabl-global)
	       msg {:to (:recipient-id @server-connection)
		    :datatype "Sociabl.Parsers.WebsocketTestMessage"
		    :msg {:type "ResetNumber"
			  :value -5}}]
	   (sc/send 
	    server-connection msg 
	    (fn [rsp] 
	      (if (= rsp {:datatype (str "Sociabl.Parsers.WebsocketTest"
					 "Message.WebsocketTestResponse")
			  :rsp {:type "SuccessNumberResponse" :value -5}})
		(-> ((jq) "body")
		    (.append 
		     (str "<div>success: reset number<div>")))
		(-> ((jq) "body")
		    (.append 
		     (str "<div>failure: "
			  "reset number</div>")))))))
	 false)))
  (-> ((jq) "a#sendmessage")
      (.click
       (fn []
	 (let [server-connection (:server-connection @sociabl-global)
	       msg {:to (:recipient-id @server-connection)
		    :datatype "Sociabl.Parsers.WebsocketTestMessage"
		    :msg {:type "SendMessage"
			  :value "hello, કખગ"}}]
	   (sc/send 
	    server-connection msg))
	 false)))
  (run-tests))

(defn run-tests
  []
  (let [increment ((jq) "a#increment")
	send-message ((jq) "a#sendmessage")]
    (js* "~{increment}.click()")
    (js* "~{increment}.click()")
    (js* "~{increment}.click()")
    (js* "~{increment}.click()")
    (js* "~{increment}.click()")
    ((js* "setTimeout") 
     (fn [] 
       (if (= 5 @gnumber)
	 (do (-> ((jq) "div#testResults")
		 (.append "<div>First test passed.</div>"))
	     (js* "~{send-message}.click()")
	     ((js* "setTimeout") 
	      (fn [] 
		(if (= "hello, કખગ, how are you?" @gword)
		  (do (-> ((jq) "div#testResults")
			  (.append "<div>Second test passed.</div>"))
		      (run-tests-2))
		  (-> ((jq) "div#testResults")
		      (.append "<div>Second test FAILED!</div>")))) 400))
	 (-> ((jq) "div#testResults")
	     (.append "<div>First test FAILED!</div>")))
       ) 500)))

(defn run-tests-2
  []
  (let [increment ((jq) "a#increment")
	send-message ((jq) "a#sendmessage")
	reset-number ((jq) "a#resetnumber")]
    (js* "~{reset-number}.click()")
    (js* "~{increment}.click()")
    ((js* "setTimeout") 
     (fn [] 
       (if (= -4 @gnumber)
	 (do (-> ((jq) "div#testResults")
		 (.append "<div>Third test passed.</div>"))
	     (js* "~{send-message}.click()")
	     ((js* "setTimeout") 
	      (fn [] 
		(if (= "hello, કખગ, how are you?" @gword)
		  (do (-> ((jq) "div#testResults")
			  (.append "<div>Fourth test passed.</div>"))
		      (js* "~{reset-number}.click()")
		      (doall (repeatedly 25 #(js* "~{increment}.click()")))
		      ((js* "setTimeout") 
		       (fn [] 
			 (if (= 20 @gnumber)
			   (-> ((jq) "div#testResults")
			       (.append "<div>Fifth test passed.</div>"))
			   (-> ((jq) "div#testResults")
			       (.append "<div>Fifth test FAILED!!</div>"))))
		       500))
		  (-> ((jq) "div#testResults")
		      (.append "<div>Fourth test FAILED!</div>")))) 500))
	 (-> ((jq) "div#testResults")
	     (.append "<div>Third test FAILED!!</div>")))) 500)))
		  
(defn update-word
  [word-node]
  (reset! gword (:value word-node))
  (-> ((jq) "body")
      (.append (str "<div>word: " (:value word-node) "</div>"))))

(defn update-number
  [number-node]
  (reset! gnumber (:value number-node))
  (-> ((jq) "body")
      (.append (str "<div>number: " (:value number-node) "</div>"))))

(defn reset-word-number
  [root]
  (update-word (mc/get-node- root [:word]))
  (update-number (mc/get-node- root [:number])))

(defn on-init-test-actor
  [model-client]
  (-> (mc/add-reset-callback model-client reset-word-number)
      (mc/add-callback [:word] update-word)
      (mc/add-callback [:number] update-number)))

(defn on-create-model 
  [mc class] 
  (cond
   (= "Main.TestActor" class)
   (on-init-test-actor mc)
   :else mc))

(defn message-handler
  [manager message reply-fn server-connection]
  (cond 
   (= "Sociabl.Parsers.WebsocketTestMessage.WebsocketTestMessage"
      (:datatype message))
   (cond
    (= "SendMessageBack" (:type (:msg message)))
    (let [value (:value (:msg message))]
      (if-not (nil? value)
	(reply-fn {:datatype "Sociabl.Parsers.WebsocketTestResponse"
		   :rsp {:type "ChangeWord"
			 :value (str value ", how are you?")}})))
    :else (l/info (pr-str "message handler: unhandled message: " message)))
   :else (l/info (pr-str "message handler: unhandled message: " message))))

(defn start
  []
  (reset! sociabl-global 
	  (wc/create-sociabl-web-client 
	   "ws://localhost:8088/" on-ready 
	   on-create-model
	   message-handler)))



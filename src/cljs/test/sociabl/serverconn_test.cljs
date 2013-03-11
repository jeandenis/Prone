(ns test.sociabl.serverconn_test
  (:require [sociabl.testing :as t]
	    [sociabl.logging :as l]
	    [sociabl.serverconn :as serv]))

(defn fake-websocket
  [close-fn send-fn]
  (let [ws (js* "{}")]
    (aset ws "close" close-fn)
    (aset ws "send" send-fn)
    ws))

(defn init-sc
  [sc ws result]
  ((.onopen ws))
  (.onmessage ws "0:0:9999:!"))

(defn parse-message-
  [s]
  (js->clj ((js* "JSON.parse") (serv/get-message-data s))
	   :keywordize-keys true))

(defn atom-inc-fn
  [a]
  (fn [o] (let [o (if (not (map? o)) (parse-message- o) o)
		incr (:v o)]
	    (swap! a + incr))))

(defn test-try*
  []
  (let [result (atom 0)]
    (try* ((js* "JSON.parse") "some junk")
	  (catch e (swap! result + 1)))
    (t/is? "try* works as expected" 1 @result)))

(defn test-clj->js
  []
  (let [cljs {:a :b 1 2 :z [4 5 6] :y {:c :b "d" "e"}}]
    (t/is? "cljs->js works for strings, numbers, arrays, and maps."
	   (js->clj 
	    (js* "{'1': 2, a: 'b', y: { c: 'b', d: 'e' }, z: [ 4, 5, 6 ]}"))
	   (js->clj (serv/clj->js cljs)))))

(defn test-sending-message
  []
  (let [result (atom 0)
	ws (fake-websocket t/do-nothing (atom-inc-fn result))
	sc (serv/server-conn- ws t/do-nothing t/do-nothing 
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (t/is? "send correct message" true (serv/send sc {:v 1}))
    (t/is? "send correct messsage result" 1 @result)
    (t/is? "send correct message 2" true (serv/send sc {:v 2}))
    (t/is? "send correct messsage 2 result" 3 @result)))

(defn test-reply-callback-on-init
  []
  (let [result (atom 0)
	ws (fake-websocket t/do-nothing t/do-nothing)
	sc (serv/server-conn- ws (fn [] (swap! result + 1)) t/do-nothing
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (t/is? "init calls ready callback" 1 @result)))

(defn test-sending-message-failures
  []
  (l/suppress true)
  (let [ws (fake-websocket t/do-nothing t/do-nothing)
	sc (serv/server-conn- ws t/do-nothing t/do-nothing
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (t/is? "send message pre init" false (serv/send sc {:v 1})))
  (let [result (atom 0)
	ws (fake-websocket t/do-nothing (atom-inc-fn result))
	sc (serv/server-conn- ws t/do-nothing t/do-nothing 
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (t/is? "send nil message" false (serv/send sc nil))
    (t/is? "send num message" false (serv/send sc 5))
    (t/is? "send string message" false (serv/send sc "hello"))
    (t/is? "send array message" false (serv/send sc [1, 2, 3]))
    (t/is? "send js array message" false (serv/send sc (js* "[1, 2, 3]")))
    (t/is? "send js object message" false (serv/send sc (js* "{a: 1, b: 2}")))
    (l/suppress false)
    ;; make sure that despite errors, sending messages still works
    (t/is? "send object message" true (serv/send sc {:v 8 :a 'b'}))
    (t/is? "send object message result" 8 @result)))

(defn test-disconnect
  []
  (let [result (atom 0)
	ws (fake-websocket (fn [] (swap! result + 1024)) (atom-inc-fn result))
	sc (serv/server-conn- ws t/do-nothing t/do-nothing 
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (t/is? "send correct message" true (serv/send sc {:v 1}))
    (t/is? "send correct messsage result" 1 @result)
    (serv/disconnect sc)
    (t/is? "close called successfully" 1025 @result)
    (l/suppress true)
    (t/is? "send correct message" false (serv/send sc {:v 1}))
    (t/is? "send correct messsage result" 1025 @result)
    (l/suppress false)))

(defn test-parse-message
  []
  (let [p (js* "JSON.parse")]
    (t/is? "normal message parsing" {:message-type "2" :message-id "4"
				   :recipient-id "6" :data {:a 1 :b 2}} 
	 (serv/parse-message "2:4:6:!{\"a\": 1, \"b\": 2}" p))
    (t/is? "normal message parsing with empty data"
	 {:message-type "2" :message-id "4"
	  :recipient-id "6" :data nil}
	 (serv/parse-message "2:4:6:!" p))
    (t/is? "not enough : message parsing" nil 
	 (serv/parse-message "1:2:3"))
    (l/suppress true)
    (t/is? "badly formatted data message parsing" nil
	 (serv/parse-message "1:2:3:!some nonsense"))
    (l/suppress false)))

(defn test-calling-on-open-out-of-turn
  []
    (let [result (atom 0)
	  ws (fake-websocket t/do-nothing (atom-inc-fn result))
	  sc (serv/server-conn- ws t/do-nothing t/do-nothing 
				(js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (t/is? "send correct message" true (serv/send sc {:v 1}))
    (t/is? "send correct messsage result" 1 @result)
    (l/suppress true)
    (t/is? "onopen out of turn fails" false ((.onopen ws)))
    (l/suppress false)
    ;; make sure that despite errors, sending messages still works
    (t/is? "send correct message" true (serv/send sc {:v 2}))
    (t/is? "send correct messsage result" 3 @result)))

(defn test-receiving-messages
  []
  (let [result (atom 0)
	ws (fake-websocket t/do-nothing t/do-nothing) 
	sc (serv/server-conn- ws t/do-nothing (atom-inc-fn result)
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (l/suppress true)
    (.onmessage ws "1:1:9999:!{\"v\": 16}")
    (t/is? "receive message but server connection not ready" 0 @result)
    (l/suppress false)
    (init-sc sc ws result)
    (.onmessage ws "1:1:9999:!{\"v\": 1}")
    (t/is? "receive corret message" 1 @result)
    (l/suppress true)
    (.onmessage ws "1:1:9998:!{\"v\": 2}")
    (t/is? "recipient id does not match" 1 @result)
    (.onmessage ws "1:1::!{\"v\": 4}")
    (t/is? "no recipient id" 1 @result)
    (.onmessage ws "1::9999:!{\"v\": 8}")
    (t/is? "no message id" 1 @result)
    (l/suppress false)))

(defn test-reply-to-message
  []
  (let [result (atom 0)
	reply-fn (fn [msg reply-fn]
		   (reply-fn msg))
	ws (fake-websocket t/do-nothing (atom-inc-fn result)) 
	sc (serv/server-conn- ws t/do-nothing reply-fn
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (.onmessage ws "1:1:9999:!{\"v\": 1}")
    (t/is? "successful reply" 1 @result)
    (.onmessage ws "1:1:9999:!{\"v\": 2}")
    (t/is? "successful reply to another message" 3 @result))
  (let [result (atom 0)
	dbl-reply-fn (fn [msg reply-fn]
		       (reply-fn msg)
		       (reply-fn msg))
	ws (fake-websocket t/do-nothing (atom-inc-fn result)) 
	sc (serv/server-conn- ws t/do-nothing dbl-reply-fn
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (l/suppress true)
    (.onmessage ws "1:1:9999:!{\"v\": 1}")
    (t/is? "double reply, but only the first reply works" 1 @result)
    (l/suppress false))
  (let [result (atom 0)
	nil-reply-fn (fn [msg reply-fn]
		       (reply-fn nil))
	ws (fake-websocket t/do-nothing (atom-inc-fn result)) 
	sc (serv/server-conn- ws t/do-nothing nil-reply-fn
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (l/suppress true)
    (.onmessage ws "1:1:9999:!{\"v\": 1}")
    (t/is? "replying with nil message properly caught" 0 @result)
    (l/suppress false))
  (let [result (atom 0)
	reply-fn (fn [msg reply-fn]
		   (reply-fn msg))
	check-reply-id-fn (fn [s] 
			    (t/is? "reply has correct message-id and content"
				 "2:1r:9999:!{\"v\":1}" s))
	ws (fake-websocket t/do-nothing check-reply-id-fn)
	sc (serv/server-conn- ws t/do-nothing reply-fn
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (.onmessage ws "1:1:9999:!{\"v\": 1}")))
    
(defn test-receiving-replies
  []
  (let [result (atom 0)
	reply-callback (fn [msg] (swap! result + (:v msg)))
	ws (fake-websocket t/do-nothing t/do-nothing)
	sc (serv/server-conn- ws t/do-nothing t/do-nothing 
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (serv/send sc {:v 1} reply-callback)
    (.onmessage ws "2:1r:9999:!{\"v\": 1}")
    (t/is? "received correct reply" 1 @result)
    (l/suppress true)
    (serv/send sc {:v 2} reply-callback)
    (.onmessage ws "2:3r:9999:!{\"v\": 2}")
    (t/is? "cannot reply with wrong message id" 1 @result)
    (.onmessage ws "2:2:9999:!{\"v\": 2}")
    (t/is? "cannot reply with message id that does not end in r" 1 @result)
    (l/suppress false)
    (.onmessage ws "2:2r:9999:!{\"v\": 2}")
    (t/is? "received correct reply, even after failures" 3 @result)
    (l/suppress true)
    (serv/send sc {:v 4})
    (.onmessage ws "2:3r:9999:!{\"v\": 4}")
    (t/is? "no callback function for reply" 3 @result)
    (l/suppress false)
    (serv/send sc {:v 8} reply-callback)
    (.onmessage ws "2:4r:9999:!{\"v\": 8}")
    (t/is? "still receiving correct replies" 11 @result)))

(defn test-disconnect-message
  []
  (let [result (atom 0)
	ws (fake-websocket (fn [] (swap! result + 1)) t/do-nothing)
	sc (serv/server-conn- ws t/do-nothing t/do-nothing 
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (.onmessage ws "3:0:9999:!")
    (t/is? "disconnected successfully because of disconnect message" 1 @result)))
    
(defn test-connected-to-ready
  []
  (let [result (atom 0)
	ws (fake-websocket t/do-nothing t/do-nothing)
	sc (serv/server-conn- ws (fn [] (swap! result + 1)) t/do-nothing 
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    ((.onopen ws))
    (l/suppress true)
    (.onmessage ws "0:0::!")
    (t/is? "could not move to ready state without recipient-id" :connected
	 (:state @sc))
    (l/suppress false)
    (.onmessage ws "0:0:1111:!")
    (t/is? "recipient id is correct on init" 1111 (:recipient-id @sc))
    (t/is? "correctly moved to ready state on init" :ready (:state @sc))
    (t/is? "called ready-callback on init" 1 @result)))

(defn test-close
  []
  (let [result (atom 0)
	ws (fake-websocket (fn [] (swap! result + 1)) t/do-nothing)
	sc (serv/server-conn- ws t/do-nothing t/do-nothing 
			      (js* "JSON.parse") (js* "JSON.stringify"))]
    (init-sc sc ws result)
    (l/suppress true)
    ((.onclose ws))
    (l/suppress false)
    (t/is? "disconnectd after onclose" :disconnected (:state @sc))
    (t/is? "close not called as part of onclose" 0 @result)))

(defn test-all
  []
  (test-try*)
  (test-clj->js)
  (test-sending-message)
  (test-sending-message-failures)
  (test-reply-callback-on-init)
  (test-disconnect)
  (test-parse-message)
  (test-calling-on-open-out-of-turn)
  (test-receiving-messages)
  (test-reply-to-message)
  (test-receiving-replies)
  (test-disconnect-message)
  (test-connected-to-ready)
  (test-close))


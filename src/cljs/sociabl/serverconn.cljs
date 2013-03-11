(ns sociabl.serverconn
  (:require [goog.string :as gstring]
	    [goog.json :as gjson]
	    [sociabl.logging :as l]))

;; TODO: replace with clojure.string implementation once that one is
;; fixed with regexp support
(defn blank?
  [s]
  (or (nil? s) (= "" s))) ;; TODO: doesn't catch many bad conditions...

(defn do-nothing [] nil)

(declare on-open)
(declare on-message)
(declare on-close)

(defn server-conn-
  ([socket ready-callback callback]
     (let [jq (js* "$")] 
       ;; dependency on jquery, use the other constructor to get around it
       (server-conn- socket ready-callback callback
		     (.parseJSON jq) gjson/serialize)))
  ([socket ready-callback callback parser stringifier]
     (let [server-conn (atom {:socket socket 
			      :callback callback
			      :ready-callback ready-callback
			      :parser parser
			      :stringifier stringifier
			      :state :connecting
			      :next-id 0
			      :reply-callbacks {}})]
       (aset socket "onopen" (on-open server-conn))
       (aset socket "onmessage" (on-message server-conn))
       (aset socket "onclose" (on-close server-conn))
       server-conn)))

(defn disconnect
  [server-conn]
  (swap! server-conn assoc :state :disconnecting)
  ((.close (:socket @server-conn))))

(defn clj->js
  "Converts a clj map/vector/number/string to a javascript 
   object/array/number/string, recursively.  Note that keywords
   are converted to strings using the name function."
   [cljo]
   (if (or (string? cljo) (number? cljo))
     cljo
     (if (keyword? cljo) 
       (name cljo)
       (if (map? cljo)
	 (let [out (js-obj)]
	   (doall (map #(let [key (first %)
			      key (if (number? key) key (name key))]
			  (if (or (string? key) (number? key))
			    (aset out key (clj->js (second %))))) cljo))
	   out)
	 (if (vector? cljo)
	   (let [out (js* "[]")]
	     (doall (map #(aset out (.length out) %) cljo))
	     out))))))

(defn send
  ([server-conn msg]
     (send server-conn msg do-nothing))
  ([server-conn msg callback]
     (if (and (= :ready (:state @server-conn))
	      (not (nil? msg))
	      (map? msg))
       (try*
	(let [msg-as-js (clj->js msg)
	      msg-id (:next-id @server-conn)
	      formatted-msg (str "1:" msg-id ":" (:recipient-id @server-conn)
				 ":!" ((:stringifier @server-conn) msg-as-js))
	      reply-callbacks (:reply-callbacks @server-conn)]
	  (swap! server-conn assoc :reply-callbacks (assoc reply-callbacks
						      (str msg-id) callback))
	  (swap! server-conn assoc :next-id (+ msg-id 1))
	  (l/info (str "send: " formatted-msg))
	  (.send (:socket @server-conn) formatted-msg)
	  true)
	(catch e
	  (l/error (str "error sending message: " @server-conn e))
	  false))
       (do (l/error (str "in wrong state or trying to send msg of incorrect "
			 "type: " @server-conn " " msg))
	   false))))
  
(defn get-message-data
  [msg]
  (let [index (.indexOf msg ":!")]
    (if (not (= -1 index))
      (subs msg (+ index 2))
      nil)))

;; message format:
;;
;; message-type:message-id:recipient-id:!data
;;
;; message types:
;;   "0" = init
;;   "1" = message
;;   "2" = reply
;;   "3" = disconnect
(defn parse-message
  [msg parser]
  (try*
   (let [message-parts (.split msg ":" 3)
	 message-data (get-message-data msg)]
     (if (not (nil? message-data))
       {:message-type (aget message-parts 0)
	:message-id (aget message-parts 1)
	:recipient-id (aget message-parts 2)
	:data (if (> (.length message-data) 0)
		(js->clj (parser message-data) :keywordize-keys true)
		nil)}
       nil))
   (catch e 
     (l/error (str "parse-message exception: " e))
     nil)))

(defn correct-recipient?
  [server-conn parsed-message]
  (and (not (nil? parsed-message))
       (= (:recipient-id @server-conn)
	  (gstring/toNumber (:recipient-id parsed-message)))))

(defn on-open
  [server-conn]
  (fn []
    (let [state (:state @server-conn)]
      (if (= :connecting state)
	(do
	  (swap! server-conn assoc :state :connected)
	  true)
	(do
	  (l/error (str "socket onopen called with incorrect "
			"server-conn state: " state))
	  false)))))

(defn on-message
  [server-conn]
  (fn [msg-event]
    (let [msg (if-not (string? msg-event) (.data msg-event) msg-event)
	  state (:state @server-conn)]
      (l/info (str "receive: " msg))
      (cond 
       ;; ready
       (= :ready state) 
       (let [pm (parse-message msg (:parser @server-conn))]
	 (if (and (not (nil? pm))
		  (correct-recipient? server-conn pm))
	   (cond
	    ;; message
	    (and (= "1" (:message-type pm))
		 (not (blank? (:message-id pm))))
	    (let [replied (atom false)
		  reply-fn 
		  (fn [reply-data]
		    (if (and (not @replied)
			     (map? reply-data)
			     (= :ready (:state @server-conn)))
		      (let [msg-as-js (clj->js reply-data)
			    reply-msg (str "2:" (:message-id pm)
					   "r:" (:recipient-id @server-conn) 
					   ":!" ((:stringifier @server-conn) 
						 msg-as-js))]
			(reset! replied true)
			(.send (:socket @server-conn) reply-msg))
		      (l/error (str "tried replying to message but reply "
				    "failed: " @replied reply-data
				    (:state @server-conn)))))]
	      ((:callback @server-conn) (:data pm) reply-fn @server-conn))
	    ;; reply
	    (and (= "2" (:message-type pm))
		 (not (blank? (:message-id pm))))
	    (let [mid (:message-id pm)]
	      (if (= "r" (subs mid (- (count mid) 1)))
		(let [callback-id (subs mid 0 (- (count mid) 1))
		      callback ((:reply-callbacks @server-conn) callback-id)]
		  (if-not (nil? callback)
		    (callback (:data pm))
		    (l/error "received reply message but no callback")))
		(l/error (str "received reply message but the message id "
			      "does not end in r"))))
	    ;; disconnect
	    (= "3" (:message-type pm))
	    (do (swap! server-conn assoc :state :disconnecting)
		((.close (:socket @server-conn))))
	    :else (l/error (str "received message with unknown message type "
				"or no message id:" pm)))
	   (l/error (str "received incorrect message (perhaps its "
			 "recipient-id does not match this client): " pm))))
       ;; connected
       (= :connected state)
       (let [pm (parse-message msg)]
	 (if (and (not (nil? pm))
		  (= "0" (:message-type pm))
		  (not (blank? (:recipient-id pm))))
	   (do
	     (swap! server-conn assoc :state :ready)
	     (swap! server-conn assoc :next-id 1)
	     (swap! server-conn assoc :recipient-id 
		    (gstring/toNumber (:recipient-id pm)))
	     (if-not (nil? (:ready-callback @server-conn))
	       ((:ready-callback @server-conn))))
	   (l/error (str "received non-init or wrong-init message when "
			 "server conn is in connected state: " pm))))
       :else (l/error (str "server conn is in an unsupported state: " 
			   (:state @server-conn)))))))

(defn on-close
  [server-conn]
  (fn []
    (if-not (= :disconnecting (:state @server-conn))
      (l/error (str "socket onclose called with incorrect server conn state: "
		    (:state @server-conn))))
    (swap! server-conn assoc :state :disconnected)))

(defn create-server-connection
  ([uri] (create-server-connection uri do-nothing do-nothing))
  ([uri callback] (create-server-connection uri callback do-nothing))
  ([uri callback ready-callback]
     (server-conn- (js* "new WebSocket(~{uri})") ready-callback callback)))
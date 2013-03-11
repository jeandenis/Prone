(ns sociabl.webclient
    (:require [sociabl.serverconn :as sc]
	      [sociabl.modelclient :as mc]
	      [sociabl.logging :as l]))

(defn create-sociabl-web-client
  [uri ready-fn on-create-model message-handler]
  (let [manager (mc/create-manager on-create-model)
	receive-message-fn 
	(fn [message reply-fn serv-conn]
	  (let [msg (:msg message)
		msg-datatype (:datatype message)]
	    (if (= (:to message) (:recipient-id serv-conn))
	      (cond
	       (= "Sociabl.SubscriberModel.ModelUpdateMessage" msg-datatype)
	       (mc/handle-unwrapped-message manager msg)
	       :else 
	       (message-handler manager message reply-fn serv-conn)))))
	server-connection 
	(sc/create-server-connection uri receive-message-fn ready-fn)]
    {:manager manager
     :server-connection server-connection}))
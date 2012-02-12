(ns CorrInt.server
  (:require [noir.server :as server])
  (:use [ring.middleware.multipart-params.byte-array]))

(server/load-views "src/CorrInt/views/")

(defn wrap-utf-8
  "Adds the 'charset=utf-8' clause onto the content type declaration,
allowing pages to display all utf-8 characters."
  [handler]
  (fn [request]
    (let [resp (handler request)
          ct (get-in resp [:headers "Content-Type"])
          neue-ct (str ct "; charset=utf-8")]
      (assoc-in resp [:headers "Content-Type"] neue-ct))))

(defn -main [& m]
  (let [mode (keyword (or (first m) :dev))
        port (Integer. (get (System/getenv) "PORT" "8080"))]
    ;(server/add-middleware (byte-array-store))
    ;(server/add-middleware ())
    (server/start port {:mode mode
                        :ns 'CorrInt})))



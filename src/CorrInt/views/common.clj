(ns CorrInt.views.common
  (:use [noir.core :only [defpartial]]
        [hiccup.page-helpers :only [include-css html5]]))

(defpartial layout [& content]
            (html5
              [:head
               [:title "testsite"]
               (include-css "/css/reset.css")]
              [:body
               [:div#wrapper
                content]]))

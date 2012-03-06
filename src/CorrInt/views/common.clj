(ns CorrInt.views.common
  (:use [noir.core :only [defpartial]]
        [hiccup.page-helpers :only [include-css html5 link-to]]))

(defpartial layout [& content]
            (html5
              [:head
               [:title "CorrInt - Correlation Integration"]
               (include-css "/css/reset.css")]
              [:body
               [:div#wrapper
                content]
                [:p (link-to "/" "Return to Main Page")]]))

(ns CorrInt.views.welcome
  (:require [CorrInt.views.common :as common]
            [noir.content.getting-started]
            [clojure.java.io :as io]
            [noir.response :as resp]
            [clojure.contrib.combinatorics :as combi])
  (:use [noir.core :only [defpage]]
        [hiccup.core]
        [hiccup.page-helpers]
        [hiccup.form-helpers]
        [incanter core charts excel stats io]))

(defn read-my-xls [xls]
  (with-data (read-xls xls)
    $data))

(defn get-from-xls [xls what]
    (with-data (read-xls xls)
      ($ what)))

(defn get-from-csv [csv what]
    (with-data (read-dataset csv :header true)
       ($ what)))

(defn indexize [xls]
  (reduce (fn [m [x y]]
            (assoc m y x))
    {} (partition 2 (interleave
                      (iterate inc 0)
                      (get-from-xls xls 0)))))

(defn xls-to-matrix [xls]
  (with-data (read-xls xls)
    (to-matrix $data)))

(defn write-lines [f seq]
   (with-open [w (clojure.java.io/writer f)]
    (doseq [x seq]
      (.write w (str x))
      (.newLine w))))

(defn calc-score [scores factors]
  (or (some #{-2} scores)
      (reduce + (map (partial * 100) factors scores))))

(defn get-all-genes2 [files]
  (apply map (fn [& args] args)
      (map #(get-from-csv % 2) files))) ; scores are in 2nd column

(defn integrate [files factors]
  (map #(calc-score % factors)
    (get-all-genes2 files)))

(defn avg [seq]
  (let [[sum count] (reduce (fn [[x y] curr] [(+ x curr) (inc y)]) [0 0] seq)]
    (/ sum count)))

(defn calc-avg-distance [data]
  (let [zhong-scores (get-from-xls "C://Users//ronenc//CorrInt//zhong.xlsx" 2)]
    (avg (map (fn [[x y]] (Math/abs (- x y)))
         (filter (fn [[x y]] (and (not= x -2) (not (nil? y)))) (map vector data zhong-scores))))))

;(defn get-avgs [files]
;  (map calc-avg-distance
;    (map #(integrate files %)
;      (filter #(= 1 (reduce + %)) (clojure.contrib.combinatorics/selections (map #(/ % 10) (range 1 6)) 3)))))

(defn sadger-cohen [files]
  (apply min (map calc-avg-distance
               (map #(integrate files %)
                 (filter #(= 1 (reduce + %)) (clojure.contrib.combinatorics/selections (map #(/ % 10) (range 1 6)) (count files)))))))

(defn correlate [matrix idx1 idx2]
  (if (and idx1 idx2)
    (let [c (Math/abs (correlation (sel matrix :rows idx1 :except-cols 0) (sel matrix :rows idx2 :except-cols 0)))]
      (max c 0))
    -2))

(defn analyze-data [xls standard]
    (let [indices (indexize xls)
          matrix (xls-to-matrix xls)]
      (loop [a-gene (get-from-xls standard "Gene1")
             b-gene (get-from-xls standard "Gene2")
             acc ["Gene1,Gene2,Score"]]
        (if (empty? a-gene)
          acc
          (let [idx1 (indices (first a-gene)) idx2 (indices (first b-gene))]
            (recur (rest a-gene) (rest b-gene) (conj acc (str (first a-gene) "," (first b-gene) "," (correlate matrix idx1 idx2)))))))))

(defpage "/success" []
  (common/layout
    [:h2 "File uploaded!"]
    (link-to "/" "Return to Main Page")))

(defpage [:post "/upload"] {:keys [file]}
  (io/copy (io/file (:tempfile file)) (io/file (str "datasets" java.io.File/separator (:filename file))))
  (resp/redirect "/success"))

(defpage "/upload" []
  (common/layout
    (form-to {:enctype "multipart/form-data"}
      [:post "/upload"]
      (label :file "File to upload:")
      (file-upload :file)
      [:br]
      (submit-button "Upload"))))

(defpage [:post "/integrate"] {:as m}
  (spit "/home/sadger/CorrInt/test.txt" m)
  (common/layout
    [:p "Thanks!"]))

(defpage "/integrate" []
  (common/layout
    [:h2 "Select Datasets to Integrate"]
    (form-to [:post "/integrate"]
      (map (fn [f] [:p (check-box "set" false (.getName f)) (.getName f) [:br]])
        (rest (file-seq (java.io.File. "analyzed"))))
      (submit-button "Integrate!"))))


(defpage "/" []
  (common/layout
    (link-to "/upload" "Upload Dataset")
    (link-to "/integrate" "Integrate Datasets")))




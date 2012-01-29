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
(import [java.io FileInputStream]
        [org.apache.commons.math.stat.correlation PearsonsCorrelation])

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
      (.write w x)
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

(defn get-avgs [files]
  (map calc-avg-distance
    (map #(integrate files %)
      (filter #(= 1 (reduce + %)) (clojure.contrib.combinatorics/selections (map #(/ % 10) (range 1 6)) 3)))))

(defn sadger-cohen [files]
  (apply min (map calc-avg-distance
               (map #(integrate files %)
                 (filter #(= 1 (reduce + %)) (clojure.contrib.combinatorics/selections (map #(/ % 10) (range 1 6)) 3))))))

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
    [:h2 "File uploaded!"]))
; Show this page in case of failure
(defpage "/fail" []
  (common/layout
    [:h2 "Something went wrong"]))

(defpage [:post "/upload"] {:keys [file]}
  (spit "C://Users//ronenc//CorrInt//test.txt" (str file))
  (io/copy (io/file (:tempfile file)) (io/file "C://Users//ronenc//CorrInt//booga.mp3"))
  (resp/redirect "/success"))
;(if (= "0" (:size file))
  ;  (resp/redirect "/success")
  ;  (resp/redirect "/fail")))

;(write-lines (str root-dir "sample.csv") (analyze-data (str root-dir "CEL-Seq_expression_datasetA.xls") (str root-dir "zhongybongy.xls"))))))

(defpage "/upload" []
  (common/layout
    (form-to {:enctype "multipart/form-data"}
      [:post "/upload"]
      (label :file "File to upload:")
      (file-upload :file)
      [:br]
      (submit-button "Upload"))))

(defpage "/welcome" []
  ;(write-lines "C://Users//ronenc//CorrInt//sample1.csv" (analyze-data "C://Users//ronenc//CorrInt//CEL-Seq_expression_datasetA.xls"
  ;  "C://Users//ronenc//CorrInt//zhong.xlsx"))
  ;(write-lines "C://Users//ronenc//CorrInt//sample2.csv" (analyze-data "C://Users//ronenc//CorrInt//Dev_Timecourse_Cele_avg_Aprobe.xls"
  ;                                                         "C://Users//ronenc//CorrInt//zhong.xlsx"))
  ;(write-lines "C://Users//ronenc//CorrInt//sample3.csv" (analyze-data "C://Users//ronenc//CorrInt//NN_expression_matrix.xls"
  ;                                                         "C://Users//ronenc//CorrInt//zhong.xlsx"))
  ;(write-lines "C://Users//ronenc//CorrInt//correlations.csv" (map str (integrate ["C://Users//ronenc//CorrInt//sample1.csv"
  ;                                                                        "C://Users//ronenc//CorrInt//sample2.csv"
  ;                                                                        "C://Users//ronenc//CorrInt//sample3.csv"]
  ;                                                           [0.5 0.5 0.5]))))

  ;(common/layout
  ;  (map (fn [x] [:p x]) (integrate ["C://Users//ronenc//CorrInt//sample1.csv"
  ;                                   "C://Users//ronenc//CorrInt//sample2.csv"
  ;                                   "C://Users//ronenc//CorrInt//sample3.csv"]
  ;                         [1 1 1]))))

  (common/layout
    ;[:p (calc-avg-distance (integrate ["C://Users//ronenc//CorrInt//sample1.csv"
    ;                                                                    "C://Users//ronenc//CorrInt//sample2.csv"
    ;                                                                    "C://Users//ronenc//CorrInt//sample3.csv"]
    ;                                                       [0.2 0.3 0.5]))]))
    (map (fn [x] [:p x]) (get-avgs ["C://Users//ronenc//CorrInt//sample1.csv"
                       "C://Users//ronenc//CorrInt//sample2.csv"
                       "C://Users//ronenc//CorrInt//sample3.csv"]))
    [:p (sadger-cohen ["C://Users//ronenc//CorrInt//sample1.csv"
                       "C://Users//ronenc//CorrInt//sample2.csv"
                       "C://Users//ronenc//CorrInt//sample3.csv"])]))




(ns CorrInt.views.welcome
  (:require [CorrInt.views.common :as common]
            [noir.content.getting-started]
            [clojure.java.io :as io]
            [noir.response :as resp]
            [clojure.contrib.combinatorics :as combi]
            [clojure.contrib.str-utils :as str-utils])
  (:use [noir.core :only [defpage]]
        [hiccup.core]
        [hiccup.page-helpers]
        [hiccup.form-helpers]
        [incanter core charts excel stats io]
        [clojure.string]))

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

(defn sadger-cohen [files]
  (let [factors (filter #(= 1 (reduce + %)) (clojure.contrib.combinatorics/selections (map #(/ % 10) (range 0 11)) (count files)))]
    (sort-by first (map vector (map calc-avg-distance
                 (map #(integrate files %) factors)) factors))))

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
             acc []]
        (if (empty? a-gene)
          acc
          (let [idx1 (indices (first a-gene)) idx2 (indices (first b-gene))]
            (recur (rest a-gene) (rest b-gene) (conj acc [(first a-gene) (first b-gene) (correlate matrix idx1 idx2)])))))))

(defpage "/success" []
  (common/layout
    [:h2 "File uploaded!"]
    (link-to "/" "Return to Main Page")))

(defn write-csv [f header lines]
  (write-lines f header)
  (write-lines f (map #(str-utils/str-join "," %) lines)))

(defn brify [seq]
  (interpose [:br] seq))

(defpage [:post "/upload"] {:keys [file]}
  (io/copy (io/file (:tempfile file)) (io/file (str "datasets" java.io.File/separator (:filename file))))
  (write-csv (str "analyzed" java.io.File/separator (:filename file) ".csv") ["Gene1,Gene2,Score"]
    (analyze-data (str "datasets" java.io.File/separator (:filename file)) "zhong.xlsx"))
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
  (spit "test.txt" m)
  (common/layout
    (map (fn [[g factors]] [:p (str g " " (apply str (interpose " " factors)))])
      (sadger-cohen (:dataset m)))))

(defn get-file-checkboxes [dir-name]
  (map (fn [f] [:p (.getName f) (check-box "dataset[]" false (.getPath f))])
    (rest (file-seq (java.io.File. dir-name)))))

(defpage "/integrate" []
  (common/layout
    [:h2 "Select Datasets to Integrate"]
    (form-to [:post "/integrate"]
      (get-file-checkboxes "analyzed")
      (submit-button "Integrate!"))))

(defn create-plot [genes dataset]
  (let [matrix (xls-to-matrix dataset)
       cols (count (sel matrix :rows 0))
       index (indexize dataset)]
    (reduce #(add-points %1 (range cols) (sel matrix :rows (index %2)))
      (xy-plot (range cols) (sel matrix :rows (index (first genes))))
        (rest genes))))

(defn get-file-name [f]
  (.getName (java.io.File. f)))

(defn create-plot-img [genes dataset]
  (let [file-name (str (get-file-name dataset)  ".png")]
    (save (create-plot genes dataset) (str "resources/public/img/" file-name))
    (str "/img/" file-name)))

(defpage [:post "/gene-viewer"] {:as m}
  (common/layout
     (map (fn [f] [:image {:src (create-plot-img (split (:genes m) #",") f)}])
       (:dataset m))))

(defpage "/gene-viewer" []
  (common/layout
    (form-to [:post "/gene-viewer"]
      [:p "Enter gene id's: " (text-field "genes")]
      (get-file-checkboxes "datasets")
      (submit-button "View!"))))

(defpage "/" []
  (common/layout
    (link-to "/upload" "Upload Dataset")
    [:br]
    (link-to "/integrate" "Integrate Datasets")
    [:br]
    (link-to "/gene-viewer" "View genes")))




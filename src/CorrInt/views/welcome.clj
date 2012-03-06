(ns CorrInt.views.welcome
  (:require [CorrInt.views.common :as common]
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

(def threshold 0.5)
(def golden-standard "zhong.xlsx")
(def golden-standard-no-header "zhong-no-header.xlsx")
(def invalid-score -2.0)
(def max-score 80)

(defn absv [x] (Math/abs x))

(defn make-table
  ([t] (make-table t nil))
  ([t properties]
    [:table (or properties {}) (map (fn [s] (into [:tr ] (map (fn [x] [:td x]) s))) t)]))

(defmacro with-table [& body] ; needs a better name
  `(make-table [~@body]))

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

(defn read-lines [f]
  (with-open [r (clojure.java.io/reader f)]
    (loop [acc []]
      (let [line (.readLine r)]
        (if (nil? line)
          acc
          (recur (conj acc line)))))))

(defn write-csv [f header lines]
  (if header
    (write-lines f header))
  (write-lines f (map #(str-utils/str-join "," %) lines)))

(defn calc [p]
  (cond
    (= 0.0 p) 0.0
    (= 1.0 p) max-score
    :else (absv (Math/log (absv (/ p (- 1 p)))))))

;(defn distribute-weights [f s]
;  (let [j (partition 2 (interleave f s))
;        over (filter (fn [[x y]] (>= y 0.7)) j)
;        under (filter (fn [[x y]] (< y 0.7)) j)
;        delta (- 1 (reduce + (map first over)))]
;    (reduce (fn [s [uf us]]
;              (map (fn [[of os]]
;                     [(+ of (* of uf) (/ (* uf delta) (count over))) os]) s)) over under)))

(defn calc-score [scores factors]
  (if (every? #(and (not= invalid-score %) (>= (absv %) threshold)) scores)
     (reduce + (map (fn [f s] (calc (* f s))) factors scores))
    invalid-score))

(defn get-all-genes2 [files]
  (apply map vector
    (map #(get-from-csv % 2) files))) ; scores are in 2nd column

(defn integrate [files factors]
  (map #(calc-score % factors)
    (get-all-genes2 files)))

(defn avg [seq]
  (let [[sum count] (reduce (fn [[x y] curr] [(+ x curr) (inc y)]) [0 0] seq)]
    (/ sum count)))

(defn calc-avg-distance [data]
  (let [zhong-scores (get-from-xls golden-standard-no-header 2)]
    (avg (map (fn [[x y]] (Math/abs (- x y)))
           (filter (fn [[x y]] (and (not= x invalid-score) (not (nil? y))))
             (map vector data zhong-scores))))))

(defn sadger-cohen [files]
  (let [factors (filter #(= 1 (reduce + %)) (clojure.contrib.combinatorics/selections (map #(/ % 10) (range 0 11)) (count files)))]
    (sort-by first (map conj factors (map calc-avg-distance
                                       (map #(integrate files %) factors))))))

(defn sel-row [matrix idx]
  (sel matrix :rows idx :except-cols 0))

(defn correlate [matrix idx1 idx2]
  (if (and idx1 idx2)
    (let [c (Math/abs (correlation (sel-row matrix idx1) (sel-row matrix idx2)))]
      (max c 0))
    invalid-score))

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

(defpage [:post "/upload"] {:keys [file]}
  (io/copy (io/file (:tempfile file)) (io/file (str "datasets" java.io.File/separator (:filename file))))
  (write-csv (str "analyzed" java.io.File/separator (:filename file) ".csv") ["Gene1,Gene2,Score"]
    (analyze-data (str "datasets" java.io.File/separator (:filename file)) golden-standard))
  (resp/redirect "/success"))

(defpage "/upload" []
  (common/layout
    (form-to {:enctype "multipart/form-data"}
      [:post "/upload"]
      (label :file "File to upload:")
      (file-upload :file )
      [:br ]
      (submit-button "Upload"))))

(defn get-file-name [f]
  (.substring f (inc (.lastIndexOf f "\\")) (.indexOf f ".")))

(defn get-cache-name [files]
  (str "cache" java.io.File/separator
    (str-utils/str-join "_" (map get-file-name files)) "_" threshold ".csv"))

(defn read-cache [file]
  (map #(split % #",") (read-lines file)))

(defn create-results-table [results]
   (make-table results {:class "table_result"}))

(defpage [:post "/integrate"] {:as m}
  (common/layout
    (let [datasets (vec (:dataset m))
          cache-name (get-cache-name datasets)]
      (if (.exists (java.io.File. cache-name))
        (create-results-table (read-cache cache-name))
        (let [result (into [(into ["Score"] (map get-file-name datasets))]
          (sadger-cohen datasets))]
          (write-csv cache-name nil result)
          (create-results-table result))))))

(defn get-file-checkboxes [dir-name]
  (map (fn [f] [(get-file-name (.getName f)) (check-box "dataset[]" false (.getPath f))])
    (rest (file-seq (java.io.File. dir-name)))))

(defpage "/integrate" []
  (common/layout
    [:p [:font {:size 8} "Select Datasets to Integrate"]
     (form-to [:post "/integrate"]
       (make-table (get-file-checkboxes "analyzed"))
       [:p (submit-button "Integrate!")])]))

(defn create-plot [genes dataset]
  (let [matrix (xls-to-matrix dataset)
        cols (count (sel matrix :rows 0))
        index (indexize dataset)]
    (reduce #(add-points %1 (range cols) (sel-row matrix (index %2)) :series-label %2)
      (scatter-plot (range cols) (sel-row matrix (index (first genes)))
        :legend true
        :x-label "Experiment"
        :y-label "Expression Level"
        :series-label (first genes)
        :title (get-file-name dataset))
      (rest genes))))

(defn create-plot-img [genes dataset]
  (let [file-name (str (get-file-name dataset) ".png")]
    (save (create-plot genes dataset) (str "resources/public/img/" file-name))
    (str "/img/" file-name)))

(defpage [:post "/gene-viewer"] {:as m}
  (common/layout
    (make-table
      (map (fn [f] [[:image {:src (create-plot-img (split (:genes m) #",") f)}]])
        (:dataset m)))))

(defpage "/gene-viewer" []
  (common/layout
    [:p (form-to [:post "/gene-viewer"]
      (make-table (get-file-checkboxes "datasets"))
      [:p "Enter gene id's: " (text-field "genes")]
      [:p (submit-button "View!")])]))

(defn img-link [url img]
  (link-to url [:image {:src img :border 0 :width 150 :height 150 :align "left"}]))

(defn get-link-bundle [url image text]
  [(img-link url image) (link-to url [:b text])])

(defpage "/" []
  (common/layout
    [:p [:b [:font {:size 16} "Welcome to CorrInt!"]]
     [:br ]
     (with-table
       ["What do you want to do?"]
       (get-link-bundle "/upload" "/img/upload.png" "Upload!")
       (get-link-bundle "/integrate" "/img/integrate.jpg" "Integrate!")
       (get-link-bundle "/gene-viewer" "/img/magnify.jpg" "View Genes!"))]))

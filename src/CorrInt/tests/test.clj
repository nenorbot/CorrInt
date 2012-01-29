(use '[incanter core charts excel stats io])
(import [java.io FileInputStream]
  [org.apache.commons.math.stat.correlation PearsonsCorrelation])
(require [clojure.contrib.combinatorics :as combi])

(defmacro time2 [label expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str "Elapsed time for " ~label " " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))

(defn read-my-xls [xls]
  (let [file xls]
    (with-data (read-xls file)
      $data)))

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
    (reduce + (map * factors scores))))

(defn get-all-genes2 [files]
  (apply map (fn [& args] args)
    (map #(get-from-csv % 2) files)))

(defn integrate [files factors]
  (map #(calc-score % factors)
    (get-all-genes2 files)))

(defn correlate [matrix idx1 idx2]
  (if (and idx1 idx2)
    (correlation (sel matrix :rows idx1 :except-cols 0) (sel matrix :rows idx2 :except-cols 0))
    -2))

(defn analyze-data [xls standard]
  (let [indices (time2 :indices (indexize xls))
        matrix (time2 :xls-to-matirx (xls-to-matrix xls))]
    (loop [a-gene (time2 :a-gene (get-from-xls standard "Gene1"))
           b-gene (time2 :b-gene (get-from-xls standard "Gene2"))
           acc ["Gene1,Gene2,Score"] n 0]
     (if (= 0 (mod n 1000)) (println n))
      (if (empty? a-gene)
        acc
        (let [idx1 (indices (first a-gene)) idx2 (indices (first b-gene))]
          (recur (rest a-gene) (rest b-gene) (conj acc (str (first a-gene) "," (first b-gene) "," (correlate matrix idx1 idx2))) (inc n)))))))

(defn calc-score [scores factors]
  (or (some #{-2} scores)
    (reduce + (map * factors scores))))

(defn get-all-genes [files]
  (apply map (fn [& args] args)
    (map #(get-from-csv % "Score") files)))

(defn integrate [files factors]
  (map #(calc-score % factors)
    (get-all-genes files)))

(defn test2 [xls]
  (time (let [matrix (xls-to-matrix xls)])))

(defn avg [seq]
  (let [[sum count] (reduce (fn [[x y] curr] [(+ x curr) (inc y)]) [0 0] seq)]
    (/ sum count)))

(defn calc-avg-distance [data]
  (let [zhong-scores (get-from-xls "C://Users//ronenc//CorrInt//zhong.xlsx" 2)]
    (avg (map (fn [[x y]] (Math/abs (- x y)))
           (filter (fn [[x y]] (not= x -2)) (map vector data zhong-scores))))))


(defn get-avgs [files]
  (map calc-avg-distance
    (map #(integrate files %)
      (filter #(= 10 (reduce + %)) (clojure.contrib.combinatorics/selections (range 1 10) 3)))))

(defn sadger-cohen [files]
  (apply min (map calc-avg-distance
               (map #(integrate files %)
                 (filter #(= 10 (reduce + %)) (clojure.contrib.combinatorics/selections (range 1 10) 3))))))
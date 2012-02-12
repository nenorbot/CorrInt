(ns CorrInt.views.welcome
  (:require [CorrInt.views.common :as common]
            [noir.content.getting-started])
  (:use [noir.core :only [defpage]]
        [hiccup.core]
        [hiccup.page-helpers]
        [incanter core charts excel]))
(import [java.io FileInputStream]
        [org.apache.commons.math.stat.correlation PearsonsCorrelation])

(defn test-xls []
  ;(let [file "/home/sadger/CEL-Seq_expression_datasetA.xls"]
  (let [file "/home/sadger/Downloads/test.xls"]
    (with-data (read-xls file)
      ;(str (first $data) "<br><br><br><br>" (second $data) "<br><br><br><br>" (nth $data 3)))))
      ;(str (nth (second (to-matrix $data)) 7)))))
      (let [row (second (to-matrix $data))]
        ;(str (count (to-matrix $data)))))))
        ;(save (time-series-plot (range 1 (count row)) row) "/home/sadger/CorrInt/resources/public/img/test.png")))))
      (str (first ($ 1)))))))

(defn read-my-xls [xls]
  (let [file xls]
  (with-data (read-xls file)
    $data)))

(defn get-from-xls [xls what]
    (with-data (read-xls xls)
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

(defn to-java-matrix [matrix]
  (let [rows (count matrix)
        columns (count (first matrix))
        array (make-array Double/TYPE rows columns)]
    (dotimes [x rows]
      (dotimes [y columns]
        (aset array x y (nth (nth matrix x) y))))
    array))

(defn calc-pearsons [matrix x y]
  (let [pearsons (PearsonsCorrelation.)
        java-matrix (to-java-matrix matrix)]
    (.correlation pearsons (nth java-matrix x) (nth java-matrix y))))

(defn analyze-data [xls standard]
  (let [indices (indexize xls)
        java-matrix (to-java-matrix (xls-to-matrix xls))]
    (loop [a-gene (get-from-xls standard "Gene1")
           b-gene (get-from-xls standard "Gene2")
           acc [:div]
           n 5]
      ;(if (empty? a-gene)
      ;  acc)
      (if (= 0 n) acc
      (let [idx1 (indices (first a-gene)) idx2 (indices (first b-gene))]
        (if (and idx1 idx2)
          (let [pc 0];(calc-pearsons java-matrix idx1 idx2)]
            (recur (rest a-gene) (rest b-gene) (conj acc [:p (str (first a-gene) " ") (str (first b-gene) " ") pc]) (dec n)))
          (recur (rest a-gene) (rest b-gene) acc (dec n))))))))

(defpage "/welcome" []
         (common/layout
           [:p "Welcome to CorrInt!"]
         ;  (str (count (indexize "/home/sadger/CorrInt/CEL-Seq_expression_datasetA.xls")))
         (analyze-data "/home/sadger/CorrInt/CEL-Seq_expression_datasetA.xls" "/home/sadger/CorrInt/zhongybongy.xls")
           (image "img/test.png")))





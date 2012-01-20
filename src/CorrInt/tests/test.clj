(use '[incanter core charts excel])
(import [java.io FileInputStream]
  [org.apache.commons.math.stat.correlation PearsonsCorrelation])

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

(defn calc-pearsons [data1 data2]
  (let [pearsons (PearsonsCorrelation.)]
    (.correlation pearsons data1 data2)))

(defn to-java-array [matrix idx]
  (let [data (nth matrix idx)
       array (make-array Double/TYPE (count data))]
    (dotimes [x (count data)]
      (aset array x (nth data x)))
  array))

(defn stam [xls standard]
  (let [indices (indexize xls)
        matrix (xls-to-matrix xls)]
    (print "step1")
    (loop [a-gene (get-from-xls standard "Gene1")
           b-gene (get-from-xls standard "Gene2")
           acc [:div]
           n 5]
      ;(if (empty? a-gene)
      ;  acc)
      (print n)
      (if (= 0 n) acc
        (recur (rest a-gene) (rest b-gene) acc (dec n))))))

(defn analyze-data [xls standard]
  (let [indices (indexize xls)
        matrix (xls-to-matrix xls)]
    (loop [a-gene (get-from-xls standard "Gene1")
           b-gene (get-from-xls standard "Gene2")
           acc [:div]
           n 5]
      ;(if (empty? a-gene)
      ;  acc)
      (if (= 0 n) acc
        (let [idx1 (indices (first a-gene)) idx2 (indices (first b-gene))]
          (if (and idx1 idx2)
            (let [pc (calc-pearsons (to-java-array matrix idx1) (to-java-array matrix idx2))]
              (recur (rest a-gene) (rest b-gene) (conj acc [:p (str (first a-gene) " ") (str (first b-gene) " ") pc]) (dec n)))
            (recur (rest a-gene) (rest b-gene) acc (dec n))))))))





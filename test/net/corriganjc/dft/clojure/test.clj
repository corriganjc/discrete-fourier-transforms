(ns net.corriganjc.dft.clojure.test
  (:use (net.corriganjc.dft.clojure core)
        (clojure test))
  (:import org.apache.commons.math3.complex.Complex
           java.lang.Math))

;; Helper functions
(def TOLERANCE 0.00000001)
 
(defn- within-tolerance 
  [diff-fn x y]
  (< (Math/abs (diff-fn x y)) TOLERANCE))

(defn- compare-sequences 
  [diff-fn xs ys]
  (reduce #'and
          (map #(within-tolerance diff-fn %1 %2) xs ys)))

;; Tests
(deftest test-fft
 (let [sequence      [1.0 1.0 1.1 1.2 1.5 2.1 5.2 2.2 1.47 1.2 1.09 1.01 1.0 0.9 1.0 1.0]
       expected-real-components [23.97 5.112719025376176 -0.10435028842544314 2.890138935696909 3.420000000000001 
                                 2.0954158906463602 -0.16435028842544364 1.7528358009670928 -2.75 -1.7528358009670915 
                                 -0.1643502884254454 -2.0954158906463607 -3.420000000000001 -2.890138935696908 
                                 -0.10435028842544491 -5.112719025376178 ]
       actual       (fft sequence)
       actual-real-components (map #(.getReal %) actual)]
     (is (compare-sequences #'- expected-real-components actual-real-components))))


(deftest test-fft-round-trip
  (let [sequence     [1.0 1.0 1.1 1.2 1.5 2.1 5.2 2.2 1.47 1.2 1.09 1.01 1.0 0.9 1.0 1.0]
        new-sequence (inverse-fft(fft sequence))]
    (is (compare-sequences #'- sequence new-sequence))))

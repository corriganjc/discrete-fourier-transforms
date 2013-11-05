(ns net.corriganjc.dft.clojure.core
  (:import org.apache.commons.math3.complex.Complex
           java.lang.Integer
           java.lang.Math))

(declare  power-of-two butterfly fft-complex)

;; Public functions
;;
(defn generate-omega-coeffs
  "This function generates Complex coefficients for inverse FFT."
  [n]
  (.nthRoot Complex/ONE n))

(defn generate-inverse-omega-coeffs
  "This function generates Complex coefficients for inverse FFT."
  [n]
  (map  #(.reciprocal % ) (generate-omega-coeffs  n))) 

(defn fft
  "This is the entry point function for FFT calculations and converts the real 
   input into Complex values before handing off work to an auxiliary function."
  ([xs]    (fft-complex (map #(Complex. %) xs)))
  ([xs ws] (fft-complex (map #(Complex. %) xs) ws)))

(defn inverse-fft 
  "This function carries out an inverse FFT."
  ([xs]
    {:pre [ (power-of-two (count xs)) ]}
    (inverse-fft xs (generate-inverse-omega-coeffs (count xs))))
  
  ([xs ws]
    {:pre [ (power-of-two (count xs))
           (= (count xs) (count ws)) ]}
    (map #(/ (.getReal %1) (count xs)) (fft-complex xs ws))))

;; Helper functions
;;
(defn- power-of-two
  "An integer representing a power of two will have a single bit set in its 
   binary representation."
  [x]
  (= 1 (Integer/bitCount x)))

(defn- butterfly
  ""
  [xs ws]
  (let [[first-half second-half] (split-at (/ (count xs) 2) xs)]
    [ (map #(.add %1 %2)      
           first-half 
           second-half)
      (map #(.multiply %1 %2) 
           ws 
           (map #(.subtract %1 %2) first-half second-half)) ]))

(defn- fft-complex
  ""
  ([xs]
    {:pre [ (power-of-two (count xs)) ]}
    (fft-complex xs (generate-omega-coeffs (count xs)))) 
  
  ([xs ws]
    {:pre [ (power-of-two (count xs))
           (= (count xs) (count ws)) ]}
    (if (= 1 (count xs)) 
      xs
      (let [ sub-ws              (take-nth 2 ws)
            [first-half-xs second-half-xs]  (butterfly xs ws) ]
        (interleave (fft-complex first-half-xs sub-ws)
                    (fft-complex second-half-xs  sub-ws))))))

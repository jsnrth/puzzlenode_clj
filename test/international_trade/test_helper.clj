(ns international-trade.test-helper)

(defn abs [n] (if (< 0 n) (* -1 n) n))

(defn close [n1 n2]
  (let [p 5 d 0.00001]
    (< (abs (- (with-precision p n1) (with-precision p n2)))) d))

(ns international-trade.rates)

(declare with-inverted inverted)

(defn conversion [rates from to]
  (let [rates (with-inverted rates)
        conversion (or
                      (when (= from to) 1.0M)
                      (get rates [from to])
                      :no-conversion)]
    [conversion rates]))

(defn- with-inverted [rates]
  (let [merge-inverted
          (fn merge-inverted [m r c]
            (let [[f t] r]
              (if (contains? rates [t f])
                m
                (assoc m [t f] (inverted c)))))]
    (reduce-kv merge-inverted rates rates)))

(defn- inverted [conversion] (bigdec (/ 1.0 conversion)))

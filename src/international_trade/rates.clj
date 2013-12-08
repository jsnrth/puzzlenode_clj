(ns international-trade.rates)

(declare hashed hasher inverted)

(defn rate [rates from to]
  (or
    (when (= from to) 1.0M)
    (get (hashed rates) [from to])
    :no-conversion))

(defn- hashed [rates]
  (reduce hasher {} rates))

(defn- hasher [all rate]
  (-> all
    (assoc [(:from rate) (:to rate)] (:conversion rate))
    (assoc [(:to rate) (:from rate)] (inverted (:conversion rate)))))

(defn- inverted [conversion] (bigdec (/ 1.0 conversion)))

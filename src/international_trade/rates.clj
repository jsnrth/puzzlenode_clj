(ns international-trade.rates)

(declare derive-rates collect-froms invert-rates inverted)

(defn conversion
  ([rates from to]
    (conversion rates from to {}))
  ([rates from to derived-rates]
    (let [all-rates (merge rates derived-rates)]
      (cond
        (= from to)
          [1.0M all-rates]
        (contains? all-rates [from to])
          [(get all-rates [from to]) all-rates]
        :else
          (let [new-rates (derive-rates all-rates from to)]
            (if (not-empty new-rates)
              (conversion rates from to (merge derived-rates new-rates))
              [:no-conversion all-rates]))))))

(defn- derive-rates [rates from to]
  (let [inverted-rates (invert-rates rates)
        all-rates (merge rates inverted-rates)
        from-rates (collect-froms all-rates from)
        dfn (fn [m ft c]
              (let [[f t] ft
                    to-rates (collect-froms all-rates t)
                    dfn2 (fn [m2 ft2 c2]
                      (let [[f2 t2] ft2]
                        (cond
                          (and (not (= f t2)) (not (contains? all-rates [f t2])))
                            (assoc m2 [f t2] (bigdec (* c c2)))
                          :else
                            m2)))]
                (if (not-empty to-rates)
                  (reduce-kv dfn2 m to-rates)
                  m)))
        new-rates (reduce-kv dfn {} from-rates)]
    (merge inverted-rates new-rates)))

(defn- collect-froms [rates from]
  (let [collector
          (fn [m r c]
            (let [[f t] r]
              (if (= f from)
                (assoc m r c)
                m)))]
    (reduce-kv collector {} rates)))

(defn- invert-rates [rates]
  (let [merge-inverted
          (fn merge-inverted [m r c]
            (let [[f t] r]
              (if (contains? rates [t f])
                m
                (assoc m [t f] (inverted c)))))]
    (reduce-kv merge-inverted {} rates)))

(defn- inverted [conversion] (bigdec (/ 1.0 conversion)))

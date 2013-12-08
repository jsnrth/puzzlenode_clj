(ns international-trade.rates)

(declare
  derive-rates
  collect-froms
  new-rate?
  from-reducer
  to-reducer
  invert-rates
  inverted)

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
        new-rates (reduce-kv (from-reducer all-rates) {} from-rates)]
    (merge inverted-rates new-rates)))

(defn- collect-froms [rates from]
  (let [collector
          (fn [new-rates [f t] c]
            (if (= f from) (assoc new-rates [f t] c) new-rates))]
    (reduce-kv collector {} rates)))

(defn- from-reducer [all-rates]
  (fn [new-rates [from to] conversion]
    (let [to-rates (collect-froms all-rates to)]
      (if (not-empty to-rates)
        (reduce-kv (to-reducer all-rates from conversion) new-rates to-rates)
        new-rates))))

(defn- to-reducer [all-rates cur-from cur-conversion]
  (fn [new-rates [from to] conversion]
    (cond
      (new-rate? all-rates cur-from to)
        (assoc new-rates [cur-from to] (bigdec (* cur-conversion conversion)))
      :else
        new-rates)))

(defn- new-rate? [rates from to]
  (and
    (not (= from to))
    (not (contains? rates [from to]))))

(defn- invert-rates [rates]
  (let [merge-inverted
          (fn merge-inverted [m r c]
            (let [[f t] r]
              (if (contains? rates [t f])
                m
                (assoc m [t f] (inverted c)))))]
    (reduce-kv merge-inverted {} rates)))

(defn- inverted [conversion] (bigdec (/ 1.0 conversion)))

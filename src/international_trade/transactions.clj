(ns international-trade.transactions
  (:require [international-trade.rates :as rates]))

(declare add sum-reducer)

(defn sum [rates transactions currency]
  (let [init [0.0M rates]]
    (reduce (sum-reducer rates transactions currency) init transactions)))

(defn- sum-reducer [rates transactions currency]
  (fn [[total new-rates] transaction]
    (if (= :no-conversion total)
      [total new-rates]
      (let [tcurrency (:currency transaction)
            tamount (:amount transaction)]
        (if (= currency tcurrency)
          [(add total tamount) new-rates]
          (let [[converted newer-rates] (rates/convert new-rates tamount tcurrency currency)]
            [(add total converted) newer-rates]))))))

(defn- add [n1 n2]
  (let [N :no-conversion]
    (if (or (= N n1) (= N n2))
      N
      (+ n1 n2))))

(ns international-trade.rates-test
  (:require [clojure.test :refer :all]
            [international-trade.rates :as rates]))

(defn c [[conversion rates]] conversion)

(deftest returns-a-conversion-rate
  (let [rates {[:FOO :BAR] 1.1M
               [:BAR :BAZ] 1.2M}]
    (is (= 1.1M (c (rates/conversion rates :FOO :BAR))))
    (is (= 1.2M (c (rates/conversion rates :BAR :BAZ))))
    (is (= :no-conversion (c (rates/conversion rates :BAT :QUX))))))

(deftest computes-inverse-conversion-rates
  (let [rates {[:FOO :BAR] 1.23M}
        [conversion new-rates] (rates/conversion rates :BAR :FOO)]
    (is (= 0.8130081300813008M conversion))
    (is (= conversion (get new-rates [:BAR :FOO])))))

(deftest converts-same-currency
  (is (= 1.0M (c (rates/conversion [] :FOO :FOO)))))

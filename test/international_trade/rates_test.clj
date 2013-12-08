(ns international-trade.rates-test
  (:require [clojure.test :refer :all]
            [international-trade.rates :as rates]
            [international-trade.test-helper :refer [close]]))

(defn c [[conversion rates]] conversion)

(defn a [[amount rates]] amount)

(deftest returns-a-conversion-rate
  (let [rates {[:FOO :BAR] 1.1M
               [:BAR :BAZ] 1.2M}]
    (is (= 1.1M (c (rates/conversion rates :FOO :BAR))))
    (is (= 1.2M (c (rates/conversion rates :BAR :BAZ))))
    (is (= :no-conversion (c (rates/conversion rates :BAT :QUX))))))

(deftest derives-inverse-conversion-rates
  (let [rates {[:FOO :BAR] 1.23M}
        [conversion new-rates] (rates/conversion rates :BAR :FOO)]
    (is (close 0.813008M conversion))
    (is (= conversion (get new-rates [:BAR :FOO])))))

(deftest converts-same-currency
  (is (= 1.0M (c (rates/conversion [] :FOO :FOO)))))

(deftest derives-missing-conversion-rates
  (let [rates {[:FOO :BAR] 1.1M
               [:BAR :BAZ] 1.2M
               [:BAZ :QUX] 1.3M
               [:BAT :QUX] 1.4M
               [:AAA :BBB] 1.4M}]
    (testing "derived through other rates"
      (let [[conv new-rates] (rates/conversion rates :FOO :BAZ)]
        (is (close 1.32M conv))
        (is (contains? new-rates [:FOO :BAZ])))
      (let [[conv new-rates] (rates/conversion rates :FOO :QUX)]
        (is (close 1.716M conv))
        (is (contains? new-rates [:FOO :QUX]))))
    (testing "derived through inverse rates"
      (let [[conv new-rates] (rates/conversion rates :FOO :BAT)]
        (is (close 1.225714M conv))
        (is (contains? new-rates [:FOO :BAT]))))
    (testing "gives up when impossible"
      (is (= :no-conversion (c (rates/conversion rates :FOO :BBB)))))))

(deftest converts-an-amount-from-one-currency-to-another
  (let [rates {[:FOO :BAR] 1.1M
               [:BAR :BAZ] 1.2M
               [:BAZ :QUX] 1.3M
               [:BAT :QUX] 1.4M
               [:AAA :BBB] 1.4M}]
    (is (close 1.716M (a (rates/convert rates 1.0M :FOO :QUX))))
    (is (close 1.22571M (a (rates/convert rates 1.0M :FOO :BAT))))
    (is (= :no-conversion (a (rates/convert rates 1.0M :FOO :AAA))))))

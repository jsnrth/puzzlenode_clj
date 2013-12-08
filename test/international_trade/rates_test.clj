(ns international-trade.rates-test
  (:require [clojure.test :refer :all]
            [international-trade.rates :as rates]))

(deftest returns-a-conversion-rate
  (let [rates [{:from :FOO :to :BAR :conversion 1.23M}
               {:from :BAR :to :BAZ :conversion 2.34M}]]
    (is (= 1.23M (rates/rate rates :FOO :BAR)))
    (is (= 2.34M (rates/rate rates :BAR :BAZ)))
    (is (= :no-conversion (rates/rate rates :BAT :QUX)))))

(deftest returns-an-inverse-conversion-rate
  (let [rates [{:from :FOO :to :BAR :conversion 1.23M}]]
    (is (= 0.8130081300813008M (rates/rate rates :BAR :FOO)))))

(deftest noop-when-converting-same-currency
  (is (= 1.0M (rates/rate [] :FOO :FOO))))


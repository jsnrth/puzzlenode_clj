(ns international-trade.transactions-test
  (:require [clojure.test :refer :all]
            [international-trade.transactions :as trans]
            [international-trade.test-helper :refer [close]]))

(def rates {[:FOO :BAR] 1.01M
            [:BAR :BAZ] 0.98M
            [:BAT :QUX] 1.0M})

(defn stores [transactions]
  (vec (map :store transactions)))

(deftest converts-and-sums-a-list-of-transactions
  (let [transactions [{:store "Abc" :sku "SKU1" :amount 1.1M :currency :FOO}
                      {:store "Bcd" :sku "SKU1" :amount 1.2M :currency :BAR}
                      {:store "Def" :sku "SKU1" :amount 1.3M :currency :BAZ}]]
    (testing "derived conversions"
      (let [[amount new-rates] (trans/sum rates transactions :FOO)]
        (is (close 3.59874M amount))
        (is (contains? new-rates [:BAR :FOO]))
        (is (contains? new-rates [:BAZ :FOO])))
      (let [[amount new-rates] (trans/sum rates transactions :BAR)]
        (is (close 3.63753M amount))
        (is (contains? new-rates [:BAZ :BAR]))))
    (testing "gives up when cannot convert"
      (let [[amount _] (trans/sum rates transactions :QUX)]
        (is (= :no-conversion amount))))))

(deftest filters-transactions-by-sku
  (let [transactions [{:store "Abc" :sku "SKU1" :amount 1.1M :currency :FOO}
                      {:store "Bcd" :sku "SKU2" :amount 1.2M :currency :BAR}
                      {:store "Def" :sku "SKU1" :amount 1.3M :currency :BAZ}]]
    (is (= ["Abc" "Def"] (stores (trans/sku "SKU1" transactions))))
    (is (= ["Bcd"] (stores (trans/sku "SKU2" transactions))))
    (is (= [] (stores (trans/sku "WAT" transactions))))))

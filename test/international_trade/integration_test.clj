(ns international-trade.integration-test
  (:require [clojure.test :refer :all]
            [international-trade.transactions :as trans]
            [international-trade.test-helper :refer [close]]))

(def rates {[:AUD :CAD] 1.0079M
            [:CAD :USD] 1.0090M
            [:USD :CAD] 0.9911M})

(def transactions [{:store "Yonkers"   :sku "DM1210"  :amount 70.00M  :currency :USD}
                   {:store "Yonkers"   :sku "DM1182"  :amount 19.68M  :currency :AUD}
                   {:store "Nashua"    :sku "DM1182"  :amount 58.58M  :currency :AUD}
                   {:store "Scranton"  :sku "DM1210"  :amount 68.76M  :currency :USD}
                   {:store "Camden"    :sku "DM1182"  :amount 54.64M  :currency :USD}])

(deftest correctly-totals-the-amount
  (let [[total new-rates] (trans/sum rates (trans/sku "DM1182" transactions) :USD)]
    (is (close 134.22M total))
    (is (contains? new-rates [:AUD :USD]))))

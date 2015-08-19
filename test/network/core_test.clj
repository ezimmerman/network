(ns network.core1-test
  (:require [clojure.test :refer :all]
            [network.networkcore1 :refer :all]))


(deftest test-store-update-utilities
  (testing "given a store, that it will now include a utiltiey value"
    (let [store (get-in indexes [:stores-index :s0])]
      (is (= 1.0 (:utility (update-utility store)))))))

(deftest test-warehouse-utilities
  (testing "given a warehouse, that it will now include a utility equal to the highest utility of a store in it's collection"
    (let [util-index (assoc-in indexes [:stores-index :s0] (update-utility (get-in indexes [:stores-index :s0])))
          warehouse  (get-in indexes [:warehouses-index :w0])]
      (is (= 1.0 (:utility (update-utility warehouse util-index)))))))

(deftest test-vendor-utilities
  (testing "given a vendor, that it will now include a utility equal to the highest utility of a warehouse in it's collection"
    (let [store-util-index (assoc-in indexes [:stores-index :s0] (update-utility (get-in indexes [:stores-index :s0])))
          warehouse-util-index (assoc-in store-util-index [:warehouses-index :w0] (update-utility (get-in store-util-index [:warehouses-index :w0]) store-util-index))
          vendor  (get-in warehouse-util-index [:vendor-index :v0])]
      (is (= 1.0 (:utility (update-utility vendor warehouse-util-index)))))))

(deftest test-include-utilities
  (testing "test we populate all the utilities"
    (let [with-utlilities-indexes (include-utilities indexes)]
    (is (= 1.0 (:utility (get-in with-utlilities-indexes [:vendor-index :v0])))))))
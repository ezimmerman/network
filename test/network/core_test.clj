(ns network.core1-test
  (:require [clojure.test :refer :all]
            [network.networkcore1 :refer :all]))

(def stores-index {:s0 (make-product-store "store0" :target-inventory 5) :s1 (make-product-store "store1" :target-inventory 10) :s2 (make-product-store "store2" :target-inventory 0)})
(def warehouses-index {:w0 (make-product-warehouse "warehouse0" (keys stores-index))})
(def vendor-index {:v0 (make-product-vendor "vendor0" (keys warehouses-index))})
(def indexes {:vendor-index vendor-index :warehouses-index warehouses-index :stores-index stores-index})

(deftest test-store-update-utilities
  (testing "given a store, that it will now include a utiltiey value"
    (let [store (get-in indexes [:stores-index :s0])]
      (is (= 1.0 (:utility (update-utility store)))))))

(deftest test-warehouse-utilities
  (testing "given a warehouse, that it will now include a utility equal to the highest utility of a store in it's collection"
    (let [util-index (assoc-in indexes [:stores-index :s0] (update-utility (get-in indexes [:stores-index :s0])))
          warehouse (get-in indexes [:warehouses-index :w0])]
      (is (= 1.0 (:utility (update-utility warehouse util-index)))))))

(deftest test-vendor-utilities
  (testing "given a vendor, that it will now include a utility equal to the highest utility of a warehouse in it's collection"
    (let [store-util-index (assoc-in indexes [:stores-index :s0] (update-utility (get-in indexes [:stores-index :s0])))
          warehouse-util-index (assoc-in store-util-index [:warehouses-index :w0] (update-utility (get-in store-util-index [:warehouses-index :w0]) store-util-index))
          vendor (get-in warehouse-util-index [:vendor-index :v0])]
      (is (= 1.0 (:utility (update-utility vendor warehouse-util-index)))))))

(deftest test-include-utilities
  (testing "test we populate all the utilities"
    (let [with-utlilities-indexes (include-utilities indexes)]
      (is (= 1.0 (:utility (get-in with-utlilities-indexes [:vendor-index :v0]))))
      (is (= 1.0 (:utility (get-in with-utlilities-indexes [:warehouses-index :w0]))))
      (is (= 1.0 (:utility (get-in with-utlilities-indexes [:stores-index :s0]))))
      )))

(deftest test-highest-utility
  (testing "test that a map of key to integer value returns the key with the highest integer value"
    (let [warehouses-index (:warehouses-index (include-utilities indexes))
          stores-index (:stores-index (include-utilities indexes))]
      (is (= :w0 (highest-utility warehouses-index)))
      (is (= :s1 (highest-utility stores-index))))))

(deftest test-flow-index
  (testing "make sure we get back a index that has the entity updated"
    (let [warehouses-index (:warehouses-index (include-utilities indexes))
          stores-index (:stores-index (include-utilities indexes))]
      (is (= 1 (:final-order (:s1 (flow-index stores-index)))))
      (is (= 1 (:existing-inventory (:s1 (flow-index stores-index)))))
      (is (= 1 (:final-order (:w0 (flow-index warehouses-index)))))
      )))

(deftest test-flow-indexes
  (testing "Flow all the indexes to get their final orders incremmented or for stores existing-inventory incremmented as well."
    (let [flowed-indexes (flow-indexes (include-utilities indexes))
          vendor-index (:vendor-index flowed-indexes)
          warehouses-index (:warehouses-index flowed-indexes)
          stores-index (:stores-index flowed-indexes)]
      (is (= 1 (:final-order (:v0 vendor-index))))
      (is (= 1 (:final-order (:w0 warehouses-index))))
      (is (= 1 (:final-order (:s1 stores-index))))
      (is (= 1 (:existing-inventory (:s1 stores-index))))
      )))

(deftest test-flow
  (testing "test the end result for the indexes data structure"
    (let [flow-indexes (flow indexes)]
      (is (= 8  (get-in flow-indexes [:vendor-index :v0 :final-order]))))))
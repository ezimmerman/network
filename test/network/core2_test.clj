(ns network.core2-test
  (:require [clojure.test :refer :all]
            [network.networkcore2 :refer :all]))

(def vendor (make-vendor "vendor0" #{(make-warehouse "warehouse0" #{(make-store "store0" :target-inventory 5)
                                                                  (make-store "store1" :target-inventory 10)
                                                                  (make-store "store2" :target-inventory 0)})}))

(deftest test-include-utility
  (testing "test that given a vendor, we calc utilities all the way down"
    (is (= 1.0 (get-utility (include-utility vendor))))))

(deftest test-flow-sinks
  (testing "test that given a set of sinks, we find the one with the greatest utility and flow it and return the new set"
    (is (= 1 (get-final-order (first (flow-sinks (get-sinks (include-utility vendor)))))))))

(deftest test-reset-utilities
  (testing "test that reset-utility will reset that entity and it's sinks"
    (let [vendor-with-utility (include-utility vendor)
          vendor-utility-reset (reset-utility vendor-with-utility)]
      (is (= 1.0 (get-utility vendor-with-utility)))
      (is (= nil (get-utility vendor-utility-reset))))))

(deftest test-flow
  (testing "test that flow updates the entity correctly"
    (is (= 8 (get-final-order (flow vendor))))))
(ns network.networkcore2
  (:gen-class)
  (:require [schema.core :as s])
  (:require [medley.core :as m]))

(s/defrecord Store [name :- String
                    existing-inventory :- s/Int
                    target-inventory :- s/Int
                    final-order :- s/Int])
(s/defrecord Warehouse [name :- String
                        final-order :- s/Int
                        stores :- [Store]])
(s/defrecord Vendor [name :- String
                     final-order :- s/Int
                     warehouses :- [Warehouse]])

(defn- make-vendor [name warehouses]
  (->Vendor name
            0
            warehouses))

(defn- make-store [name & opts]
  (let [{:keys [existing-inventory
                target-inventory] :or {existing-inventory 0 target-inventory 0}} opts] ;; default to 0
    (->Store name
             existing-inventory
             target-inventory
             0)))

(defn- make-warehouse [name product-stores]
  (->Warehouse name
               0
               product-stores))

(defn- get-final-order [entity]
  (:final-order entity))

(defn- get-existing-inventory [store]
  (:existing-inventory store))

(defn- get-stores [map]
  (:stores map))

(defn- get-warehouses [vendor]
  (:warehouses vendor))

(defn- get-target-inventory [store]
  (:target-inventory store))

(defn- get-entity-utility [entity]
  (:utility entity))

(defn- reset-entity-utility [entity]
  (dissoc entity :utility))

(defn- highest-util-entity [entity0 entity1]
  (if (> (:utility entity0) (:utility entity1)) entity0 entity1))

(defn- entity-equals [entity0]
  (fn [entity1]
    (= (:name entity0) (:name entity1))))

(defn- replace-entity [entity seq]
  (conj (remove (entity-equals entity) seq) entity))


(defprotocol Product-utility (utility [entity]))
(defprotocol Flow (flow-one-pack [entity]))
(defprotocol Include-Utility (include-utility [entity]))
(defprotocol Reset-utility (reset-utility [entity]))

(defn- reset-utilities [entity children-key]
  (assoc entity children-key(map reset-utility (children-key entity))))

(defn- include-utilities [entity children-key child-fn]
  (assoc entity children-key (map utility (child-fn entity))))

(extend-protocol Reset-utility
  Store
  (reset-utility [store]
    (reset-entity-utility [store])))

(extend-protocol Reset-utility
  Warehouse
  (reset-utility [warehouse]
    (reset-utilities warehouse :stores)))

(extend-protocol Reset-utility
  Vendor
  (reset-utility [vendor]
    (reset-utilities vendor :warehouses)))

(extend-protocol Include-Utility
  Warehouse
  (include-utility [warehouse]
    (include-utilities warehouse :stores get-stores)))

(extend-protocol Include-Utility
  Vendor
  (include-utility [vendor]
    (include-utilities vendor :warehouses get-warehouses)))

(extend-protocol Product-utility
  Store
  (utility [store]
    (let [pack (+ (get-final-order store) (get-existing-inventory store))
          end  (get-target-inventory store)]
      (assoc store :utility (if (< pack end) 1.0 (* (/ 1.0 pack) -1.0))))))

(extend-protocol Product-utility
  Warehouse
  (utility [warehouse]
    (let [warehouse-with-utilities (include-utility warehouse)]
      (assoc warehouse-with-utilities :utility (reduce #(max (:utility %2) %) 0 (:stores warehouse-with-utilities))))))

(extend-protocol Product-utility
  Vendor
  (utility [vendor]
    (let [vendor-with-utilities (include-utility vendor)]
      (assoc vendor-with-utilities :utility (reduce #(max (:utility %2) %) 0 (:warehouses vendor-with-utilities))))))

(extend-protocol Flow
  Store
  (flow-one-pack [store]
    (update (update store :final-order inc) :existing-inventory inc)))

(extend-protocol Flow
  Warehouse
  (flow-one-pack [warehouse]
    (let [updated-warehouse (update warehouse :final-order inc)]
      (assoc updated-warehouse :stores (replace-entity (flow-one-pack (reduce highest-util-entity (:stores updated-warehouse))) (:stores updated-warehouse))))))


(extend-protocol Flow
  Vendor
  (flow-one-pack [vendor]
    (let [updated-vendor (update vendor :final-order inc)]
      (assoc updated-vendor :warehouses (replace-entity (flow-one-pack (reduce highest-util-entity (:warehouses updated-vendor))) (:warehouses updated-vendor))))))




(def vendor (make-vendor "vendor0" [(make-warehouse "warehouse0" [(make-store "store0" :target-inventory 5)
                                                                  (make-store "store1" :target-inventory 10)
                                                                  (make-store "store2" :target-inventory 0)])]))


(defn flow [vendor]
  (loop [flowed-vendor vendor]
    ;Get the utilities first.  We'll assoc the utility value onto the objects, so the data structure will be the same
    ;as nodes but with the utility added.
    ; If the vendor has positive utility we need to order.
    (let [utilities-vendor (utility flowed-vendor)]
      (if (<= (get-entity-utility utilities-vendor) 0)
        (println utilities-vendor)
        ;We'll recur until the vendor doesn't have positive utility for that pack.
        (recur (flow-one-pack utilities-vendor)))))
  )

(defn -main
  [& args]
  )
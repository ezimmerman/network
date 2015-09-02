(ns network.networkcore2
  (:gen-class)
  (:require [schema.core :as s])
  (:require [medley.core :as m]))

; This approach is a nested relationship approach.  Vendors have a hashset of warehouses
; warehouses have a hashset of stores.

(s/defrecord Store [name :- String
                    existing-inventory :- s/Int
                    target-inventory :- s/Int
                    final-order :- s/Int])
(s/defrecord Warehouse [name :- String
                        final-order :- s/Int
                        sinks :- #{Store}])
(s/defrecord Vendor [name :- String
                     final-order :- s/Int
                     sinks :- #{Warehouse}])

(defn make-vendor [name sinks]
  (->Vendor name
            0
            sinks))

(defn make-store [name & opts]
  (let [{:keys [existing-inventory
                target-inventory] :or {existing-inventory 0 target-inventory 0}} opts] ;; default to 0
    (->Store name
             existing-inventory
             target-inventory
             0)))

(defn make-warehouse [name sinks]
  (->Warehouse name
               0
               sinks))

(defn get-final-order [entity]
  (:final-order entity))

(defn get-existing-inventory [store]
  (:existing-inventory store))

(defn get-sinks [map]
  (:sinks map))

(defn get-target-inventory [store]
  (:target-inventory store))

(defn get-name
  [entity]
  (:name entity))

(defn get-utility [entity]
  (:utility entity))

(defn reset-entity-utility [entity]
  (dissoc entity :utility))

(defn highest-util-entity [entity0 entity1]
  (if (> (get-utility entity0) (get-utility entity1)) entity0 entity1))


(defprotocol Include-utility (include-utility [entity]))
(defprotocol Flow (flow-one-pack [entity]))
(defprotocol Reset-utility (reset-utility [entity]))

;Reset utilities
(defn reset-utilities [entity-col]
  (map reset-utility entity-col))

(extend-protocol Reset-utility
  Store
  (reset-utility [store]
    (reset-entity-utility store)))

(extend-protocol Reset-utility
  Warehouse
  (reset-utility [warehouse]
    (reset-entity-utility (assoc warehouse :sinks (reset-utilities (get-sinks warehouse))))))

(extend-protocol Reset-utility
  Vendor
  (reset-utility [vendor]
    (reset-entity-utility (assoc vendor :sinks (reset-utilities (get-sinks vendor))))))


; Include-utility
(extend-protocol Include-utility
  Store
  (include-utility [store]
    (let [pack (+ (get-final-order store) (get-existing-inventory store))
          end (get-target-inventory store)]
      (assoc store :utility (if (< pack end) 1.0 (* (/ 1.0 pack) -1.0))))))

(extend-protocol Include-utility
  Warehouse
  (include-utility [warehouse]
    (let [warehouse-with-stores-set (assoc warehouse :sinks (map include-utility (get-sinks warehouse)))]
      (assoc warehouse-with-stores-set :utility (reduce #(max (get-utility %2) %) 0 (get-sinks warehouse-with-stores-set))))))

(extend-protocol Include-utility
  Vendor
  (include-utility [vendor]
    (let [vendor-with-warehoueses-set (assoc vendor :sinks (map include-utility (get-sinks vendor)))]
      (assoc vendor-with-warehoueses-set :utility (reduce #(max (get-utility %2) %) 0 (get-sinks vendor-with-warehoueses-set))))))

; Flow

(defn entity-equals [entity0]
  (fn [entity1]
    (= (get-name entity0) (get-name entity1))))

(defn replace-entity [entity seq]
  (conj (remove (entity-equals entity) seq) entity))

(defn flow-sinks
  [sinks]
  (replace-entity (flow-one-pack (reduce highest-util-entity sinks)) sinks))

(extend-protocol Flow
  Store
  (flow-one-pack [store]
    (update (update store :final-order inc) :existing-inventory inc)))

(extend-protocol Flow
  Warehouse
  (flow-one-pack [warehouse]
    (let [warehouse-with-store-flowed (assoc warehouse :sinks (flow-sinks (get-sinks warehouse)))]
      (update warehouse-with-store-flowed :final-order inc))))


(extend-protocol Flow
  Vendor
  (flow-one-pack [vendor]
    (let [vendor-with-warehouse-flowed (assoc vendor :sinks (flow-sinks (get-sinks vendor)))]
      (reset-utility (update vendor-with-warehouse-flowed :final-order inc)))))


(defn flow [vendor]
  (loop [flowed-vendor vendor]
    ;Get the utilities first.  We'll assoc the utility value onto the objects, so the data structure will be the same
    ;as nodes but with the utility added.
    ; If the vendor has positive utility we need to order.
    (let [utilities-vendor (include-utility flowed-vendor)]
      (if (<= (get-utility utilities-vendor) 0)
        utilities-vendor
        ;We'll recur until the vendor doesn't have positive utility for that pack.
        (recur (flow-one-pack utilities-vendor)))))
  )


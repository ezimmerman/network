(ns network.networkcore1
  (:require [schema.core :as s])
  (:require [medley.core :as m]))

; This approach takes the index approach.  Vendors hold on to an index of their warehouses, and warehouses their stores.
; The index is a key to the actual entity.  This approach is different from the direct nesting of the relationship. Say
; having a vendor have a vector of warehouses directly.

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

(defn make-product-vendor [name warehouses]
  (->Vendor name
            0
            warehouses))

(defn make-product-store [name & opts]
  (let [{:keys [existing-inventory
                target-inventory] :or {existing-inventory 0 target-inventory 0}} opts] ;; default to 0
    (->Store name
             existing-inventory
             target-inventory
             0)))

(defn make-product-warehouse [name product-stores]
  (->Warehouse name
               0
               product-stores))

; functions to work against data structure

(defprotocol Product-utility (product-utility [entity] [entity indexes]))
(defprotocol Flow-pack (flow-pack [entity]))

(defn get-final-order [entity]
  (:final-order entity))

(defn get-existing-inventory [store]
  (:existing-inventory store))

(defn get-stores [map]
  (:stores map))

(defn get-warehouses [vendor]
  (:warehouses vendor))

(defn get-target-inventory [store]
  (:target-inventory store))

(defn get-utility [entity]
  (if (nil? (:utility entity)) 0.0 (:utility entity)))

(extend-protocol Product-utility
  Store
  (product-utility [store]
    (let [pack (+ (get-final-order store) (get-existing-inventory store))
          end (get-target-inventory store)]
      (if (< pack end) 1.0 (* (/ 1.0 pack) -1.0)))))

(extend-protocol Product-utility
  Warehouse
  (product-utility [warehouse indexes]
    (reduce #(max (get-utility (%2 (:stores-index indexes))) %) 0 (get-stores warehouse))))

(extend-protocol Product-utility
  Vendor
  (product-utility [vendor indexes]
    (reduce #(max (get-utility (%2 (:warehouses-index indexes))) %) 0 (get-warehouses vendor))))

(extend-protocol Flow-pack
  Store
  (flow-pack [store]
    (update (update store :existing-inventory inc) :final-order inc)))

(extend-protocol Flow-pack
  Warehouse
  (flow-pack [warehouse]
    (update warehouse :final-order inc)))

(extend-protocol Flow-pack
  Vendor
  (flow-pack [vendor]
    (update vendor :final-order inc)))

(defn update-utility
  "add the utility to the entity"
  ([entity] (assoc entity :utility (product-utility entity)))
  ([entity indexes] (assoc entity :utility (product-utility entity indexes))))

(defn include-utilities
  "using the stores, warehouses and vendor indexes, add the utility calculations for the potential pack"
  [indexes]
  (let [stores-with-utilities (assoc indexes :stores-index (m/map-vals update-utility (:stores-index indexes)))
        warehouses-with-utilities (assoc stores-with-utilities :warehouses-index (m/map-vals #(update-utility % stores-with-utilities) (:warehouses-index stores-with-utilities)))]
    (assoc warehouses-with-utilities :vendor-index (m/map-vals #(update-utility % warehouses-with-utilities) (:vendor-index warehouses-with-utilities)))))

(defn highest-utility
  "map of key value pairs, highest value of the value k is the key to get the children"
  [m]
  (reduce #(if (> (:utility (% m)) (:utility (%2 m))) % %2) (keys m)))

(defn flow-index
  "after flowing an index the entity in the index with the highest utility will have a final order and if a store, an existing-inventory"
  [index]
  (let [highest-utility-key (highest-utility index)]
    (assoc index highest-utility-key (flow-pack (highest-utility-key index)))))

(defn flow-indexes
  "Given an index, which is a map of :vendor-index to vendor, :warehouses-indexs to warehouses, and :stores-index to stores."
  [indexes]
  (m/map-vals flow-index indexes))


(defn flow [indexes]
  (loop [flowed-indexes indexes]
    ;Include the utilities on the entities for the next possible pack
    (let [flowed-indexes (include-utilities flowed-indexes)
          v0 (:v0 (:vendor-index flowed-indexes))]
      ; If the vendor has positive utility we need to order.
      ; IF not we are done!  Return the map with information about final orders etc.
      (if (<= (:utility v0) 0)
        flowed-indexes
        ;We'll recur until the vendor doesn't have positive utility for that pack.
        (recur (flow-indexes flowed-indexes)))))
  )


(ns network.core
  (:gen-class)
  (:require [schema.core :as s])
  (:require [medley.core :as m]))

(s/defrecord Store [name :- String
                           existing-inventory :- s/Int
                           target-inventory :- s/Int
                           final-order :- s/Int ])
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
(def stores-index {:s0 (make-product-store "store0" :target-inventory 5) :s1 (make-product-store "store1" :target-inventory 10) :s2 (make-product-store "store2" :target-inventory 0)})
(def warehouses-index {:w0 (make-product-warehouse "warehouse0" (keys stores-index))})
(def vendors-index {:v0 (make-product-vendor "vendor0" (keys warehouses-index))})

; functions to work against data structure

(defprotocol Product-utility (product-utility [entity] [entity utilities-map]))

(defn get-final-order [entity]
  (:final-order entity))

(defn get-existing-inventory [store]
  (:existing-inventory store))

(defn get-stores [warehouse]
  (:stores warehouse))

(defn get-warehouses [vendor]
  (:warehouses vendor))

(defn get-target-inventory [store]
  (:target-inventory store))

(defn is-vendor-key? [k]
  (if (= (subs (name k) 0 1)  "v")  true false))

(defn get-vendor-utility [utilities]
  (reduce #(if
            (is-vendor-key? %2)
            (max (%2 (apply hash-map utilities)) %)
            %)
          0 (keys (apply hash-map utilities)))
  )

(extend-protocol Product-utility
  Store
  (product-utility [store]
    (let [pack (+ (get-final-order store) (get-existing-inventory store))
          end (get-target-inventory store)]
      (if (< pack end) 1.0 (* (/ 1.0 pack) -1.0)))))

(extend-protocol Product-utility
  Warehouse
  (product-utility [warehouse utilities]
    (reduce #(max  (%2 (apply hash-map utilities)) %) 0 (get-stores warehouse))))

(extend-protocol Product-utility
  Vendor
  (product-utility [vendor utilities]
    (reduce #(max  (%2 (apply hash-map utilities)) %) 0 (get-warehouses vendor))))

(defn include-utility
  ([index]
   (mapcat #(vector (key %) (product-utility (val %))) index))
  ([index utilities]
   (mapcat #(vector (key %) (product-utility (val %) utilities)) index))
  )

(defn get-utilities [stores warehouses vendor]
  (let [stores-index-to-utility (include-utility stores)
        warehouses-index-to-utilities (include-utility warehouses stores-index-to-utility)
        vendor-index-with-utilities (include-utility vendor warehouses-index-to-utilities)]
    (concat vendor-index-with-utilities warehouses-index-to-utilities stores-index-to-utility)
    )
  )

(defn flow []
  (let [utilities (get-utilities stores-index warehouses-index vendors-index)]
    (if (> (get-vendor-utility utilities) 0)
      (print utilities)
      (recur (flow-pack utilities))))
  )

(defn -main
  [& args]
  (flow )
  )

(ns network.networkcore1
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
(def nodes {:stores {:s0 (make-product-store "store0" :target-inventory 5) :s1 (make-product-store "store1" :target-inventory 10) :s2 (make-product-store "store2" :target-inventory 0)}
            :warehouses {:w0 (make-product-warehouse "warehouse0" (keys stores-index))}
            :vendor {:v0 (make-product-vendor "vendor0" (keys warehouses-index))}})



; functions to work against data structure

(defprotocol Product-utility (product-utility [entity] [entity utilities-map]))
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

(defn is-vendor-key? [k]
  (if (= (subs (name k) 0 1)  "v")  true false))

(defn is-store-key? [k]
  (if (= (subs (name k) 0 1)  "s")  true false))

(defn get-vendor-utility [utilities]
  (reduce #(if
            (is-vendor-key? %2)
            (max (%2 utilities) %)
            %)
          0 (keys utilities))
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

(extend-protocol Flow-pack
  Store
  (flow-pack [store]
    (update (update store :existing-inventory inc) :final-order inc)))

(extend-protocol Flow-pack
  Vendor
  (flow-pack [vendor]
    (update vendor :final-order inc)))

(defn get-utilities "return utility values for stores, warehouses, and vendor as a seq."[{:keys [stores warehouses vendor]}]
  (let [stores-index-to-utility (include-utility stores)
        warehouses-index-to-utilities (include-utility warehouses stores-index-to-utility)
        vendor-index-with-utilities (include-utility vendor warehouses-index-to-utilities)]
    (apply hash-map (concat vendor-index-with-utilities warehouses-index-to-utilities stores-index-to-utility))
    )
  )


(defn update-index [index]
  "take an index say stores and update the inventory of the individual stores returns a map"
    (into {} (map (fn [[key val]] [key  (flow-pack val )]) index)))


(defn- flow-one-pack [utilities nodes]
  "take a map of nodes and incrementes the existing inv for only the stores in the utilities col and returns nodes updated"
  (let [stores-only (m/filter-keys is-store-key? utilities)
        store-index-to-flow (reduce (fn [[k1 v1] [k2 v2]] (if (> v1 v2) [k1 v1] [k2 v2]))  stores-only)
        inc-store (flow-pack (get-in nodes [:stores (first store-index-to-flow)]))
        nodes-stores-inc (assoc-in nodes [:stores (first store-index-to-flow)] inc-store)
        inc-vendor (update-index vendors-index)]
    (into {} (map (fn [[k v]] (assoc-in nodes-stores-inc [:vendor k] v)) inc-vendor))))

(defn flow [nodes]
  (loop [flowed-nodes nodes]
    ;Get the utilities first.  Looks like {:v0 1.0 :w0 1.0 :s0 1.0 :s2 0 :s3 1.0}
    (let [utilities (get-utilities flowed-nodes)]
      ; If the vendor has positive utility we need to order.
      (if (<= (get-vendor-utility utilities) 0)
        (print utilities flowed-nodes)
        ;We'll recur until the vendor doesn't have positive utility for that pack.
        (recur (flow-one-pack utilities flowed-nodes)))))
  )


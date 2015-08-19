(ns ^:figwheel-always ipnet.core
    (:require [reagent.core :as reagent :refer [atom]]
              [clojure.string :as s]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(def zero-32 "00000000000000000000000000000000")
(def one-32 "11111111111111111111111111111111")


(defn input-component [param]
  [:input {:type :text :value (get-in @app-state [:input param])
           :on-change (fn [e]
                        (swap! app-state assoc-in [:input param]
                               (.-target.value e))
                        (swap! app-state assoc
                               :address nil
                               :address-bin nil
                               :network-mask nil
                               :network-mask-bin nil
                               :wildcard nil
                               :wildcard-bin nil))}])

(defn addr-to-octets [addr]
  (vec (map #(js/parseInt %) (s/split addr "."))))

(defn bit-str-to-octets [bstr]
  (vec (map #(s/join "" %) (partition 8 bstr))))

(defn bit-str-to-dec [s]
  (js/parseInt s 2))

(defn dec-to-8-bit-str [s]
  (let [bstr (.toString s 2)
        cnt (count bstr)]
    (if (< cnt 8)
      (str (subs zero-32 0 (- 8 cnt)) bstr)
      bstr)))

(defn bit-octets-to-dec-octets [[a b c d]]
  [(bit-str-to-dec a)
   (bit-str-to-dec b)
   (bit-str-to-dec c)
   (bit-str-to-dec d)])

(defn dec-octets-to-bit-octets [[a b c d]]
  [(dec-to-8-bit-str a)
   (dec-to-8-bit-str b)
   (dec-to-8-bit-str c)
   (dec-to-8-bit-str d)])

(defn bit-octets-to-bit-str [octets]
  (s/join "" octets))

(defn cidr-bits [bit-cnt]
  (s/join "" [(subs one-32 0 bit-cnt)
              (subs zero-32 0 (- 32 bit-cnt))]))

(defn as-address [octets]
  (s/join "." octets))

(defn as-binary [octets]
  (s/join " " octets))

(defn get-address []
  (let [addr-octets (addr-to-octets (get-in @app-state [:input :address]))]
    (swap! app-state assoc :result
           {:address addr-octets
            :address-bin (dec-octets-to-bit-octets addr-octets)})))

(defn get-netmask []
  (let [bits (js/parseInt (get-in @app-state [:input :cidr-bits]))
        net-mask (s/join "" [(subs one-32 0 bits)
                             (subs zero-32 0 (- 32 bits))])]
    (swap! app-state assoc-in [:result :cidr-bits] bits)
    (swap! app-state assoc-in [:result :netmask]
           (bit-octets-to-dec-octets (bit-str-to-octets net-mask)))
    (swap! app-state assoc-in [:result :netmask-bin]
           (bit-str-to-octets net-mask))))

(defn get-wildcard []
  (let [cidr-bits (get-in @app-state [:result :cidr-bits])
        bits (s/join "" [(subs zero-32 0 cidr-bits)
                         (subs one-32 0 (- 32 cidr-bits))])]
    (swap! app-state assoc-in [:result :wildcard]
           (bit-octets-to-dec-octets (bit-str-to-octets bits)))
    (swap! app-state assoc-in [:result :wildcard-bin]
           (bit-str-to-octets bits))))

(defn get-network []
  (let [bits (bit-octets-to-bit-str (get-in @app-state [:result :address-bin]))
        cidr-bits (get-in @app-state [:result :cidr-bits])
        net-bin (bit-str-to-octets
                 (s/join "" [(subs bits 0 cidr-bits)
                             (subs zero-32 0 (- 32 cidr-bits))]))]
    (swap! app-state assoc-in [:result :network-bin] net-bin)
    (swap! app-state assoc-in [:result :network]
           (bit-octets-to-dec-octets net-bin))))

(defn get-host-min []
  (let [bits (bit-octets-to-bit-str (get-in @app-state [:result :network-bin]))
        cidr-bits (get-in @app-state [:result :cidr-bits])
        host (bit-str-to-octets (s/join "" [(subs bits 0 cidr-bits)
                                            (subs zero-32 0 (- 32 cidr-bits 1))
                                            "1"]))]
    (swap! app-state assoc-in [:result :host-min-bin] host)
    (swap! app-state assoc-in [:result :host-min]
           (bit-octets-to-dec-octets host))))

(defn get-host-max []
  (let [bits (bit-octets-to-bit-str (get-in @app-state [:result :network-bin]))
        cidr-bits (get-in @app-state [:result :cidr-bits])
        host (bit-str-to-octets (s/join "" [(subs bits 0 cidr-bits)
                                            (subs one-32 0 (- 32 cidr-bits 1))
                                            "0"]))]
    (swap! app-state assoc-in [:result :host-max-bin] host)
    (swap! app-state assoc-in [:result :host-max]
           (bit-octets-to-dec-octets host))))

(defn get-max-host [net host-bits]
  (let [bits (bit-octets-to-bit-str (dec-octets-to-bit-octets net))]
    (bit-octets-to-dec-octets (bit-str-to-octets
                               (s/join "" [(subs bits 0 (- 32 host-bits))
                                           (subs one-32 0 (- host-bits 1))
                                           "0"])))))

(defn calculate []
  (get-address)
  (get-netmask)
  (get-wildcard)
  (get-network)
  (get-host-min)
  (get-host-max))

(defn ipcalc-page []
  [:div
   [:h1 "IP Network Calculator"]
   [:p "Address: "
    (input-component :address)]
   [:p "CIDR Bits: "
    (input-component :cidr-bits)]
   [:button {:type "button"
             :on-click (fn []
                         (calculate))}
    "Calculate"]
   (into [:table]
         (for [i [:address :netmask
                  :wildcard :network :host-min :host-max]]
           [:tr
            [:td (name i)]
            [:td (as-address (get-in @app-state [:result i]))]
            [:td (as-binary
                  (get-in @app-state [:result (keyword
                                               (str (name i) "-bin"))]))]]))])

(reagent/render-component [ipcalc-page]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


(ns ^:figwheel-always ipnet.core
    (:require [reagent.core :as reagent :refer [atom]]
              [clojure.string :as s]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

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

(defn dec-to-8-bit-str [s]
  (let [bstr (.toString s 2)
        cnt (count bstr)]
    (if (< cnt 8)
      (reduce #(str %2 %1) bstr (take (- 8 cnt) zero-32))
      bstr)))

(defn bit-str-to-dec [s]
  (js/parseInt s 2))

(defn addr-to-octets [addr]
  (vec (map #(js/parseInt %) (s/split addr "."))))

(defn bin-to-dec-octets [[a b c d]]
  [(bit-str-to-dec a)
   (bit-str-to-dec b)
   (bit-str-to-dec c)
   (bit-str-to-dec d)])

(defn octets-to-binary-string [[a b c d]]
  [(dec-to-8-bit-str a)
   (dec-to-8-bit-str b)
   (dec-to-8-bit-str c)
   (dec-to-8-bit-str d)])

(defn bit-str-to-octets [bstr]
  (vec (map #(s/join "" %) (partition 8 bstr))))

(defn cidr-bits [bit-cnt]
  (s/join "" [(reduce #(str %1 %2) "" (take bit-cnt one-32))
              (reduce #(str %1 %2) "" (take (- 32 bit-cnt) zero-32))]))

(defn wildcard-bits [bit-cnt]
  (s/join "" [(reduce #(str %1 %2) "" (take bit-cnt zero-32))
              (reduce #(str %1 %2) "" (take (- 32 bit-cnt) one-32))]))
(defn get-cidr [bits]
  (vec (map bit-str-to-dec (bit-str-to-octets (cidr-bits bits)))))

(defn get-wildcard [bits]
  (vec (map bit-str-to-dec (bit-str-to-octets (wildcard-bits bits)))))

(defn calculate []
  (let [octets (addr-to-octets (get-in @app-state [:input :address]))
        cidr-bits (js/parseInt (get-in @app-state [:input :cidr-bits]))
        cidr (get-cidr cidr-bits)
        wildcard (get-wildcard cidr-bits)]
    (println (str "Octets: " octets))
    (println (str "CIDR: " cidr))
    (swap! app-state assoc
           :address (s/join "." octets)
           :address-bin (s/join " " (octets-to-binary-string octets))
           :network-mask (s/join "." cidr)
           :network-mask-bin (s/join " " (octets-to-binary-string cidr))
           :wildcard (s/join "." wildcard)
           :wildcard-bin (s/join " " (octets-to-binary-string wildcard)))))

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
   [:table
    [:tr
     [:td "Address"]
     [:td (get-in @app-state [:input :address])]
     [:td (get @app-state :address-bin)]]
    [:tr
     [:td "Network Mask (" (get-in @app-state [:input :cidr-bits]) ")"]
     [:td (get @app-state :network-mask)]
     [:td (get @app-state :network-mask-bin)]]
    [:tr
     [:td "Wildcard"]
     [:td (get @app-state :wildcard)]
     [:td (get @app-state :wildcard-bin)]]]])

(reagent/render-component [ipcalc-page]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


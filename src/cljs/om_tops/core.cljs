(ns om-tops.core
  (:require
    [cljs.reader :as reader]
    [goog.events :as events]
    [om.core :as om :include-macros true]
    [om.dom :as dom :include-macros true]
    [ankha.core :as ankha])
  (:import
    [goog.net XhrIo]
    goog.net.EventType
    [goog.events EventType]))

(enable-console-print!)

(def ^:private meths
  {:get "GET"
   :put "PUT"
   :post "POST"
   :delete "DELETE"})

(defn edn-xhr [{:keys [method url data on-complete]}]
  (let [xhr (XhrIo.)]
    (events/listen xhr goog.net.EventType.COMPLETE
      (fn [e]
        (on-complete (reader/read-string (.getResponseText xhr)))))
    (. xhr
      (send url (meths method) (when data (pr-str data))
        #js {"Content-Type" "application/edn"}))))

(def app-state
  (atom {:words []
         :invalid {}}))

(defn submit-word [word app]
  (edn-xhr
    {:method :put
     :url "word"
     :data {:word word}
     :on-complete
     (fn [res]
       (if (= :ok res)
         (println "server response:" res)
         (om/transact! app :invalid #(merge % res))))}))

(defn tops-view [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (js/setInterval
        (fn []
          (edn-xhr
            {:method :get
             :url "word"
             :on-complete #(om/transact! app :words (fn [x] (conj (vec (take-last 9 x)) [% :server])))}))
        1000))
    om/IInitState
    (init-state [_]
      {:text ""})
    om/IRenderState
    (render-state [this state]
      (dom/div nil
        (dom/h1 nil "Om Tops")
        (dom/input
          #js {:value (:text state)
               :onChange #(om/set-state! owner :text (.. % -target -value))})
        (dom/button
          #js {:onClick #(do
                           (submit-word (om/get-state owner :text) app)
                           (om/transact! app :words (fn [x] (conj (vec (take-last 9 x)) [(om/get-state owner :text) :local])))
                           (om/set-state! owner :text ""))}
          "Submit")
        (apply dom/div nil
          (map
            (fn [[w o]]
              (dom/p #js {:className (str (if (= o :local) "local" "")
                                       (if (get (:invalid app) w)
                                         " invalid" ""))}
                w))
             (reverse (:words app))))))))

(om/root tops-view app-state
  {:target (js/document.getElementById "tops")})

(om/root
 ankha/inspector
 app-state
 {:target (js/document.getElementById "debug")})

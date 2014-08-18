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
  (atom {:words []}))

(defn submit-word [word app]
  (edn-xhr
    {:method :put
     :url "word"
     :data {:word word}
     :on-complete
     (fn [res]
       (when-not (= :ok res)
         (om/transact! app :words
           #(mapv (fn [x]
                    (if (= res (:word x))
                      (assoc x :invalid true)
                      x))
              %))))}))

(defn word-view [{:keys [word origin invalid]} owner]
  (om/component
    (dom/p #js {:className (str (if (= origin :local) "local" "")
                             (if invalid " invalid" ""))}
      word)))

(defn input-word [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:input ""})
    om/IRenderState
    (render-state [this state]
      (dom/div nil
        (dom/input
          #js {:value (:input state)
               :onChange #(om/set-state! owner :input (.. % -target -value))})
        (dom/button
          #js {:onClick #(let [w (om/get-state owner :input)]
                           (submit-word w app)
                           (om/transact! app :words
                             (fn [x]
                               (conj (vec (take-last 9 x))
                                 {:word w
                                  :origin :local})))
                           (om/set-state! owner :input ""))}
          "Submit")))))

(defn tops-view [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (js/setInterval
        (fn []
          (edn-xhr
            {:method :get
             :url "word"
             :on-complete #(om/transact! app :words
                             (fn [x]
                               (conj (vec (take-last 9 x))
                                 {:word %
                                  :origin :server})))}))
        1000))
    om/IRender
    (render [this]
      (dom/div nil
        (dom/h1 nil "Om Tops")
        (om/build input-word app)
        (apply dom/div nil
          (om/build-all word-view (reverse (:words app))))))))

(om/root tops-view app-state
  {:target (js/document.getElementById "tops")})

#_(om/root
 ankha/inspector
 app-state
 {:target (js/document.getElementById "debug")})

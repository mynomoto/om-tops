(ns om-tops.core
  (:require
    [clojure.string :as str]
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
       (om/transact! app :words
         #(mapv (fn [x]
                  (cond
                    (= (:invalid res) (:word x))
                    (assoc x :invalid true)

                    (= (:valid res) (:word x))
                    (assoc x :valid true)

                    :else x))
            %)))}))

(defn word-view [{:keys [word origin invalid valid]} owner]
  (om/component
    (dom/li
      #js {:className (str "list-group-item"
                        (when (and (= origin :local) (not invalid) (not valid))
                          " list-group-item-warning")
                        (when (and (= origin :local) valid)
                          " list-group-item-success")
                        (when invalid " invalid list-group-item-danger"))}
      word)))

(defn input-word [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:input ""})
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "input-group"}
        (dom/input
          #js {:className "form-control"
               :value (:input state)
               :onChange #(om/set-state! owner :input (.. % -target -value))})
        (dom/span #js {:className "input-group-btn"}
          (dom/button
            #js {:className "btn btn-primary"
                 :onClick #(let [w (om/get-state owner :input)]
                             (when-not (str/blank? w)
                               (submit-word w app)
                               (om/transact! app :words
                                 (fn [x]
                                   (conj (vec (take-last 9 x))
                                     {:word w
                                      :origin :local})))
                               (om/set-state! owner :input "")))}
            "Submit"))))))

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
      (dom/div #js {:className "row"}
        (dom/div #js {:className "col-lg-4 col-md-5 col-sm-6"}
          (dom/h1 nil "Om Tops")
          (om/build input-word app)
          (apply dom/ul #js {:className "list-group"}
            (om/build-all word-view (reverse (:words app)))))))))

(om/root tops-view app-state
  {:target (js/document.getElementById "tops")})

(om/root
 ankha/inspector
 app-state
 {:target (js/document.getElementById "debug")})

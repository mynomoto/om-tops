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

(def state (atom {:words []}))

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

(defn word-item [{:keys [word origin invalid valid]} owner]
  (om/component
    (dom/li
      #js {:className (str "list-group-item"
                        (when (and (= origin :local) (not invalid) (not valid))
                          " list-group-item-warning")
                        (when (and (= origin :local) valid)
                          " list-group-item-success")
                        (when invalid " invalid list-group-item-danger"))}
      word)))

(defn word-list [words owner]
  (om/component
    (apply dom/ul #js {:className "list-group"}
      (om/build-all word-item words))))

(defn word-input [app owner]
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

(defn tops-component [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [update-state
            (fn []
              (edn-xhr
                {:method :get
                 :url "word"
                 :on-complete #(when-not (str/blank? %)
                                 (om/transact! app :words
                                   (fn [x]
                                     (conj (vec (take-last 9 x))
                                       {:word %
                                        :origin :server}))))}))]
        (update-state)
        (js/setInterval update-state 1000)))
    om/IRender
    (render [this]
      (dom/div #js {:className "row"}
        (dom/div #js {:className "col-lg-4 col-md-5 col-sm-6"}
          (dom/h1 nil "Om Tops")
          (om/build word-input app)
          (om/build word-list (reverse (:words app))))))))

(when (js/document.getElementById "tops")
  (om/root tops-component state
    {:target (js/document.getElementById "tops")}))

#_(om/root
 ankha/inspector
 state
 {:target (js/document.getElementById "debug")})

(ns om-tops.core-test
  (:require-macros
    [cemerick.cljs.test
     :refer [is deftest with-test run-tests testing test-var]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [dommy.macros :refer [sel sel1 node]])
  (:require
    [cemerick.cljs.test :as t]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop :include-macros true]
    [clojure.string :as str]
    [dommy.utils :as utils]
    [dommy.core :as dommy]
    [om.core :as om :include-macros true]
    [om-tops.core :as o]))

(def runs 100)

(def isClient (not (nil? (try (.-document js/window)
                              (catch js/Object e nil)))))

(defn container-div []
  (let [id (str "container-" (gensym))]
    [(node [:div {:id id}]) (str "#" id)]))

(defn insert-container! [container]
  (dommy/append! (sel1 js/document :body) container))

(defn new-container! []
  (let [[n s] (container-div)]
    (insert-container! n)
    (sel1 s)))

(defspec server-word-item runs
  (prop/for-all [w (gen/hash-map :word gen/string-ascii)]
    (when isClient
      (let [div (new-container!)
            _ (om/root o/word-item (assoc w :origin :server) {:target div})
            el (.-firstChild div)]
        (is (= 1 (.-childElementCount div)))
        (is (= (:word w) (dommy/text el)))
        (is (dommy/has-class? el "list-group-item"))
        (is (not (dommy/has-class? el "list-group-item-warning")))
        (is (not (dommy/has-class? el "list-group-item-success")))
        (is (not (dommy/has-class? el "list-group-item-danger")))
        (is (not (dommy/has-class? el "invalid"))))
      true)))

(defspec local-word-item runs
  (prop/for-all [w (gen/hash-map :word gen/string-ascii)]
    (when isClient
      (let [div (new-container!)
            _ (om/root o/word-item (assoc w :origin :local) {:target div})
            el (.-firstChild div)]
        (is (= 1 (.-childElementCount div)))
        (is (= (:word w) (dommy/text el)))
        (is (dommy/has-class? el "list-group-item"))
        (is (dommy/has-class? el "list-group-item-warning"))
        (is (not (dommy/has-class? el "list-group-item-success")))
        (is (not (dommy/has-class? el "list-group-item-danger")))
        (is (not (dommy/has-class? el "invalid"))))
      true)))

(defspec local-word-item-valid runs
  (prop/for-all [w (gen/hash-map :word gen/string-ascii)]
    (when isClient
      (let [div (new-container!)
            _ (om/root o/word-item (assoc w
                                     :origin :local
                                     :valid true) {:target div})
            el (.-firstChild div)]
        (is (= 1 (.-childElementCount div)))
        (is (= (:word w) (dommy/text el)))
        (is (dommy/has-class? el "list-group-item"))
        (is (not (dommy/has-class? el "list-group-item-warning")))
        (is (dommy/has-class? el "list-group-item-success"))
        (is (not (dommy/has-class? el "list-group-item-danger")))
        (is (not (dommy/has-class? el "invalid"))))
      true)))

(defspec local-word-item-invalid runs
  (prop/for-all [w (gen/hash-map :word gen/string-ascii)]
    (when isClient
      (let [div (new-container!)
            _ (om/root o/word-item (assoc w
                                     :origin :local
                                     :invalid true) {:target div})
            el (.-firstChild div)]
        (is (= 1 (.-childElementCount div)))
        (is (= (:word w) (dommy/text el)))
        (is (dommy/has-class? el "list-group-item"))
        (is (not (dommy/has-class? el "list-group-item-warning")))
        (is (not (dommy/has-class? el "list-group-item-success")))
        (is (dommy/has-class? el "list-group-item-danger"))
        (is (dommy/has-class? el "invalid")))
      true)))

(deftest tops-component-test
  (when isClient
    (let [ws {:words [{:word "blabla" :origin :server}
                      {:word "bleble" :origin :server}]}]
      (let [div (new-container!)
            _ (om/root o/tops-component ws {:target div})
            elp (.-firstChild div)
            elc (.-firstChild elp)
            h (aget (.-childNodes elc) 0)
            wl (aget (.-childNodes elc) 2)]
        (is (= 1 (.-childElementCount div)))
        (is (= 1 (.-childElementCount elp)))
        (is (= 3 (.-childElementCount elc)))
        (is (= 3 (-> elc .-childNodes .-length)))
        (is (dommy/has-class? elp "row"))
        (is (dommy/has-class? elc "col-lg-4"))
        (is (dommy/has-class? elc "col-md-5"))
        (is (dommy/has-class? elc "col-sm-6"))
        (is (= "Om Tops" (dommy/text h)))
        (is (= 2 (.-childElementCount wl)))
        (is (dommy/has-class? wl "list-group"))
        (is (= (str/join (map :word (reverse (:words ws)))) (dommy/text wl)))
        ))))

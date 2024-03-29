(ns om-tops.core-test
  (:require-macros
    [cemerick.cljs.test
     :refer [is deftest with-test run-tests testing test-var]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [dommy.macros :refer [sel sel1 node]])
  (:require
    [cemerick.cljs.test]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop :include-macros true]
    [clojure.string :as str]
    [dommy.utils :as utils]
    [dommy.core :as dommy]
    [om.core :as om :include-macros true]
    [om-tops.core :as t]))

(def runs 100)

(defn new-container! []
  (let [id (str "container-" (gensym))
        div (node [:div {:id id}])]
    (dommy/append! (.-body js/document) div)
    div))

(defspec server-word-item runs
  (prop/for-all [w (gen/hash-map :word gen/string-ascii)]
    (let [div (new-container!)
          _ (om/root t/word-item (assoc w :origin :server) {:target div})
          el (.-firstChild div)]
      (and (= 1 (.-childElementCount div))
        (= (:word w) (dommy/text el))
        (dommy/has-class? el "list-group-item")
        (not (dommy/has-class? el "list-group-item-warning"))
        (not (dommy/has-class? el "list-group-item-success"))
        (not (dommy/has-class? el "list-group-item-danger"))
        (not (dommy/has-class? el "invalid"))))))

(defspec local-word-item runs
  (prop/for-all [w (gen/hash-map :word gen/string-ascii)]
    (let [div (new-container!)
          _ (om/root t/word-item (assoc w :origin :local) {:target div})
          el (.-firstChild div)]
      (and
        (= 1 (.-childElementCount div))
        (= (:word w) (dommy/text el))
        (dommy/has-class? el "list-group-item")
        (dommy/has-class? el "list-group-item-warning")
        (not (dommy/has-class? el "list-group-item-success"))
        (not (dommy/has-class? el "list-group-item-danger"))
        (not (dommy/has-class? el "invalid"))))))

(defspec local-word-item-valid runs
  (prop/for-all [w (gen/hash-map :word gen/string-ascii)]
    (let [div (new-container!)
          _ (om/root t/word-item (assoc w
                                   :origin :local
                                   :valid true) {:target div})
          el (.-firstChild div)]
      (and
        (= 1 (.-childElementCount div))
        (= (:word w) (dommy/text el))
        (dommy/has-class? el "list-group-item")
        (not (dommy/has-class? el "list-group-item-warning"))
        (dommy/has-class? el "list-group-item-success")
        (not (dommy/has-class? el "list-group-item-danger"))
        (not (dommy/has-class? el "invalid"))))))

(defspec local-word-item-invalid runs
  (prop/for-all [w (gen/hash-map :word gen/string-ascii)]
    (let [div (new-container!)
          _ (om/root t/word-item (assoc w
                                   :origin :local
                                   :invalid true) {:target div})
          el (.-firstChild div)]
      (and
        (= 1 (.-childElementCount div))
        (= (:word w) (dommy/text el))
        (dommy/has-class? el "list-group-item")
        (not (dommy/has-class? el "list-group-item-warning"))
        (not (dommy/has-class? el "list-group-item-success"))
        (dommy/has-class? el "list-group-item-danger")
        (dommy/has-class? el "invalid")))))

(defspec word-list-test runs
  (prop/for-all [w (gen/vector (gen/hash-map :word gen/string-ascii) 0 10)]
    (let [div (new-container!)
          _ (om/root t/word-list (map #(assoc % :origin :server) w)
              {:target div})
          el (.-firstChild div)]
      (and
        (= 1 (.-childElementCount div))
        (= (count w) (.-childElementCount el))
        (dommy/has-class? el "list-group")
        (= (str/join (map :word (reverse w))) (dommy/text el))))))

(deftest word-input-test
  (let [ws {:words [{:word "blabla" :origin :server}
                    {:word "bleble" :origin :server}]}
        div (new-container!)
        _ (om/root t/word-input ws {:target div})
        el (.-firstChild div)
        in (.-firstChild el)
        sp (aget (.-childNodes el) 1)
        bt (.-firstChild sp)]
    (is (dommy/has-class? el "input-group"))
    (is (= 2 (.-childElementCount el)))
    (is (dommy/has-class? in "form-control"))
    (is (= "" (.-value in)))
    (is (= 1 (.-childElementCount sp)))
    (is (dommy/has-class? sp "input-group-btn"))
    (is (dommy/has-class? bt "btn"))
    (is (dommy/has-class? bt "btn-primary"))
    (is (= (dommy/text bt) "Submit"))))

(defspec tops-component-test runs
  (prop/for-all [w (gen/vector (gen/hash-map :word gen/string-ascii) 0 10)]
    (let [ws {:words w}
          div (new-container!)
          _ (om/root t/tops-component ws {:target div})
          elp (.-firstChild div)
          elc (.-firstChild elp)
          h (aget (.-childNodes elc) 0)
          wl (aget (.-childNodes elc) 2)]
      (and
        (= 1 (.-childElementCount div))
        (= 1 (.-childElementCount elp))
        (= 3 (.-childElementCount elc))
        (dommy/has-class? elp "row")
        (dommy/has-class? elc "col-lg-4")
        (dommy/has-class? elc "col-md-5")
        (dommy/has-class? elc "col-sm-6")
        (= "Om Tops" (dommy/text h))
        (= (count w) (.-childElementCount wl))
        (dommy/has-class? wl "list-group")
        (= (str/join (map :word (reverse (:words ws)))) (dommy/text wl))))))

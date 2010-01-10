;;; A Resource interning library.
(ns grayswx.context.resource
  (:refer-clojure :exclude (intern alias load find get))
  (:use [clojure.test]
        [grayswx.util]
        [grayswx.util.file]
        [grayswx.util.cast]
        [grayswx.util.string]
        [clojure.contrib.def])
  (:import (java.awt Image)
           (java.io File IOException)
           (javax.imageio ImageIO)))

(with-test
  (defn put
    "Puts a resource into the context."
    [ctx type name rsrc]
    (doto-reduce ctx
      (modify-key :rmap {}
        (modify-key type {}
          (assoc name rsrc)))
      (assoc :return rsrc)))
  (is (= {:rmap {:a {:b :c}}
          :return :c}
         (put {} :a :b :c))))

(with-test
  (defn get
    "Gets a resource from the context."
    ([ctx type name]
       (if-key ctx :rmap
         (if-key type
           (clojure.core/get name))))
    ([ctx name]
       (some #(get ctx % name)
             (keys (ctx :rmap)))))
  (let [ctx {:rmap {:a {1 2 3 4}
                    :b {5 6 7 8}}}]
    (is (not (get ctx :a 2)))
    (is (= 4 (get ctx :a 3)))
    (is (not (get ctx 4)))
    (is (= 6 (get ctx 5)))))

(with-test
  (defn put-path
    "Adds a directory to the context's path."
    {:ctx true}
    [ctx dir]
    (modify-key ctx :path #{"."}
      (conj dir)))
  (is (= {:path #{"." "hello"}}
         (put-path {} "hello"))))

(with-test
  (defn get-path
    "Returns the path of a context."
    [ctx]
    (clojure.core/get ctx :path #{"."}))
  (is (= #{"/"} (get-path {:path #{"/"}})))
  (is (= #{"." "/"} (get-path (put-path {} "/")))))

(with-test
  (defn register
    "Registers a resource type.
Takes the context, the name of the type,
and the function used to load the type."
    [ctx type load]
    (modify-key ctx :rtype {}
      (assoc type load)))
  (is (= {:rtype {:a :b}}
         (register {} :a :b))))

(defn load
  "Loads a resource of the given type.
Interns the resource."
  [ctx type name & args]
  (if-let [load-fn ((ctx :rtype) type)]
    (apply load-fn ctx name args)))

(defn alias
  "Aliases an existing resource."
  [ctx type old new]
  (if-let [rsrc (get ctx type old)]
    (put ctx type new rsrc)))

(defn find
  "Finds a resource of the given type, loading it if necessary."
  [ctx type name & args]
  (if-let [rsrc (get ctx type name)]
    (assoc ctx :return rsrc)
    (apply load ctx type name args)))
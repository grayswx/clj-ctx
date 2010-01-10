;; A set of utility functions for context functions.
(ns grayswx.context.util
  (:use [clojure.test]))

(with-test
  (defn map-context
    "Like map, except it returns a context w/ a :return key, and
side effects on the context carry over from one iteration to the next."
    [ctx fn & args]
    (loop [cur-ctx ctx
           arglist (map first args)
           rest-args (map rest args)
           cur-ret (vector)]
      (let [new-ctx (apply fn cur-ctx arglist)
            new-ret (conj cur-ret (:return new-ctx))]
        (if (> (apply min (map count rest-args)) 0)
          (recur new-ctx
                 (map first rest-args)
                 (map rest rest-args)
                 new-ret)
          (assoc new-ctx :return new-ret)))))
  )
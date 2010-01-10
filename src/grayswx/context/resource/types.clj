;;; A set of default interning functions.
(ns grayswx.context.resource.types
  (:use [grayswx.util]
        [grayswx.util.cast]
        [grayswx.util.file])
  (:require [grayswx.context.resource :as rsrc])
  (:import (java.io File IOException)
           (javax.imageio ImageIO)))

(defn find-file
  "Finds a file.  Can optionally filter extensions."
  [ctx name & exts]
  (let [exts (if (empty? exts) '("*") exts)
        exts (map str (repeat ".") exts)
        exts (conj exts "")
        glob-search (fn [string]
                      (some #(first (glob (path-str % string)))
                            (rsrc/get-path ctx)))]
    (some #(glob-search (str name %)) exts)))

(defn find-image-file
  "Finds an image file with the given name."
  [ctx name]
  (apply find-file ctx name (vec (ImageIO/getReaderFileSuffixes))))

(defn intern-image
  "Interns an image, read with ImageIO."
  [ctx name]
  (if-let [file (find-image-file ctx name)]
    (rsrc/put ctx :image name (ImageIO/read file))
    (throw (IOException. (str "intern-image:  Could not find file <"
                              name ">.")))))

(defn register-images
  "Registers the image type."
  [ctx]
  (rsrc/register ctx :image intern-image))
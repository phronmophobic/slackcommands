(ns slackcommands.util.img
  (:require [membrane.ui :as ui]
            [clojure.java.io :as io]
            [membrane.java2d :as java2d]
            )
  (:import java.awt.image.BufferedImage
           java.awt.Graphics2D
           java.awt.Color
           java.awt.RenderingHints
           javax.imageio.ImageIO))

(defn ^:private new-img
  "Returns a new BufferedImage in a format suitable for creating a gif"
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR))

(defn save-image-with-masks [img-path output-path masks]
  (let [uiimg (ui/image img-path)
        [w h] (ui/bounds uiimg)
        img (new-img w h)
        g (.createGraphics img)]
    
    (java2d/draw-to-graphics g uiimg)
    (.setBackground g (Color. 1 0 0 0))
    (doseq [{:strs [x y width height]} masks]
      (.clearRect g x y width height))
    (with-open [os (clojure.java.io/output-stream output-path)]
      (ImageIO/write ^BufferedImage img "png" os))))




(comment

  (save-image-with-masks "treats.jpeg"
                         [{"x" 10
                           "y" 20
                           "width" 10
                           "height" 20}])
  ,)

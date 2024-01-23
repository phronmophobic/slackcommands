(ns slackcommands.ai.img
  (:require [membrane.ui :as ui]
            [clojure.java.io :as io]
            ;; [membrane.java2d :as java2d]
            [membrane.skia :as skia]
            [slackcommands.util :as util]))

;; 1. Resize
;; json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "Resize",
;;   "description": "A function to resize the image by modifying its width and height.",
;;   "type": "object",
;;   "properties": {
;;     "width": {
;;       "description": "The new width of the image.",
;;       "type": "integer"
;;     },
;;     "height": {
;;       "description": "The new height of the image.",
;;       "type": "integer"
;;     }
;;   }
;; }
(defonce schemas (atom {}))

(defmacro defschema [schema]
  `(let [schema# ~schema
         op# (get schema# "name")]
     (swap! schemas
            assoc op# schema#)
     schema#))

(defmulti img-op* (fn [m]
                    (get m "op")))


(defschema
  {"name" "resize",
   "description" "Resizes the image at url given the width and height. Omitting the width or height will perform aspect rescale.",
   "parameters"
   {"type" "object",
    "required" ["url"]
    "properties"
    {"width" {"type" "integer",
              "description" "The new width of the image"}
     "height" {"type" "integer",
               "description" "The new height of the image"}
     "url" {"type" "string",
            "pattern" "^http.*"
            "description" "A url to an image."}}}})
(defmethod img-op* "resize" [{:strs [url width height]}]
  (ui/image (io/as-url url)
            [width height]))



;; 2. Crop
;; json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "Crop",
;;   "description": "A function to crop the image to a specific region.",
;;   "type": "object",
;;   "properties": {
;;     "x": {
;;       "description": "The x-coordinate of the crop region's upper left corner.",
;;       "type": "integer"
;;     },
;;     "y": {
;;       "description": "The y-coordinate of the crop region's upper left corner.",
;;       "type": "integer"
;;     },
;;     "width": {
;;       "description": "The width of the crop region.",
;;       "type": "integer"
;;     },
;;     "height": {
;;       "description": "The height of the crop region.",
;;       "type": "integer"
;;     }
;;   }
;; }
(defschema
  {"name" "crop",
   "description" "Crops the image at url.",
   "parameters"
   {"type" "object",
    "required" ["url" "width" "height"]
    "properties"
    {"x" {"type" "integer",
          "default" 0
          "description" "The x-coordinate of the crop region's upper left corner."}
     "y" {"type" "integer",
          "default" 0
          "description" "The y-coordinate of the crop region's upper left corner."}

     "width" {"type" "integer",
              "description" "The new width of the crop region."}
     "height" {"type" "integer",
               "description" "The new height of the crop region."}
     "url" {"type" "string",
            "pattern" "^http.*"
            "description" "A url to an image."}}}})
(defmethod img-op* "crop" [{:strs [url x y width height]}]
  (ui/translate
   (- (or x 0)) (- (or y 0))
   (ui/scissor-view
    [0 0]
    [width height]
    (ui/image (io/as-url url)))))


;; 3. Rotate
;; json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "Rotate",
;;   "description": "A function to rotate the image by a specified number of degrees.",
;;   "type": "object",
;;   "properties": {
;;     "degrees": {
;;       "description": "The number of degrees to rotate the image by.",
;;       "type": "integer"
;;     }
;;   }
;; }
#_(defn rotate [{:strs [img degrees]}]
  (ui/rotate ))


;; 4. Flip
;; json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "Flip",
;;   "description": "A function to flip the image either horizontally or vertically.",
;;   "type": "object",
;;   "properties": {
;;     "horizontal": {
;;       "description": "If true, flip the image horizontally.",
;;       "type": "boolean"
;;     },
;;     "vertical": {
;;       "description": "If true, flip the image vertically.",
;;       "type": "boolean"
;;     }
;;   }
;; }


;; 5. Adjust Colors
;; json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "AdjustColors",
;;   "description": "A function to adjust the hue, saturation, and brightness of an image.",
;;   "type": "object",
;;   "properties": {
;;     "hue": {
;;       "description": "The amount to adjust the hue by.",
;;       "type": "integer"
;;     },
;;     "saturation": {
;;       "description": "The amount to adjust the saturation by.",
;;       "type": "integer"
;;     },
;;     "brightness": {
;;       "description": "The amount to adjust the brightness by.",
;;       "type": "integer"
;;     }
;;   }
;; }
;; 6. Overlay
;; ```json
;; {
;;  "$id": "https://example.com/imageEditor.json",
;;  "$schema": "http://json-schema.org/draft-07/schema#",
;;  "title": "Overlay",
;;  "description": "A function to overlay another image on top of the original.",
;;  "type": "object",
;;  "properties": {
;;    "overlayImage": {
;;      "description": "The URL of the image to be overlaid.",
;;      "type": "string",
;;      "format": "uri"
;;    },
;;    "opacity": {
;;      "description": "The opacity of the overlaid image.",
;;      "type": "integer"
;;    },
;;    "position": {
;;      "description": "The position where the image will be overlaid.",
;;      "type": "object",
;;      "properties": {
;;        "x" : {
;;          "description": "The x-coordinate of the position.",
;;          "type": "integer"
;;        },
;;        "y" : {
;;          "description": "The y-coordinate of the position.",
;;          "type": "integer"
;;        }
;;      }
;;    }
;;  }
;; }
(defschema
  {"name" "overlay",
   "description" "Overlays the image at overlay_url on top of the image at url.",
   "parameters"
   {"type" "object",
    "required" ["url" "overlay_url"]
    "properties"
    {
     "position" {"description" "The location where the image will be overlaid."
                 "type" "object"
                 "properties" {"x" {"type" "integer",
                                    "description" "The x-coordinate."}
                               "y" {"type" "integer",
                                    "description" "The y-coordinate."}}}
     "opacity" {"type" "number",
                "minimum" 0
                "maximum" 1
                "description" "The opacity of the overlaid image on a scale from [0,1]."}
     "url" {"type" "string",
            "pattern" "^http.*"
            "description" "A url for the image on bottom."}
     "overlay_url" {"type" "string",
                    "pattern" "^http.*"
                    "description" "The url to the image to be overlaid on top."}}}})

(defmethod img-op* "overlay" [{:strs [url overlay_url opacity position]}]
  [(ui/image (io/as-url url))
   (ui/translate
    (get position "x") (get position "y")
    (ui/image (io/as-url overlay_url) nil opacity))])


(defschema
  {"name" "pad",
   "description" "Adds extra transparent pixels on the outside of the image at url. Any omited pads will be treated as zero",
   "parameters"
   {"type" "object",
    "required" ["url"]
    "properties"
    {
     "url" {"type" "string",
            "pattern" "^http.*"
            "description" "A url for the image on bottom."}
     "top" {"type" "integer",
            "description" "Number of pixels to add to the top."}
     "right" {"type" "integer",
            "description" "Number of pixels to add to the right."}
     "left" {"type" "integer",
            "description" "Number of pixels to add to the left."}
     "bottom" {"type" "integer",
            "description" "Number of pixels to add to the bottom."}}}})

(defmethod img-op* "pad" [{:strs [url left top right bottom] :as m}]
  (ui/padding (get m "top" 0) (get m "right" 0) (get m "bottom" 0) (get m "left" 0) 
              (ui/image (io/as-url url))))

;; 7. Opacity
;; ```json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "Opacity",
;;   "description": "Adjust the transparency level of the image or a specific layer in the image.",
;;   "type": "object",
;;   "properties": {
;;     "opacity": {
;;       "description": "The transparency level of the image. A value of 100 is fully opaque, and 0 is fully transparent.",
;;       "type": "integer"
;;     }
;;   }
;; }


;; 8. Remove Background
;; json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "RemoveBackground",
;;   "description": "A function to remove the background from the image.",
;;   "type": "object",
;;   "properties": {
;;     "tolerance": {
;;       "description": "A measure of how different the colors can be and still be considered as background. A higher value means more colors will be removed.",
;;       "type": "integer"
;;     }
;;   }
;; }
;; 9. Adjust Contrast
;; json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "AdjustContrast",
;;   "description": "A function to adjust the contrast level of an image.",
;;   "type": "object",
;;   "properties": {
;;     "contrast": {
;;       "description": "The amount to adjust the contrast by. Can be negative to decrease contrast, or positive to increase contrast.",
;;       "type": "integer"
;;     }
;;   }
;; }
;; 10. Exposure Correction
;; json
;; {
;;   "$id": "https://example.com/imageEditor.json",
;;   "$schema": "http://json-schema.org/draft-07/schema#",
;;   "title": "ExposureCorrection",
;;   "description": "A function to correct the brightness of an image to fix overexposure or underexposure.",
;;   "type": "object",
;;   "properties": {
;;     "correctionFactor": {
;;       "description": "The amount to adjust the image's exposure by. Can be negative to make the image darker, or positive to make the image lighter.",
;;       "type": "integer"
;;     }
;;   }
;; }


(defn img-op [m]
  (let [view (img-op* m)
        outf (io/file util/aimage-dir
                      (str (random-uuid) ".png"))
        result (skia/save-image (.getCanonicalPath outf)
                                view)]
    (if (zero? result)
      "There was an error saving the result."
      (let []
        (util/upload-file outf)
        (str "https://" "aimages.smith.rocks/" (.getName outf))))))

(defn image-schemas []
  (into []
        (map (fn [schema]
               {"type" "function"
                "function" schema}))
        (vals @schemas)))



(defn tool-fns []
  (into {}
        (map (fn [op]
               [op (fn [_thread-id m]
                     (img-op (assoc m "op" op)))]))
        (keys @schemas)))

(comment

  (def my-view
    (img-op {"op" "overlay"
             "url" "https://cdn.discordapp.com/attachments/1124159763865939990/1199131952863776868/phronmophobic_stan_lee_holding_a_mug_2738285d-3015-4eaa-b6d7-789f694bf408.png?ex=65c16da8&is=65aef8a8&hm=7b65739ff2d6715be01e4d3090108b06b301b55f9203662b665214b270f1918a&"
             "overlay_url" "https://cdn.discordapp.com/attachments/1124159763865939990/1195460462062612590/phronmophobic_a_frog_on_a_white_background_36caa679-5dbe-4686-9894-5e766dffab96.png?ex=65b41250&is=65a19d50&hm=f72590190520ddbf72a056e4e3ce9ea9443183f07a5cf270ce35678a5f237c3a&"
             "opacity" 0.5
             "position" {"x" 100
                         "y" 50}}))

  (skia/save-image "op.png" my-view)

  ,)

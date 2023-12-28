(ns slackcommands.ai.vision
  (:require [clojure.java.io :as io]
            [clojure.datafy :as d]
            [membrane.ui :as ui]
            [clojure.core.protocols :as p]
            [slackcommands.util :as util])
  (:import com.google.cloud.vision.v1.ImageAnnotatorClient
           com.google.cloud.vision.v1.ImageAnnotatorSettings
           com.google.api.gax.core.FixedCredentialsProvider
           com.google.auth.oauth2.ServiceAccountCredentials
           com.google.cloud.vision.v1.AnnotateImageRequest
           com.google.cloud.vision.v1.BatchAnnotateImagesResponse
           com.google.cloud.vision.v1.AnnotateImageResponse
           com.google.cloud.vision.v1.Feature
           com.google.cloud.vision.v1.Feature$Type
           com.google.cloud.vision.v1.Image
           com.google.protobuf.ByteString))


;; try (ImageAnnotatorClient imageAnnotatorClient = ImageAnnotatorClient.create()) {
;;   List<AnnotateImageRequest> requests = new ArrayList<>();
;;   BatchAnnotateImagesResponse response = imageAnnotatorClient.batchAnnotateImages(requests);
;; }

;; ImageAnnotatorSettings imageAnnotatorSettings =
;;     ImageAnnotatorSettings.newBuilder()
;;         .setCredentialsProvider(FixedCredentialsProvider.create(myCredentials))
;;         .build();
;; ImageAnnotatorClient imageAnnotatorClient = ImageAnnotatorClient.create(imageAnnotatorSettings);


(defonce credentials
  (delay
    (with-open [is (io/input-stream "google.json")]
      (ServiceAccountCredentials/fromStream is))))
(defonce settings
  (delay
    (-> (doto (ImageAnnotatorSettings/newBuilder)
          (.setCredentialsProvider (FixedCredentialsProvider/create @credentials)))
        (.build))))
(defonce client
  (delay
    (ImageAnnotatorClient/create @settings)))

;; ByteString imgBytes = ByteString.readFrom(new FileInputStream(filePath));

;; Image img = Image.newBuilder().setContent(imgBytes).build();

#_(def img
    (.build
     (doto (Image/newBuilder)
       (.setContent
        (with-open [is (io/input-stream "/Users/adrian/Downloads/BC_WordCount.jpg")]
          (ByteString/readFrom is))))))


;; AnnotateImageRequest request =
;;     AnnotateImageRequest.newBuilder()
;;         .addFeatures(Feature.newBuilder().setType(Type.OBJECT_LOCALIZATION))
;;         .setImage(img)
;;         .build();
;; requests.add(request);
#_(def my-request
    (.build
     (doto (AnnotateImageRequest/newBuilder)
       #_(.addFeatures (doto (Feature/newBuilder)
                         (.setType Feature$Type/OBJECT_LOCALIZATION)))
       (.addAllFeatures
        [(.build
          (doto (Feature/newBuilder)
            (.setType Feature$Type/OBJECT_LOCALIZATION)))
         (.build
          (doto (Feature/newBuilder)
            (.setType Feature$Type/DOCUMENT_TEXT_DETECTION)))
         (.build
          (doto (Feature/newBuilder)
            (.setType Feature$Type/TEXT_DETECTION)))
         (.build
          (doto (Feature/newBuilder)
            (.setType Feature$Type/LABEL_DETECTION)))])
       (.setImage img))))

;; (def response (.batchAnnotateImages client [my-request] ) )
;; (def responses (.getResponsesList response))
;; (def res (first responses))


(defn datafy-props
  "Returns a transducer that will datafy props.

  Expects a stream of unpacked k,v pairs."
  []
  (comp
   (partition-all 2)
   (keep (fn [[k v]]
           (let [v (if (seqable? v)
                     (when-let [v (seq v)]
                       (into []
                             (map d/datafy)
                             v))
                     (when (not= "" (str v))
                       (d/datafy v)))]
             (when v
               [k v]))))))

(extend-protocol p/Datafiable
  com.google.cloud.vision.v1.BatchAnnotateImagesResponse
  (datafy [res]
    (into []
          (map d/datafy)
          (.getResponsesList res)))

  com.google.cloud.vision.v1.AnnotateImageResponse
  (datafy [res]
    (into
     {}
     (datafy-props)
     [:context (.getContext res)
      :crop-hints (.getCropHintsAnnotation res)
      :error (.getError res)
      :face-annotations (.getFaceAnnotationsList res)
      :full-text-annotations (.getFullTextAnnotation res)
      :image-properties-annotation (.getImagePropertiesAnnotation res)
      :label-annotations (.getLabelAnnotationsList res)
      :landmark-annotations (.getLandmarkAnnotationsList res)
      :localized-object-annotations (.getLocalizedObjectAnnotationsList res)
      :logo-annotations (.getLogoAnnotationsList res)
      :productSearchResults (.getProductSearchResults res)
      :safe-search-annotation (.getSafeSearchAnnotation res)
      :text-annotations (.getTextAnnotationsList res)
      :web-detection (.getWebDetection res)]))

  com.google.cloud.vision.v1.LocalizedObjectAnnotation
  (datafy [ann]
    {:bounding-poly (d/datafy (.getBoundingPoly ann))
     :mid (.getMid ann)
     :score (.getScore ann)
     :name (.getName ann)
     :language-code (.getLanguageCode ann)})

  com.google.cloud.vision.v1.BoundingPoly
  (datafy [poly]
    (into []
          (map (fn [v]
                 {:x (.getX v)
                  :y (.getY v)}))
          #_(.getNormalizedVertices poly)
          (.getNormalizedVerticesList poly)))

  com.google.cloud.vision.v1.EntityAnnotation
  (datafy [ann]
    {:description (.getDescription ann)
     :score (.getScore ann)
     :mid (.getMid ann)
     :topicality (.getTopicality ann)})

  com.google.cloud.vision.v1.TextAnnotation
  (datafy [ann]
    {:pages (into [] (map d/datafy (.getPagesList ann)))
     :text (.getText ann)})

  com.google.cloud.vision.v1.Page
  (datafy [page]
    {:blocks (into []
                   (map d/datafy)
                   (.getBlocksList page))
     :confidence (.getConfidence page)
     :width (.getWidth page)
     :height (.getHeight page)
     :property (d/datafy (.getProperty page))})

  com.google.cloud.vision.v1.TextAnnotation$TextProperty
  (datafy [p]
    (into
     []
     (map d/datafy)
     (.getDetectedLanguagesList p)))

  com.google.cloud.vision.v1.TextAnnotation$DetectedLanguage
  (datafy [p]
    {:language-code (.getLanguageCode p)
     :confidence (.getConfidence p)})

  com.google.cloud.vision.v1.Block
  (datafy [block]
    (into {}
          (datafy-props)
          [:bounding-box (.getBoundingBox block)
           :block-type (.getBlockType block)
           :paragraphs (.getParagraphsList block)])
    )

  com.google.cloud.vision.v1.Block$BlockType
  (datafy [bt]
    (str bt))

  com.google.cloud.vision.v1.Paragraph
  (datafy [p]
    (into {}
          (datafy-props)
          [:bounding-box (.getBoundingBox p)
           :confidence (.getConfidence p)
           :property (.getProperty p)
           :words (.getWordsList p)]))

  com.google.cloud.vision.v1.Word
  (datafy [p]
    (into {}
          (datafy-props)
          [:bounding-box (.getBoundingBox p)
           :confidence (.getConfidence p)
           :property (.getProperty p)
           :symbols (.getSymbolsList p)]))

  com.google.cloud.vision.v1.Symbol
  (datafy [s]
    (into {}
          (datafy-props)
          [:bounding-box (.getBoundingBox s)
           :confidence (.getConfidence s)
           :property (.getProperty s)
           :symbol (.getText s)])
    )
  ,)


(defn url->image [url]
  (if-let [f (util/url->local url)]
    (with-open [is (io/input-stream f)]
      (.build
       (doto (Image/newBuilder)
         (.setContent
          (ByteString/readFrom is)))))
    (with-open [is (io/input-stream
                    (io/as-url url))]
      (.build
       (doto (Image/newBuilder)
         (.setContent
          (ByteString/readFrom is)))))))

(defn examine-image [url]
  (let [img (url->image url)
        request (.build
                 (doto (AnnotateImageRequest/newBuilder)
                   #_(.addFeatures (doto (Feature/newBuilder)
                                     (.setType Feature$Type/OBJECT_LOCALIZATION)))
                   (.addAllFeatures
                    [(.build
                      (doto (Feature/newBuilder)
                        (.setType Feature$Type/OBJECT_LOCALIZATION)))
                     (.build
                      (doto (Feature/newBuilder)
                        (.setType Feature$Type/DOCUMENT_TEXT_DETECTION)))
                     (.build
                      (doto (Feature/newBuilder)
                        (.setType Feature$Type/TEXT_DETECTION)))
                     (.build
                      (doto (Feature/newBuilder)
                        (.setType Feature$Type/LABEL_DETECTION)))])
                   (.setImage img)))
        response (.batchAnnotateImages @client [request])]
    (first
     (d/datafy response))))


(defn extract-text [url]
  (let [img (url->image url)
        request (.build
                 (doto (AnnotateImageRequest/newBuilder)
                   #_(.addFeatures (doto (Feature/newBuilder)
                                     (.setType Feature$Type/OBJECT_LOCALIZATION)))
                   (.addAllFeatures
                    [#_(.build
                        (doto (Feature/newBuilder)
                          (.setType Feature$Type/OBJECT_LOCALIZATION)))
                     (.build
                      (doto (Feature/newBuilder)
                        (.setType Feature$Type/DOCUMENT_TEXT_DETECTION)))
                     #_(.build
                        (doto (Feature/newBuilder)
                          (.setType Feature$Type/TEXT_DETECTION)))
                     #_(.build
                        (doto (Feature/newBuilder)
                          (.setType Feature$Type/LABEL_DETECTION)))])
                   (.setImage img)))
        response (.batchAnnotateImages @client [request])]
    (-> response
        d/datafy
        first
        :full-text-annotations
        :text)))


(defn label-image [url]
  (let [img (url->image url)
        request (.build
                 (doto (AnnotateImageRequest/newBuilder)
                   (.addAllFeatures
                    [(.build
                      (doto (Feature/newBuilder)
                        (.setType Feature$Type/LABEL_DETECTION)))])
                   (.setImage img)))
        response (.batchAnnotateImages @client [request])]
    (-> response
        d/datafy
        first
        :label-annotations)))

(defn find-objects [url]
  (let [img (url->image url)
        request (.build
                 (doto (AnnotateImageRequest/newBuilder)
                   (.addAllFeatures
                    [(.build
                      (doto (Feature/newBuilder)
                        (.setType Feature$Type/OBJECT_LOCALIZATION)))])
                   (.setImage img)))
        response (.batchAnnotateImages @client [request])]
    (-> response
        d/datafy
        first
        :localized-object-annotations)))


(def find-objects-memo (memoize find-objects))
(defn highlight-objects
  ([url]
   (let [objs (find-objects-memo url)]
     (highlight-objects url objs)))
  ([url objs]
   (let [
         img (ui/image (io/as-url url))
         [w h] (ui/bounds img)

         size 400
         ;; target width and target height
         [tw th] (if (> w h)
                   [size (* h (/ size w))]
                   [(* w (/ size h)) size])

         img (ui/image (io/as-url url)
                       [tw th])

         anns
         (into []
               (comp
                (map (fn [{:keys [bounding-poly
                                  name
                                  score]
                           :as m}]
                       (let [minx (apply min (map :x bounding-poly))
                             miny (apply min (map :y bounding-poly))
                             maxx (apply max (map :x bounding-poly))
                             maxy (apply max (map :y bounding-poly))]
                         (ui/translate
                          (* tw minx) (* th miny) 
                          [(ui/filled-rectangle
                            [1 0 0 0.3]
                            (* tw (- maxx minx))
                            (* th (- maxy miny)))
                           (ui/label (str name ": " (format "%.2f"score)))])
                         ))))
               objs)
         ]
     (apply
      ui/vertical-layout
      [img
       anns]
      (for [{:keys [mid name score]} objs]
        (ui/label (str name ": " score)))
      
      )
     )))

(defn obj->mask [[w h] {:keys [bounding-poly]}]
  (let [minx (apply min (map :x bounding-poly))
        miny (apply min (map :y bounding-poly))
        maxx (apply max (map :x bounding-poly))
        maxy (apply max (map :y bounding-poly))]
    (ui/translate
     (* w minx) (* h miny) 
     (ui/filled-rectangle
      [1 1 1 ]
      (* w (- maxx minx))
      (* h (- maxy miny)))
     )
    )
  
  )


(defn obj->mask2 [[w h] {:keys [bounding-poly]}]
  (let [minx (apply min (map :x bounding-poly))
        miny (apply min (map :y bounding-poly))
        maxx (apply max (map :x bounding-poly))
        maxy (apply max (map :y bounding-poly))]
    [(ui/translate
      0 0
      (ui/filled-rectangle
       [1 1 1]
       (* w minx)
       h))
     (ui/translate
      0 0
      (ui/filled-rectangle
       [1 1 1]
       w
       (* h miny)))
     (ui/translate
      (* w maxx) 0
      (ui/filled-rectangle
       [1 1 1]
       (- w (* w maxx))
       h))
     (ui/translate
      0 (* h maxy)
      (ui/filled-rectangle
       [1 1 1]
       w
       (- h (* h maxy))
       )
      )]
    )
  
  )

(comment
  (clojure.pprint/pprint
   (extract-text "https://pbs.twimg.com/media/GCOTFCWXsAAvZx0?format=jpg&name=medium"))

  (clojure.pprint/pprint
   (label-image "https://pbs.twimg.com/media/GCOTFCWXsAAvZx0?format=jpg&name=medium"))

  (clojure.pprint/pprint
   (find-objects "https://pbs.twimg.com/media/GCOrOcBXgAA-PC9?format=jpg&name=medium"))


  (require '[membrane.ui :as ui]
           '[membrane.skia :as skia])




  (skia/run
    (fn [& args]
      (highlight-objects "https://external-preview.redd.it/-7LYWWoN4P0WD31P0SOIUiEKpZDOQi8hsFWFCxSfjZ8.jpg?width=640&crop=smart&auto=webp&s=5bfc1b77a5678e3c260d5ffa519f96ba3b63f436")
      ))


  (def url "https://preview.redd.it/klo7lzrrjv8c1.jpeg?width=640&crop=smart&auto=webp&s=507b1c40590b40794ee3be3b1aa6365a65ab6bd5")

  (def original-img (ui/image (io/as-url url)))

  (def in-fname "in5.png")

  (let [[ow oh] (ui/bounds original-img)
        w 512
        #_(- ow (mod ow 64))
        h 512
        #_(- oh (mod oh 64))
        ]
    (prn w h)
    (skia/save-image in-fname
                     (ui/scissor-view
                      [0 0]
                      [w h]
                      original-img)
                     [w h]))
  (def img (ui/image in-fname))
  

  (def objs
    (find-objects (io/as-url (io/file in-fname))))

  (def cat (first objs))
  (skia/run
    (fn [& args]
      [img
       (obj->mask (ui/bounds img) cat)]))
  (skia/save-image "out.png"
                   [#_(ui/filled-rectangle
                     [0 0 0]
                     (ui/width img)
                     (ui/height img))
                    (obj->mask2 (ui/bounds img) cat)]
                    (ui/bounds img)
                    nil
                    100
                    false)

  (require '[slackcommands.stability :as stability])

  (def result
    (stability/edit-image
     (io/file in-fname)
     (io/file "out.png")
     "A monkey"))


  (require '[wkok.openai-clojure.api :as api])

  (def response
    (api/create-image-edit
     { ;; :model "dall-e-2"
      :prompt "flowers"
      :n 4
      :image (io/file in-fname)
      :mask (io/file "out.png")}
     {:api-key slackcommands.ai/api-key}))

  (require '[clojure.java.shell :as sh])
  (doseq [{:keys [url]} (:data response)]
    (sh/sh "open" url))

  
  
  ,)

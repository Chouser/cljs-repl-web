(ns cljs-repl-web.core
  (:require [reagent.core :as reagent]
            [re-frame.core :refer [dispatch dispatch-sync]]
            [re-com.core :refer [p h-box v-box box gap line]]
            [devtools.core :as devtools]
            [cljs-repl-web.handlers]
            [cljs-repl-web.subs]
            [cljs-repl-web.views :as views]
            [cljs-repl-web.views.utils :as utils]
            [cljs-repl-web.replumb-proxy :as replumb-proxy]
            [cljs-repl-web.config :as config]
            [cljs-repl-web.localstorage :as ls]
            [cljs-repl-web.app :as app]
            [re-console.common :as common]
            [re-complete.core :as re-complete]
            [goog.dom :as dom]))

(defonce console-key :cljs-console)

;; https://github.com/binaryage/cljs-devtools/releases/tag/v0.5.3
(when-not (:production? config/defaults)
  (devtools/set-pref! :install-sanity-hints true)
  (devtools/install!))

(enable-console-print!)

(def PI js/Math.PI)

(defn color3 [r g b] (js/BABYLON.Color3. r g b))
(defn vec3
  ([] (js/BABYLON.Vector3.Zero))
  ([x y z] (js/BABYLON.Vector3. x y z)))

(defn mesh-loaded [scene part meshes particle-systems]
  (let [mesh (nth meshes 0)
        material (js/BABYLON.StandardMaterial. "m1" scene)]
    (set! (.-diffuseColor material) (color3 0.8 0.3 0.8))
    (set! (.-material mesh) material)
    (condp = part
      "foot 1" (doseq [x (range 3)
                       z (range 2)]
                 (when-not (and (zero? x) (zero? z))
                   (let [foot (.createInstance mesh (str "foot" x z))]
                     (set! (.-position foot) (vec3 (* x -20) 0 (* z -20)))))))))

(defn ^:export main []
  (let [canvas (.getElementById js/document "renderCanvas")
        engine (js/BABYLON.Engine. canvas true)
        scene (js/BABYLON.Scene. engine)]
    (set! (.-enableOfflineSupport engine) false)
    (doseq [part ["head" "back" "tail" "foot 1"]]
      (js/BABYLON.SceneLoader.ImportMesh
         "" "stl/" (str "magbear - " part ".stl") scene
         #(mesh-loaded scene part %1 %2)))

    (set! (.-clearColor scene) (color3 0.2 0.2 0.267))
    (doto (js/BABYLON.ArcRotateCamera. "Camera" (/ PI 2) 1.0 110 
	                               (vec3)
                                       scene)
      (.attachControl canvas true)
      (.setTarget (vec3)))
    (let [light (js/BABYLON.HemisphericLight. "light1" (vec3 0 1 0) scene)]
      (set! (.-intesity light) 0.5))

    (.runRenderLoop engine #(.render scene))
    (.addEventListener js/window "resize" #(.resize engine)))
  (js/console.log "magbear engine")

  (let [{:keys [name verbose-repl? src-paths]} config/defaults
        local-storage-values (ls/get-local-storage-values)]
    (dispatch [:options console-key {:trim-chars "[](){}#'@^`~."
                                     :keys-handling {:visible-items 6
                                                     :item-height 20}}])
    (println "[Entering]" name)
    (dispatch-sync [:initialize config/defaults local-storage-values])
    (reagent/render [views/repl-component console-key {:eval-opts (replumb-proxy/eval-opts verbose-repl? src-paths)
                                                       :mode (:mode local-storage-values)
                                                       :mode-line? true
                                                       :on-after-change #(do (dispatch [:input console-key (common/source-without-prompt (.getValue %))])
                                                                             (dispatch [:focus console-key true])
                                                                             (app/create-dictionary (common/source-without-prompt (.getValue %)) console-key)
                                                                             (utils/align-suggestions-list %2))}]
                    (.getElementById js/document "app-center"))
    #_(reagent/render [views/bottom-panel] (.getElementById js/document "app-bottom"))
    #_(reagent/render [views/footer-component] (.getElementById js/document "app-footer"))))

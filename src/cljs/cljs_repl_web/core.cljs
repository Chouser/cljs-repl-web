(ns cljs-repl-web.core
  (:require-macros [cljs.core.async.macros :refer [go]])
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
            [cljs.core.async :refer [put! chan <! >! timeout close!]]
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

(defn add-physics [mesh scene]
  (set! (.-physicsImposter mesh)
        (js/BABYLON.PhysicsImpostor.
         mesh
         js/BABYLON.PhysicsImpostor.BoxImpostor
         (js-obj "mass" 1, "restitution" 0.1)
         scene)))

(def world (atom {}))

(defn get-mesh [n]
  (go
    (<! (get-in @world [n :loaded-chan]))
    (get-in @world [n :mesh])))

(defn mesh-loaded [scene part meshes particle-systems]
  (let [mesh (nth meshes 0)
        material (js/BABYLON.StandardMaterial. "m1" scene)
        imposter (add-physics mesh scene)]
    (set! (.-position mesh) (vec3 0 40 0))
    (set! (.-diffuseColor material) (color3 0.2 0.4 0.7))
    (set! (.-material mesh) material)
    (swap! world assoc-in [part :mesh] mesh)
    (close! (get-in @world [part :loaded-chan]))))

(defn ^:export main []
  (go
    (when (.-stopMagbearEngine js/window)
      ((.-stopMagbearEngine js/window)))
    (let [canvas (.getElementById js/document "renderCanvas")
          engine (js/BABYLON.Engine. canvas true)
          scene (js/BABYLON.Scene. engine)]
      (set! (.-enableOfflineSupport engine) false)
      (.enablePhysics scene (vec3 0 -9.81 0) (js/BABYLON.CannonJSPlugin.))

      (let [ground (js/BABYLON.MeshBuilder.CreateTiledGround
                    "ground" (js-obj "xmin" -500, "xmax" 500,
                                     "zmin" -500, "zmax" 500
                                     "subdivisions" (js-obj "w" 20 "h" 20))
                    scene)
            ground-material (js/BABYLON.StandardMaterial. "ground-material" scene)]
        (set! (.-diffuseTexture ground-material)
              (js/BABYLON.Texture. "grass.jpg" scene))
        (set! (.-material ground) ground-material)

        (js/BABYLON.PhysicsImpostor.
         ground
         js/BABYLON.PhysicsImpostor.BoxImpostor
         (js-obj "mass" 0, "restitution" 0.9)
         scene))

      (doseq [part ["head" "back" "tail" "foot 1"]]
        (let [loaded-chan (chan)]
          (swap! world assoc part {:loaded-chan loaded-chan})
          (js/BABYLON.SceneLoader.ImportMesh
           "" "stl/" (str "magbear - " part ".stl") scene
           #(mesh-loaded scene part %1 %2))))

      (set! (.-clearColor scene) (color3 0.2 0.2 0.267))
      (doto (js/BABYLON.ArcRotateCamera.
             "Camera" (/ PI 2) 1.0 170 (vec3) scene)
        (.attachControl canvas true)
        (.setTarget (vec3)))
      (let [light (js/BABYLON.HemisphericLight. "light1" (vec3 0 1 0) scene)]
        (set! (.-intesity light) 0.5))

      (let [head-phy (.-physicsImposter (<! (get-mesh "head")))
            back-phy (.-physicsImposter (<! (get-mesh "back")))
            tail-phy (.-physicsImposter (<! (get-mesh "tail")))
            foot1 (<! (get-mesh "foot 1"))]
        (.addJoint head-phy back-phy
                   (js/BABYLON.DistanceJoint. (js-obj "maxDistance" 20 "minDistance" 15)))
        (.addJoint back-phy tail-phy
                   (js/BABYLON.DistanceJoint. (js-obj "maxDistance" 30 "minDistance" 25)))

        (.addJoint back-phy (.-physicsImposter foot1)
                   (js/BABYLON.DistanceJoint. (js-obj "maxDistance" 40 "minDistance" 35)))
        (doseq [x (range 3)
                z (range 2)]
          (when-not (and (zero? x) (zero? z))
            (let [foot (.createInstance foot1 (str "foot" x z))]
              (set! (.-position foot) (vec3 (* x -20) 40 (* z -20)))
              (add-physics foot scene)
              (.addJoint back-phy (.-physicsImposter foot)
                         (js/BABYLON.DistanceJoint. (js-obj "maxDistance" 40 "minDistance" 35)))))))

      (let [resize-fn #(.resize engine)
            render-fn #(.render scene)]
        (.addEventListener js/window "resize" resize-fn)
        (.runRenderLoop engine render-fn)
        (set! (.-stopMagbearEngine js/window)
              #(do
                 (.removeEventListener js/window "resize" resize-fn)
                 (.stopRenderLoop engine render-fn))))))

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

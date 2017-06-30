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

(declare sync-world!)

(defmulti sync-attr!
  (fn [{:keys [attr] :as sync-args} refs]
    attr))

(defmethod sync-attr! :default
  [{:keys [goal-attr-val]}]
  goal-attr-val)

(defmethod sync-attr! :import
  [{:keys [elem-id goal-attr-val]}
   {:keys [scene objects* world-actual* world-goal*] :as refs}]
  (let [[dir file] goal-attr-val]
    (js/BABYLON.SceneLoader.ImportMesh
     "" dir file scene
     ;; this is a hot racy mess: what if another mesh has been loaded
     ;; in the meantime? Shouldn't overwrite it with an old one, or
     ;; leave both in the real scene.
     (fn mesh-loaded [meshes particle-systems]
       (swap! objects* update elem-id assoc
              :mesh (aget meshes 0)
              :particle-systems particle-systems)
       (sync-world! refs)))
    goal-attr-val))

(defmethod sync-attr! :instance
  [{:keys [elem-id goal-attr-val]}
   {:keys [objects*] :as refs}]
  (when-let [src-mesh (get-in @objects* [goal-attr-val :mesh])]
    (let [new-mesh (.createInstance src-mesh (name elem-id))]
      (swap! objects* assoc-in [elem-id :mesh] new-mesh)
      ;; can't sync yet because we haven't yet updated world-actual*,
      ;; yet without this sync a pending update could get lost:
      #_(sync-world! refs))
    goal-attr-val))

(defmethod sync-attr! :color
  [{:keys [elem-id goal-attr-val]} {:keys [scene objects*]}]
  (when-let [mesh (get-in @objects* [elem-id :mesh])]
    (let [[r g b] goal-attr-val
          material (js/BABYLON.StandardMaterial. "m1" scene)]
      (set! (.-diffuseColor material) (color3 r g b))
      (set! (.-material mesh) material)
      goal-attr-val)))

(defmethod sync-attr! :position
  [{:keys [elem-id goal-attr-val]} {:keys [scene objects*]}]
  (when-let [mesh (get-in @objects* [elem-id :mesh])]
    (let [[x y z] goal-attr-val]
      (set! (.-position mesh) (vec3 x y z))
      goal-attr-val)))

;; TODO: make sure only one sync-world! is running at a time.
(defn sync-world!
  "Compare world-goal* with world-actual* and apply appropriate side
  effects to move actual toward goal as immediately as possible"
  [{:keys [world-actual* world-goal*] :as refs}]
  (let [actual @world-actual*
        goal @world-goal*]
    (doseq [elem-id (into (set (keys actual)) (keys goal))
            :let [actual-elem (get actual elem-id)
                  goal-elem (get goal elem-id)]
            :when (not= actual-elem goal-elem)
            attr (into (set (keys actual-elem)) (keys goal-elem))
            :let [actual-attr-val (get actual-elem attr)
                  goal-attr-val (get goal-elem attr)]
            :when (not= actual-attr-val goal-attr-val)]
      (let [new-attr-val
            (sync-attr!
             {:attr attr, :elem-id elem-id, :goal-attr-val goal-attr-val}
             refs)]
        (swap! world-actual* assoc-in [elem-id attr] new-attr-val)))))

(defn ^:export main []
  (go
    (when (.-stopMagbearEngine js/window)
      ((.-stopMagbearEngine js/window)))
    (let [canvas (.getElementById js/document "renderCanvas")
          engine (js/BABYLON.Engine. canvas true)
          scene (js/BABYLON.Scene. engine)
          start-time (cljs.core/system-time)

          objects* (atom {})
          world-actual* (atom {})
          world-goal*
          (atom
           (into
            {:head {:import ["stl/" "magbear - head.stl"]
                    :color [0.2 0.4 0.9]}
             :back {:import ["stl/" "magbear - back.stl"]
                    :color [0.2 0.4 0.7]}
             :tail {:import ["stl/" "magbear - tail.stl"]
                    :color [0.2 0.4 0.7]}
             :foot00 {:import ["stl/" "magbear - foot 1.stl"]
                      :color [0.2 0.4 0.7]}}
            (for [x (range 3)
                  z (range 2)
                  :when (not (and (zero? x) (zero? z)))]
              [(keyword (str "foot" x z))
               {:instance :foot00
                :base-position [(* x -20) 0 (* z -20)]}])))

          refs {:objects* objects*
                :world-actual* world-actual*
                :world-goal* world-goal*
                :canvas canvas
                :engine engine
                :scene scene}]
      (set! (.-enableOfflineSupport engine) false)
      (swap! world-goal*
             (fn [goal]
               (reduce-kv
                (fn [goal elem-id elem]
                  (let [base-position (-> (get elem :base-position [0 0 0])
                                          (update 1 + 10))]
                    (-> goal
                        (assoc-in [elem-id :base-position] base-position)
                        (assoc-in [elem-id :position] base-position))))
                goal
                goal)))
      (sync-world! refs)

      (let [ground (js/BABYLON.MeshBuilder.CreateTiledGround
                    "ground" (js-obj "xmin" -500, "xmax" 500,
                                     "zmin" -500, "zmax" 500,
                                     "subdivisions" (js-obj "w" 20 "h" 20))
                    scene)
            ground-material (js/BABYLON.StandardMaterial. "ground-material" scene)]
        (set! (.-diffuseTexture ground-material)
              (js/BABYLON.Texture. "grass.jpg" scene))
        (set! (.-specularColor ground-material) (color3 0.0 0.0 0.0))
        (set! (.-material ground) ground-material))

      #_(let [ground (js/BABYLON.MeshBuilder.CreateSphere
                    "sphere" (js-obj "diameter" 30 "arc" 0.6)
                    scene)
            ground-material (js/BABYLON.StandardMaterial. "ground-material" scene)]
        (set! (.-diffuseColor ground-material) (color3 0.4 0.4 0.6))
        (set! (.-bumpTexture ground-material) (js/BABYLON.Texture. "normalMap.jpg" scene))
        (set! (.-backFaceCulling ground-material) false)
        (set! (.-material ground) ground-material))

      (set! (.-clearColor scene) (color3 0.2 0.2 0.267))
      (let [light (js/BABYLON.HemisphericLight. "light1" (vec3 0 1 0) scene)]
        (set! (.-intesity light) 0.5))

      (let [mouse-down (atom false)
            camera-pos (atom nil)
            render-frames (atom 5)
            camera (doto (js/BABYLON.ArcRotateCamera.
                          "Camera" (/ PI 2) 1.2 120 (vec3) scene)
                     (.attachControl canvas true)
                     (.setTarget (vec3)))
            resize-fn #(.resize engine)
            render-fn (fn mb-render []
                        (let [t (- (cljs.core/system-time) start-time)
                              x-offset (* 3 (/ t 300))]
                          (swap! world-goal*
                                 (fn [goal]
                                   (let [goal
                                         (reduce
                                          (fn [goal [x z foot-id]]
                                            (let [[base-x base-y base-z] (get-in goal [foot-id :base-position])
                                                  theta (+ (* Math/PI (- x z)) (/ t 300))]
                                              (assoc-in goal [foot-id :position]
                                                        [(+ base-x x-offset (* -3 (Math/cos theta)))
                                                         (+ base-y (Math/max 0 (* 5 (Math/sin theta))))
                                                         base-z])))
                                          goal
                                          (for [x (range 3)
                                                z (range 2)]
                                            [x z (keyword (str "foot" x z))]))

                                         goal
                                         (reduce (fn [goal elem-id]
                                                   (assoc-in goal [elem-id :position 0] x-offset))
                                                 goal
                                                 [:head :back :tail])]
                                     goal)))
                          (sync-world! refs))

                        (.render scene)
                        #_(when (or @mouse-down (> @render-frames 0))
                          (swap! render-frames dec)
                          (let [old-pos (.clone (.-position camera))]
                            (.render scene)
                            (when (not (.equals (.-position camera) old-pos))
                              (reset! render-frames 2)))))]
        (.addEventListener canvas "mousedown" #(reset! mouse-down true))
        (.addEventListener canvas "mouseup"   #(reset! mouse-down false))
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

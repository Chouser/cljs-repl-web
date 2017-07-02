(ns magbear.model)

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

(defmethod sync-attr! :rotation
  [{:keys [elem-id goal-attr-val]} {:keys [scene objects*]}]
  (when-let [mesh (get-in @objects* [elem-id :mesh])]
    (let [[x y z] goal-attr-val]
      (set! (.-rotation mesh) (vec3 x y z))
      goal-attr-val)))

(defmethod sync-attr! :parent
  [{:keys [elem-id goal-attr-val]} {:keys [objects*]}]
  (when-let [mesh (get-in @objects* [elem-id :mesh])]
    (when-let [parent-mesh (get-in @objects* [goal-attr-val :mesh])]
      (set! (.-parent mesh) parent-mesh))))

(defmethod sync-attr! :pivot-position
  [{:keys [elem-id goal-attr-val]} {:keys [objects*]}]
  (when-let [mesh (get-in @objects* [elem-id :mesh])]
    (let [[x y z] goal-attr-val]
      (.setPivotMatrix mesh (js/BABYLON.Matrix.Translation x y z))
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
            (try
              (sync-attr!
               {:attr attr, :elem-id elem-id, :goal-attr-val goal-attr-val}
               refs)
              (catch js/Error e
                (js/console.log "Error in sync-attr!"
                                (js-obj "attr" attr
                                        "elem-id" elem-id
                                        "goal-attr-val" goal-attr-val))
                goal-attr-val))]
        (swap! world-actual* assoc-in [elem-id attr] new-attr-val)))))

(defn degrees [d]
  (/ (* Math/PI d) 180))

#_
(defn set-curl-ratio! [r]
  (let [back-angle (degrees (* 100 r))
        tail-angle (degrees (* 80 r))]
    (swap! world-goal* #(-> %
                            (assoc-in [:back :rotation] [0 0 back-angle])
                            (assoc-in [:tail :rotation] [0 0 tail-angle])))))

(defn update-curl-ratio [world-goal r]
  (let [back-angle (degrees (* 100 r))
        tail-angle (degrees (* 80 r))]
    (-> world-goal
        (assoc-in [:back :rotation] [0 0 back-angle])
        (assoc-in [:tail :rotation] [0 0 tail-angle]))))

(defn update-pivot-position [elem [x y z]]
  (-> elem
      (assoc :position [(- x) (- y) (- z)])
      (assoc :pivot-position [x y z])))

(def world-actual* (atom {}))

(let [parts
      {:head {:import ["stl/" "magbear - head.stl"]
              :position [0 10 0]
              :color [0.2 0.4 0.9]}
       :back {:import ["stl/" "magbear - back.stl"]
              :parent :head
              :color [0.2 0.4 0.7]}
       :tail {:import ["stl/" "magbear - tail.stl"]
              :parent :back
              :color [0.2 0.4 0.7]}
       :foot00 {:import ["stl/" "magbear - foot 1.stl"]
                :parent :head
                :position [0 0 0]
                :color [0.2 0.4 0.7]}}

      parts (-> parts
                (update :back update-pivot-position [-11 -10 0])
                (update :tail update-pivot-position [7.8 -9.2 0]))

      more-feet
      (into {} (for [x (range 3)
                     z (range 2)
                     :when (not (and (zero? x) (zero? z)))]
                 [(keyword (str "foot" x z))
                  {:instance :foot00
                   :parent :head
                   :position [(* x -20) 0 (* z -20)]}]))]
  (def world-goal* (atom (merge parts more-feet))))

(defn init []
  (prn @world-goal*)
  (when (.-stopMagbearEngine js/window)
    ((.-stopMagbearEngine js/window)))
  (let [canvas (.getElementById js/document "renderCanvas")
        engine (js/BABYLON.Engine. canvas true)
        scene (js/BABYLON.Scene. engine)
        start-time (cljs.core/system-time)
        objects* (atom {})
        refs {:objects* objects*
              :world-actual* world-actual*
              :world-goal* world-goal*
              :canvas canvas
              :engine engine
              :scene scene}]
    (set! (.-enableOfflineSupport engine) false)
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
                      (let [t (- (cljs.core/system-time) start-time)]
                        ;; Walking:
                        #_(swap! world-goal*
                                 (fn [goal]
                                   (let [goal (assoc-in goal [:head :position 0] (* 3 (/ t 300)))
                                         goal
                                         (reduce
                                          (fn [goal [x z foot-id]]
                                            (let [theta (+ (* Math/PI (- x z)) (/ t 300))]
                                              (assoc-in goal [foot-id :position]
                                                        [(+ (* x -20) (* -3 (Math/cos theta)))
                                                         (Math/max 0 (* 5 (Math/sin theta)))
                                                         (* z -20)])))
                                          goal
                                          (for [x (range 3)
                                                z (range 2)]
                                            [x z (keyword (str "foot" x z))]))]
                                     goal)))
                        (let [ratio (/ (+ 1 (Math/sin (/ t 1000))) 2)]
                          (swap! world-goal* update-curl-ratio ratio))
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

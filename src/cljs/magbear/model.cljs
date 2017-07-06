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

(defmulti sync-mode!
  (fn [{:keys [from-mode to-mode]}]
    [from-mode to-mode]))

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

(defmethod sync-attr! :sphere
  [{:keys [elem-id goal-attr-val]}
   {:keys [objects* scene] :as refs}]

  (let [{:keys [diameter segments arc]} goal-attr-val]
    (swap! objects* assoc-in [elem-id :mesh]
           (js/BABYLON.MeshBuilder.CreateSphere
            (name elem-id) (js-obj "diameter" diameter,
                                   "segments" segments,
                                   "arc" arc)
            scene))
    goal-attr-val))

(defmethod sync-attr! :is-visible
  [{:keys [elem-id goal-attr-val]} {:keys [objects*]}]
  (when-let [mesh (get-in @objects* [elem-id :mesh])]
    (set! (.-isVisible mesh) goal-attr-val)
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

(defmethod sync-attr! :mode
  [{:keys [elem-id goal-attr-val]}
   {:keys [objects* world-actual* world-goal*]}]
  ;; This is a mess of atoms and swapping. Think of a better way.
  (let [now (cljs.core/system-time)
        new-actual (swap! world-actual*
                          (fn [world-actual]
                            (let [actual-mode (get-in world-actual [elem-id :mode])]
                              (if (map? actual-mode)
                                world-actual
                                (assoc-in world-actual [elem-id :mode]
                                          {:from-mode actual-mode
                                           :to-mode goal-attr-val
                                           :start-time now
                                           :from-world world-actual})))))
        {:keys [from-mode to-mode start-time from-world]
         :as new-actual-mode} (get-in new-actual [elem-id :mode])
        mode-duration (- now start-time)

        [done-mode new-world-goal]
        (try
          (sync-mode! {:from-mode from-mode
                       :to-mode to-mode
                       :from-world from-world
                       :mode-duration mode-duration
                       :world-goal @world-goal*})
          (catch js/Error e
            (js/console.log "Error in sync-mode!"
                            (pr-str {:from-mode from-mode
                                     :to-mode to-mode
                                     :start-time start-time})
                            e)
            [to-mode @world-goal*]))]
    (reset! world-goal* new-world-goal)
    (if (= :continue done-mode)
      new-actual-mode
      done-mode)))

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
                                (pr-str
                                 {:attr attr
                                  :elem-id elem-id
                                  :actual-val actual-attr-val
                                  :goal-val goal-attr-val})
                                e)
                goal-attr-val))]
        (swap! world-actual* assoc-in [elem-id attr] new-attr-val)))))

(defn degrees [d]
  (/ (* Math/PI d) 180))

(def normal-height 15)
(def step-height 5)

(defn scale [ratio min max]
  (+ min (* (- max min) ratio)))

(defn update-curl-ratio [goal r]
  (let [mode-split 0.5
        retract-limit -6
        retract-ratio (if (< r mode-split) (* r 2) 1)
        curl-ratio (if (< r mode-split) 0 (- (* r 2) 1))
        head-angle (degrees (* -90 curl-ratio))
        back-angle (degrees (* 100 curl-ratio))
        tail-angle (degrees (* 80 curl-ratio))
        ;; foot-extension is 0 when feet are flush with chin, positive
        ;; when in walking position, negative when retracted
        foot-extension (scale retract-ratio normal-height retract-limit)
        foot-distance (- 10 foot-extension)

        goal (reduce
              (fn [goal [x z foot-id]]
                (assoc-in goal [foot-id :position 1] foot-distance))
              goal
              (for [x (range 3)
                    z (range 2)]
                [x z (keyword (str "foot" x z))]))]
    (-> goal
        (assoc-in [:head :position 1] (Math/max 0 foot-extension))
        (assoc-in [:head :rotation] [0 0 head-angle])
        (assoc-in [:back :rotation] [0 0 back-angle])
        (assoc-in [:tail :rotation] [0 0 tail-angle]))))

(defn easy-ratio-over-time [total now]
  (/ (+ 1 (Math/cos (* Math/PI (+ 1 (/ now total))))) 2))

(defmethod sync-mode! [nil :stand] [{:keys [mode-duration world-goal]}]
  [:stand world-goal])

(defmethod sync-mode! [nil :walk] [{:keys [mode-duration world-goal]}]
  [:stand world-goal])

(defmethod sync-mode! [:stand :curl] [{:keys [mode-duration world-goal]}]
  (let [ratio (easy-ratio-over-time 3000 mode-duration)]
    (if (< mode-duration 3000)
      [:continue (update-curl-ratio world-goal ratio)]
      [:curl     (update-curl-ratio world-goal 1)])))

(defmethod sync-mode! [:curl :stand] [{:keys [mode-duration world-goal]}]
  (let [ratio (easy-ratio-over-time 3000 mode-duration)]
    (if (< mode-duration 3000)
      [:continue (update-curl-ratio world-goal (- 1 ratio))]
      [:stand     (update-curl-ratio world-goal 0)])))

(defmethod sync-mode! [:stand :walk] [{:keys [mode-duration world-goal]}]
  [:ready-to-walk world-goal])

(defmethod sync-mode! [:ready-to-walk :stand] [{:keys [mode-duration world-goal]}]
  [:stand world-goal])

(defmethod sync-mode! [:ready-to-walk :curl] [{:keys [mode-duration world-goal]}]
  [:stand world-goal])

(defmethod sync-mode! [:ready-to-walk :walk] [{:keys [mode-duration world-goal from-world]}]
  (let [theta (/ mode-duration 300)
        world-goal (assoc-in world-goal
                             [:magparent :position 0]
                             (+ (get-in from-world [:magparent :position 0])
                                (* 3
                                   (+ 1
                                      (if (< theta Math/PI)
                                        (Math/cos (+ Math/PI theta))
                                        (+ 2 (Math/cos theta)))))))
        world-goal
        (reduce
         (fn [world-goal [x z foot-id]]
           (let [theta (if (odd? (- x z)) theta (+ theta Math/PI))]
             (assoc-in world-goal [foot-id :position]
                       [(+ (* x -20) (* -3 (Math/cos theta)))
                        (+ 10
                           (- normal-height)
                           (* step-height
                              (Math/max 0 (Math/sin theta))))
                        (* z -20)])))
         world-goal
         (for [x (range 3)
               z (range 2)]
           [x z (keyword (str "foot" x z))]))]
    (if (> theta (* 2 Math/PI))
      [:ready-to-walk world-goal]
      [:continue world-goal])))

(defn update-pivot-position [elem [x y z]]
  (-> elem
      (assoc :position [(- x) (- y) (- z)])
      (assoc :pivot-position [x y z])))

(def world-actual* (atom {}))

(let [goal
      (->
       {:magparent {:sphere {:segments 1, :diameter 30}
                    :is-visible false
                    :position [0 0 0]
                    :mode :walk}
        :head {:import ["stl/" "magbear - head.stl"]
               :parent :magparent
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

       (into (for [x (range 3)
                   z (range 2)
                   :when (not (and (zero? x) (zero? z)))]
               [(keyword (str "foot" x z))
                {:instance :foot00
                 :parent (nth [:head :back :tail] x)
                 :position [(* x -20) 0 (* z -20)]}]))

       (update :head update-pivot-position [-49 0 0])
       (update :back update-pivot-position [-11 -10 0])
       (update :tail update-pivot-position [7.8 -9.2 0])
       (update-curl-ratio 0))]
  (def world-goal* (atom goal)))

(defn set-mode! [mode]
  (swap! world-goal* assoc-in [:magparent :mode] mode))

(defn init []
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

    (set! (.-clearColor scene) (color3 0.2 0.2 0.267))
    (let [light (js/BABYLON.HemisphericLight. "light1" (vec3 0 1 0) scene)]
      (set! (.-intesity light) 0.5))

    (let [mouse-down (atom false)
          render-frames (atom 5)
          camera (doto (js/BABYLON.ArcRotateCamera.
                        "Camera" (/ PI 2) 1.2 120 (vec3) scene)
                   (.attachControl canvas true)
                   (.setTarget (vec3)))
          resize-fn #(.resize engine)
          render-fn (fn mb-render []
                      (let [t (- (cljs.core/system-time) start-time)
                            step-height 5]
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

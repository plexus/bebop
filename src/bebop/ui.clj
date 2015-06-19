(ns bebop.ui
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [bebop.music :as music]
            [overtone.music.rhythm :refer [metro-bpm]]
            [bebop.music.rhythm :refer [beat-fraction bar-fraction]]))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)

  @bebop.music/song)

(defn handle-keypress [state keymap]
  (let [key-code (:key-code keymap)
        key (:key keymap)]

    (cond
      (= 32 key-code)
      (music/start-stop!)

      (= :+ key)
      (music/inc-bpm!)

      (= :- key)
      (music/dec-bpm!)

      :else
      (do
        (println keymap)
        state))))

(defn update-state [state]
  @bebop.music/song)

(defmacro isolate-matrix [& body]
  `(do
     (q/push-matrix)
     ~@body
     (q/pop-matrix)))

(defn draw-state [state]
  (q/background 240)

  (let [metro (:metro state)
        bpm (metro-bpm metro)
        beat (if metro (metro) 0)
        bar-fract (bar-fraction metro)
        beat-frac (beat-fraction metro)]

    (q/with-translation [(- (q/width) 130) (- (q/height) 125)]
      (q/fill 90)
      (q/stroke 160)
      (q/arc 60 60 120 120 (q/radians -90) (q/radians (- (* 360 bar-fract) 90)))
      (if (< beat-frac 0.2)
        (q/fill 100 120 200)
        (q/fill 240))
      (q/stroke 240)
      (q/ellipse 60 60 100 100))

    (isolate-matrix
     (q/fill 90)
     (q/scale 4)
     (q/text-align :center)
     (q/text (str bpm) 238 180))





    #_(let [factor (q/sin (q/radians (* 360 beat-frac)))
          size (*  factor 20)]
      (q/fill (* factor 255) 120 200)
      (q/ellipse 50 (- (q/height) 50) size size))))

(q/defsketch bebop
  :title "You spin my circle right round"
  :size [1024 768]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode]
  :key-pressed handle-keypress)

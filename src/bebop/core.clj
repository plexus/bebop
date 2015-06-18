(ns bebop.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [overtone.live :as o]))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)

  {:bpm 110
   :beats-per-bar 4
   :metro nil})

(defn handle-keypress [state keymap]
  (let [key-code (:key-code keymap)
        key (:key keymap)
        bpm (:bpm state)
        metro (:metro state)]

    (cond
      (= 32 key-code)
      (if metro
        (assoc state :metro nil)
        (assoc state :metro (o/metronome bpm)))

      (= :+ key)
      (do
        (when metro
          (metro :bpm (inc bpm)))
        (update-in state [:bpm] inc))

      (= :- key)
      (do
        (when metro
          (metro :bpm (dec bpm)))
        (update-in state [:bpm] dec))

      :else
      (do
        (println keymap)
        state))))

(defn update-state [state]
  state)

(defn now []
  (System/currentTimeMillis))

(defn bpm->beat-millis
  "How many milliseconds does one beat take"
  [bpm]
  (/ 60000.0 bpm))

(defn beat-fraction
  "How far in the current beat are we (0 to 1)"
  [bpm metro]
  (if metro
    (let [beat (metro)
          ms (metro beat)]
      (/ (- ms (now)) (bpm->beat-millis bpm)))
    0))

(defn bar-fraction
  "How far in the current bar are we (0 to 1)"
  [bpm beats-per-bar metro]
  (if metro
    (let [beat (dec (metro))
          bar-start-beat (- beat (mod beat beats-per-bar))
          ms (metro bar-start-beat)]
      (/ (- (now) ms) (bpm->beat-millis bpm) beats-per-bar)
      )
    0))

(defn draw-state [state]
  (q/background 240)

  (let [bpm (:bpm state)
        metro (:metro state)
        beats-per-bar (:beats-per-bar state)
        beat (if metro (metro) 0)
        bar-fract (bar-fraction bpm beats-per-bar metro)
        beat-frac (beat-fraction bpm metro)]

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

(defmacro isolate-matrix [& body]
  `(do
     (q/push-matrix)
     ~@body
     (q/pop-matrix)))

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

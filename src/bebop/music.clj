(ns bebop.music
  (:require [overtone.core :refer :all]
            [bebop.music.rhythm :refer :all]))

;(connect-external-server 6543)

(def initial-song {:metro (pause-metronome (metronome 90))})

(def song (atom initial-song))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands

(defn start-stop [song]
  (let [metro (:metro song)]
    (assoc song :metro
           (if (paused? metro)
             (unpause-metronome metro)
             (pause-metronome metro)))))

(defn update-bpm [f]
  (fn [song]
    (let [metro (:metro song)
          new-bpm (f (metro-bpm metro))]
      (metro :bpm new-bpm))))

(def inc-bpm! (update-bpm inc))
(def dec-bpm! (update-bpm dec))

(defn start-stop! []
  (swap! song start-stop))

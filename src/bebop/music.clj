(ns bebop.music
  (:require [overtone.core :refer :all]
            [clojure.java.io :as io]
            [bebop.music.rhythm :refer :all]
            [bebop.music.bbt :refer :all]))

;(connect-external-server 6543)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; drum samples
;;

(defn resource-sample [name]
  (sample (.getPath (io/resource name))))

(def drums {:kick (resource-sample "kick_OH_F_1.wav")
            :snare (resource-sample "snare_OH_F_1.wav")
            :hihat (resource-sample "hihatClosed_OH_F_1.wav")
            :hi-tom (resource-sample "hiTom_OH_FF_1.wav")
            :lo-tom (resource-sample "loTom_OH_FF_1.wav")
            :cowbell (resource-sample "cowbell_FF_1.wav")
            :crash (resource-sample "crash1_OH_FF_1.wav")
            :ride (resource-sample "ride1_OH_FF_1.wav")})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; song

(def initial-song {:metro (pause-metronome (metronome 90))
                   :drums [
                           [ [1 1 0] :kick  ]
                           [ [1 2 0] :snare ]
                           [ [1 3 0] :kick  ]
                           [ [1 3 32] :kick ]
                           [ [1 3 64] :kick ]
                           [ [1 4 0] :snare ]
                          ]})

(def song (atom initial-song))

(defn drum-loop [mbeat]
  (let [song @song
        metro (:metro song)
        drum-pattern (:drums song)
        max-bars (apply max (map (comp first first) drum-pattern))
        bar-beat (mbeat->bar-beat metro mbeat)
        next-mbeat (inc mbeat)]

    (schedule-beat metro max-bars bar-beat drum-pattern drums)
    (if (not (paused? metro))
      (apply-by (metro next-mbeat) #'drum-loop [next-mbeat]))))

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
      (metro :bpm new-bpm))
    song))

(defn inc-bpm! [] (swap! song (update-bpm inc)))
(defn dec-bpm! [] (swap! song (update-bpm dec)))

(defn start-stop! []
  (swap! song start-stop)
  (if (paused? (:metro @song))
    (stop)
    (let [next-beat ((:metro @song))]
      (drum-loop (if (= 1 next-beat) 0 next-beat)))))

(ns bebop.music.rhythm
  (:require [overtone.music.rhythm :refer :all]
            [overtone.music.time :refer [now]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metro utilities

(defprotocol IPausedMetronome
  (paused? [metro]))

(extend overtone.music.rhythm.Metronome
  IPausedMetronome
  {:paused? (fn [metro] false)})

(deftype PausedMetronome [offset bpm bpb]
  IMetronome
  (metro-start [metro] (- (now) offset))
  (metro-start [metro start-beat] 0)
  (metro-bar-start [metro] 0)
  (metro-bar-start [metro start-bar] 0)
  (metro-tick  [metro] (beat-ms 1 bpm))
  (metro-tock  [metro] (beat-ms bpb bpm))
  (metro-beat  [metro] (inc (long (/ offset (metro-tick metro)))))
  (metro-beat  [metro b] (+ (* b (metro-tick metro)) (metro-start metro)))
  (metro-bar   [metro] (inc (long (/ offset (metro-tock metro)))))
  (metro-bar   [metro b] (+ (* b (metro-tock metro)) (metro-start metro)))
  (metro-bpm   [metro] bpm)
  (metro-bpm   [metro new-bpm] 0)
  (metro-bpb   [metro] bpb)
  (metro-bpb   [metro new-bpb] 0)

  IPausedMetronome
  (paused? [metro] true)

  clojure.lang.IFn
  (invoke [this] (metro-beat this))
  (invoke [this arg]
    (cond
     (number? arg) (metro-beat this arg)
     (= :bpm arg) (metro-bpm this))))

(defn pause-metronome [metro]
  (PausedMetronome. (- (now) (metro-start metro)) (metro-bpm metro) (metro-bpb metro)))

(defn unpause-metronome [metro]
  (let [start (atom (metro-start metro))
        bar-start (atom @start)
        bpm (atom (metro-bpm metro))
        bpb (atom (metro-bpb metro))]
    (overtone.music.rhythm.Metronome. start bar-start bpm bpb)))

(defn beat-fraction
  "How far in the current beat are we (0 to 1)"
  [metro]
  (if metro
    (/
     (- (now) (metro (dec (metro))))
     (metro-tick metro))
    0))

(defn bar-fraction
  "How far in the current bar are we (0 to 1)"
  [metro]
  (if metro
    (/
     (- (now) (metro-bar metro (dec (metro-bar metro))))
     (metro-tock metro))
    0))

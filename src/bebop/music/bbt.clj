(ns bebop.music.bbt
  (:require [overtone.core :refer :all]
            [bebop.music.rhythm :refer :all]))

;; BBT or bar.beat.tick is a notation for musical time used in Akai's
;; MPC line of instruments.
;;
;; Bar: The bar number, starts at 1. So if a song has three bars, then
;; bar can be 1, 2, or 3.
;;
;; Beat: The beat in the bar. When a song has 4 beats per bar (bpb),
;; then the first five beats will be written as 1.1, 1.2, 1.3, 1.4, 2.1.
;;
;; Tick: Fraction of a beat with a resolution of 96 ticks per beat. So
;; 1.3.0 is exactly at the 3rd beat of the first bar. 1.3.48 is
;; exactly halfway between the third and the fourth beat.

;; For what follows these times are written as vectors, e.g. [1 4 0]
;; "metro" is an overtone.music.rhythm.Metronome.
;;
;; A pattern is a list of time/instrument pairs, like [[1 1 0] :kick
;;                                                     [1 2 0] :snare]

(defn bar-beat->mbeat
  "Convert from [bar beat] to a metronome beat (simple integer)"
  [metro [bar beat]]
  (+ (dec beat)
     (* (dec bar)
        (metro-bpb metro))))

(defn mbeat->bar-beat
  "Convert from a metronome beat (integer, counts from 1 up) to a [bar beat]"
  [metro mbeat]
  (let [bpb (metro-bpb metro)]
    [(inc (quot mbeat bpb)) (inc (mod mbeat bpb))]))


(defn bar-beat->ms
  "Convert a [bar beat] into a timestamp, according to the metronome."
  [metro [bar beat]]
  (metro (bar-beat->mbeat metro [bar beat])))

(defn tick->ms
  "Convert a tick (0 to 95) to a fraction of the duration of one beat in ms."
  [metro tick]
  (* tick 1/96 (metro-tick metro)))

(defn bbt->ms
  "Convert a [bar beat tick] into a timestamp, according to the metronome."
  [metro [bar beat tick]]
  (+ (bar-beat->ms metro [bar beat]) (tick->ms metro tick)))


(defn inc-beat
  "Increment a [bar beat] by one beat. Takes into account the
  bars-ber-beat of the metronome."
  [metro [bar beat]]
  (if (= beat (metro-bpb metro))
    [(inc bar) 1]
    [bar (inc beat)]))

(defn mod-bar
  "Take the modulo of a bar and the total amount of bars in a
  song. It's like a regular integer modulo, except that bars start at
  1 instead of zero."
  [max-bars bar]
  (inc (mod (dec bar) max-bars)))


(defn slice-pattern
  "From a pattern, return only the events for a certain bar and beat"
  [pattern [bar beat]]
  (filter
   (fn [[[br bt _] _]]
     (= [br bt] [bar beat]))
   pattern))

(defn schedule-beat [metro max-bars [bar beat] pattern inst-map]
  (doseq [[[_ _ tick] inst] (slice-pattern pattern [(mod-bar max-bars bar) beat])]
    (apply-at (bbt->ms metro [bar beat tick]) (inst-map inst))))

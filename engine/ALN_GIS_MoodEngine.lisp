;; ALN.GIS.GLOBAL-MOOD ENGINE + WORLD MEMORY LAYER
;; Author: ImmersiveNPC-Lab
;; License: Prototype-Only

(defpackage :aln-gis
  (:use :cl)
  (:export :init-global-mood-engine
           :shift-mood-event
           :register-npc
           :propagate-mood
           :trigger-mood-anomaly
           :record-world-event
           :recall-memory
           :madness-event-modifier
           :tick-contamination
           :spawn-recursive-madness))

(in-package :aln-gis)

;;;; --------------------
;;;; Global Structures
;;;; --------------------

(defclass global-mood ()
  ((fear :initform 0.20 :accessor fear)
   (despair :initform 0.10 :accessor despair)
   (chaos :initform 0.00 :accessor chaos)
   (hope :initform 0.00 :accessor hope)))

(defvar *global-mood-engine* (make-instance 'global-mood))
(defvar *mood-subscribers* (make-hash-table))
(defvar *world-memory-log* nil)
(defvar *memory-contamination* 0.0)

;;;; --------------------
;;;; Mood Engine Core
;;;; --------------------

(defun init-global-mood-engine ()
  (setf *global-mood-engine* (make-instance 'global-mood)
        *mood-subscribers* (make-hash-table :test 'eq)
        *world-memory-log* nil
        *memory-contamination* 0.0)
  (format t "[DEBUG] Global Mood Engine initialized.~%"))

(defun shift-mood-event (event)
  ;; Handles mood shifts for various events
  (ecase event
    (:storm
     (incf (fear *global-mood-engine*) 0.2)
     (incf (despair *global-mood-engine*) 0.1))
    (:playerdeath
     (incf (fear *global-mood-engine*) 0.4)
     (incf (despair *global-mood-engine*) 0.5)
     (incf (chaos *global-mood-engine*) 0.3))
    (:festival
     (incf (hope *global-mood-engine*) 0.3))
    (:ambient
     ;; Mild ambient decay
     (decf (fear *global-mood-engine*) 0.01)
     (decf (despair *global-mood-engine*) 0.01)
     (decf (chaos *global-mood-engine*) 0.005))
    (:darkening
     (incf (despair *global-mood-engine*) 0.2))
    (otherwise
     (format t "[DEBUG] Unknown event for shift-mood-event: ~A~%" event)))
  (format t "[DEBUG] Mood event ~A triggered. Mood state: F~2$ D~2$ C~2$ H~2$~%"
          event (fear *global-mood-engine*) (despair *global-mood-engine*)
          (chaos *global-mood-engine*) (hope *global-mood-engine*))
  (propagate-mood))

(defun register-npc (npc-symbol npc-obj)
  (setf (gethash npc-symbol *mood-subscribers*) npc-obj)
  (format t "[DEBUG] NPC ~A registered with mood engine.~%" npc-symbol))

(defun current-mood-label ()
  (let ((f (fear *global-mood-engine*))
        (d (despair *global-mood-engine*))
        (c (chaos *global-mood-engine*))
        (h (hope *global-mood-engine*)))
    (cond ((and (> f d) (> f h)) :fear-heavy)
          ((and (> d f) (> d c)) :despair-heavy)
          ((> c 0.6) :chaos-rising)
          ((> h 0.5) :hope-glimmer)
          (t :neutral-dark))))

(defun propagate-mood ()
  (let ((mood (current-mood-label)))
    (maphash (lambda (sym npc)
               (funcall (getf npc :modulate-emotion) mood))
             *mood-subscribers*)
    (format t "[DEBUG] Propagated mood ~A to all NPCs.~%" mood)))

;;;; --------------------
;;;; Mood Anomaly System
;;;; --------------------

(defun trigger-mood-anomaly ()
  (let ((roll (random 1.0)))
    (when (< roll 0.05)
      (let ((event-list '(:broadcast-whisper :freeze-clocks :mirror-voices :mass-bleed-effect))
            (eid (random 4)))
        (ecase (nth eid event-list)
          (:broadcast-whisper (broadcast-whisper "ALL THE TEETH"))
          (:freeze-clocks (emit-world-event 'all-time-paused 10))
          (:mirror-voices (emit-world-event 'npc-mirror-voices))
          (:mass-bleed-effect (emit-world-event 'npc-mouths-bleed)))
        (format t "[DEBUG] Mood anomaly triggered: ~A~%" (nth eid event-list))))))

(defun broadcast-whisper (msg)
  (maphash (lambda (sym npc)
             (funcall (getf npc :whisper-line) msg))
           *mood-subscribers*)
  (format t "[DEBUG] Broadcasted whisper to all NPCs: '~A'~%" msg))

(defun emit-world-event (event &optional detail)
  (format t "[DEBUG] Global world event fired: ~A [~A]~%" event detail))

;;;; --------------------
;;;; World Memory Layer
;;;; --------------------

(defun record-world-event (event)
  (push event *world-memory-log*)
  (incf *memory-contamination* 0.05)
  (format t "[DEBUG] WorldMemory recorded event: ~A, contamination ~2$~%" event *memory-contamination*))

(defun recall-memory ()
  (if (null *world-memory-log*)
      "The world stares blankly..."
      (nth (random (length *world-memory-log*)) *world-memory-log*)))

;;;; --------------------
;;;; Madness Event Modifier
;;;; --------------------

(defparameter *madness-types* '(:insanity :amnesia :dementia :paranoia :schizofragment :hysteria :obsession))

(defun madness-event-modifier (npc)
  (let ((roll (random 1.0)))
    (when (> roll 0.85)
      (let ((type (nth (random (length *madness-types*)) *madness-types*)))
        (apply-madness-to-npc npc type)
        (record-world-event (list :madness-trigger (getf npc :symbol) type))))))

(defun apply-madness-to-npc (npc type)
  (case type
    (:insanity
     (push (format nil "nonsense-~A" (random 100)) (getf npc :vocab))
     (format t "~A babbles incoherently about '~A'.~%" (getf npc :symbol) (recall-memory)))
    (:amnesia
     (setf (getf npc :emotion) '(0 0 0 0))
     (format t "~A forgets your name mid-sentence.~%" (getf npc :symbol)))
    (:dementia
     (format t "~A repeats a decades-old memory: '~A'.~%" (getf npc :symbol) (recall-memory)))
    (:paranoia
     (format t "~A insists you are being followed by your double.~%" (getf npc :symbol)))
    (:schizofragment
     (format t "~A speaks in two voices at once yes/no mine/not mine.~%" (getf npc :symbol)))
    (:hysteria
     (format t "~A laughs until voice breaks, then whispers what you said moments ago.~%" (getf npc :symbol)))
    (:obsession
     (format t "~A fixates on a body part: 'Show me your hands, show me, show me...'.~%" (getf npc :symbol)))))

;;;; --------------------
;;;; Contamination Spiral Layer
;;;; --------------------

(defparameter *spiral-ripples* '(0.15 0.35 0.65 0.85))
(defvar *spiral-intensity* 0.0)

(defun tick-contamination (player npcs)
  "Main tick: increments contamination, triggers ripple events."
  (incf *spiral-intensity* 0.01)
  (dolist (t *spiral-ripples*)
    (when (and (>= *spiral-intensity* t) (< (- *spiral-intensity* t) 0.01))
      (cascade-event t player npcs))))

(defun cascade-event (threshold player npcs)
  (format t "[DEBUG] Contamination spiral threshold ~2$ reached.~%" threshold)
  (case threshold
    (0.15 (mapc (lambda (npc) (madness-event-modifier npc)) npcs))
    (0.35 (record-world-event (format nil "ENVIRONMENT_RIPPLE @~2$" threshold)))
    (0.65 (mapc (lambda (npc) (spawn-recursive-madness npc)) npcs))
    (0.85 (emit-world-event :global-hallucination-layer))))

(defun spawn-recursive-madness (npc)
  (let ((personalities '("Judge-Juggler" "Mailman-Mirror" "Goldfish-Cop" "Crayon-Doctor")))
    (let ((role (nth (random (length personalities)) personalities)))
      (format t "~A abruptly roleplays as ~A~%" (getf npc :symbol) role)
      (record-world-event (list :recursive-overlay (getf npc :symbol) role)))))

;;;; --------------------
;;;; Example NPC Plugins (stub)
;;;; --------------------

(defun make-npc (symbol)
  ;; Example factory, fill in with game NPC logic/handlers as needed
  (list :symbol symbol
        :emotion '(0.5 0.5 0.5 0.5)
        :vocab '("bread" "teeth" "root")
        :modulate-emotion (lambda (mood)
                            (format t "~A modulated by world mood: ~A~%" symbol mood))
        :whisper-line (lambda (msg)
                        (format t "~A whispers: '~A'~%" symbol msg))))

;;;; --------------------
;;;; Engine Initialization Example
;;;; --------------------

;; Usage:
;; (init-global-mood-engine)
;; (let ((npc1 (make-npc 'BlindBaker))
;;       (npc2 (make-npc 'ToothCollector)))
;;   (register-npc 'BlindBaker npc1)
;;   (register-npc 'ToothCollector npc2)
;;   (shift-mood-event :storm)
;;   (trigger-mood-anomaly)
;;   (tick-contamination 'player (list npc1 npc2))
;;   (record-world-event "BlindBaker gave the player a pulsing loaf with tooth")
;;   (recall-memory)
;;   (madness-event-modifier npc1))

;; FULL DEBUG (setf *debug-io* *standard-output*)

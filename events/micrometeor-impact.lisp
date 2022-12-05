(uiop:define-package :journey-of-the-hive/events/micrometeor-impact
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs :journey-of-the-hive/defscene :journey-of-the-hive/gui)
  (:local-nicknames (#:alex :alexandria))
  (:export #:*micrometeor-impact*))
(in-package :journey-of-the-hive/events/micrometeor-impact)

(defclass micrometeor-impact-scene-data ()
  ((sustenance-loss :type unsigned-byte
                 :accessor micrometeor-impact-sustenance-loss
                 :initarg :sustenance-loss)
   (capacity-loss :type unsigned-byte
                  :accessor micrometeor-impact-capacity-loss
                  :initarg :capacity-loss)
   (success-modifier :type single-float
                     :accessor micrometeor-impact-success-modifier
                     :initarg :success-modifier)
   (success-p :type boolean
              :accessor micrometeor-impact-success-p))
  (:default-initargs :success-modifier 1f0))

(defun connection-scene-data (connection)
  (get-scene-data connection '*micrometeor-impact*))

(defun (setf connection-scene-data) (new-data connection)
  (setf (get-scene-data connection '*micrometeor-impact*)
        new-data))

(defun connection-sustenance-loss (connection)
  (micrometeor-impact-sustenance-loss (connection-scene-data connection)))

(defun (setf connection-sustenance-loss) (new-quantity connection)
  (setf (micrometeor-impact-sustenance-loss (connection-scene-data connection))
        new-quantity))

(defun connection-capacity-loss (connection)
  (micrometeor-impact-capacity-loss (connection-scene-data connection)))

(defun (setf connection-capacity-loss) (new-quantity connection)
  (setf (micrometeor-impact-capacity-loss (connection-scene-data connection))
        new-quantity))

(defun connection-success-modifier (connection)
  (micrometeor-impact-success-modifier (connection-scene-data connection)))

(defun (setf connection-success-modifier) (new-modifier connection)
  (setf (micrometeor-impact-success-modifier (connection-scene-data connection))
        new-modifier))

(defun connection-success-p (connection)
  (micrometeor-impact-success-p (connection-scene-data connection)))

(defun (setf connection-success-p) (new-value connection)
  (setf (micrometeor-impact-success-p (connection-scene-data connection)) new-value))

(define-event *micrometeor-impact*
  (:title "Micrometeor Impact")
  (:on-enter
   (connection)
   (let* ((sustenance (connection-sustenance connection))
          (loss (floor sustenance 3))
          (capacity-loss loss)
          (scene-data (make-instance 'micrometeor-impact-scene-data
                                     :sustenance-loss loss
                                     :capacity-loss capacity-loss)))
     (apply-effects connection :sustenance (- loss) :sustenance-capacity (- capacity-loss))
     (setf (connection-scene-data connection)
           scene-data)))
  (:display-scene
   (window)
   (let* ((sustenance-loss (connection-sustenance-loss window))
          (capacity-loss (connection-capacity-loss window)))
     (if (zerop sustenance-loss)
         (display-micrometeor-impact-empty-tank window)
         (display-micrometeor-impact-some-loss window sustenance-loss capacity-loss)))))

(defun display-micrometeor-impact-empty-tank (window)
  (with-clog-create (window-content window)
      (event-contents ()
                      (p (:content "Micrometeor impact. Must have slipped past point-defense array. Opened
                                    breach into empty tank intended for nutrient fluid. Current low sustenance
                                    storage perhaps blessing in disguise. Thrusters down. Locate standard hull
                                    patch kit. Tall facet and long-armed facet don space suits. Exit
                                    airlock. Climb along hull. Locate impact site. Apply patch. Test patch by
                                    filling tank with low pressure of expendable gas. Patch holds. Enter
                                    airlock. Thrusters up."))
                      (advance-button (window
                                       :content "Continue, and hope to locate additional sustenance.")))))

(predeclare-scene *micrometeor-impact-choose-work-facets*)

(defun display-micrometeor-impact-some-loss (window sustenance-loss capacity-loss)
  (with-clog-create (window-content window)
      (event-contents ()
                      (p (:content (format nil
                                           "-~d sustenance / -~d sustenance capacity."
                                           sustenance-loss capacity-loss)))
                      (p (:content "Micrometeor impact. Must have slipped past point-defense array. Hull
                                    breach venting nutrient fluid. Main thrusters down. Acceleration pushes
                                    ship into undesirable spiral motion."))
                      (panel (:bind choices :display :flex)
                             (event-option (window
                                            :proposal "engage maneuvering thrusters to counteract unwanted acceleration."
                                            :concern "fuel utilization."
                                            :effects '(:fuel -1)
                                            :button-content "Engage maneuvering thrusters."
                                            :target-scene *micrometeor-impact-choose-work-facets*))
                             (event-option (window
                                            :proposal "attempt to patch hull during uncontrolled acceleration."
                                            :concern "unsafe working conditions; reduced quality of patch."
                                            :button-content "Begin repairs."
                                            :target-scene *micrometeor-impact-choose-work-facets*
                                            :before-continuing (lambda (connection)
                                                                 (setf (connection-success-modifier connection)
                                                                       (* 0.6 (connection-success-modifier connection))))))))))

(predeclare-scene *micrometeor-impact-begin-repairs*)

(define-scene *micrometeor-impact-choose-work-facets*
  (:title "Micrometeor Impact - Choose Work Facets")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "Optimal facets for hull repair in recuperation cycle. Severe enough
                                     incident to wake them?"))
                       (panel (:display :flex)
                              (event-option (window
                                             :proposal "threat sufficient to necessitate waking primary repair
                                                        facets."
                                             :concern "grogginess; general sourness due to interrupted
                                                       recuperation cycle."
                                             :effects '(:morale -1)
                                             :button-content "Wake primary repair facets."
                                             :target-scene *micrometeor-impact-begin-repairs*))
                              (event-option (window
                                             :proposal "better to maintain them at optimal efficiency in case
                                                        of a greater disruption down the line. Send secondary
                                                        repair facets."
                                             :concern "reduced quality of patch."
                                             :button-content "Send secondary repair facets."
                                             :target-scene *micrometeor-impact-begin-repairs*
                                             :before-continuing (lambda (connection)
                                                                  (setf (connection-success-modifier connection)
                                                                        (* 0.8 (connection-success-modifier connection)))))))))))

(predeclare-scene *micrometeor-impact-vent-tank*)
(predeclare-scene *micrometeor-impact-install-patch*)

(define-scene *micrometeor-impact-begin-repairs*
  (:title "Micrometeor Impact - Begin Repairs")
  (:display-scene
   (window)
   (let* ((current-loss (connection-sustenance-loss window))
          (original-sustenance (+ (connection-sustenance window)
                                  current-loss))
          (total-loss-after-venting (floor original-sustenance 2))
          (vent-sustenance-delta (- total-loss-after-venting current-loss)))
     (with-clog-create (window-content window)
         (event-contents ()
                         (p (:content "Selected repair facets don space suits. Exit airlock. Traverse
                                     hull. Locate breach. Visual of light refracted by trail of ejected fluid,
                                     captured from two distinct viewpoints. Haunting and poetic. Inspection
                                     suggests force of venting fluid beyond tolerance of standard patch kit."))
                         (panel (:display :flex)
                                (event-option (window
                                               :proposal "vent fluid before installing patch. Controlled vent
                                                        will create coherent cloud, allow reclaiming nutrient
                                                        fluid after applying patch."
                                               :concern "efficiency of reclaiming vented fluid surely below
                                                       100%; will require fuel."
                                               :effects `(:sustenance ,(- vent-sustenance-delta))
                                               :button-content "Vent tank."
                                               :target-scene *micrometeor-impact-vent-tank*))
                                (event-option (window
                                               :proposal "install panel without emptying tank."
                                               :concern "patch may not hold."
                                               :button-content "Install patch."
                                               :before-continuing (lambda (connection)
                                                                    (setf (connection-success-modifier connection)
                                                                          (* 0.8 (connection-success-modifier connection))))
                                               :target-scene *micrometeor-impact-install-patch*))))))))

(define-scene *micrometeor-impact-vent-tank*
  (:title "Micrometeor Impact - Vent Tank")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "Extra-vehicular facets re-enter airlock. No need to remove suits. Strap
                                     into acceleration dampers. Open vent. Compromised tank exposed to
                                     vacuum. Resulting lurch, though expected, is jarring."))
                       (p (:content "Extract hull-repair facets from acceleration dampers. Exit
                                     airlock. Traverse hull. Return to breach location. Undesired nutrient
                                     fluid ejection no longer present."))
                       (advance-button (window
                                        :content "Install patch."
                                        :target-scene *micrometeor-impact-install-patch*))))))

(define-scene *micrometeor-impact-install-patch*
  (:title "Micrometeor Impact - Install Patch")
  (:on-enter
   (connection)
   (let* ((success-chance (connection-success-modifier connection))
          (random (random 1f0))
          (success-p (< random success-chance)))
     (setf (connection-success-p connection) success-p)
     (when success-p
       (apply-effects connection :sustenance-capacity (connection-capacity-loss connection)))))
  (:display-scene
   (window)
   (if (connection-success-p window)
       (display-micrometeor-impact-install-tank-success window)
       (display-micrometeor-impact-install-tank-failure window))))

(defun display-micrometeor-impact-install-tank-success (window)
  (let* ((overall-loss (connection-sustenance-loss window))
         (capacity-regained (connection-capacity-loss window)))
    (with-clog-create (window-content window)
        (event-contents ()
                        (p (:content (format nil "+~d sustenance capacity" capacity-regained)))
                        (p (:content "Apply patch kit per specifications. Test pressurization. Patch
                                      holds. Repair facets return to airlock, remove space suits, re-enter
                                      recuperation cycle."))
                        (p (:content (format nil "Sustenance capacity returned to pre-incident
                                                  levels. Sustenance storage depleted by ~d; current storage
                                                  ~d."
                                             overall-loss
                                             (connection-sustenance window))))
                        (panel (:display :flex)
                               (event-option (window
                                              :proposal "attempt to reclaim some of lost nutrient fluid by
                                                         maneuvering through vapor trail."
                                              :concern "fuel expenditure."
                                              :effects `(:fuel -2 :sustenance ,(ceiling overall-loss 2))
                                              :button-content "Route through vapor trail, then continue."))
                               (event-option (window
                                              :proposal "fuel waste currently of greater concern than
                                                         reclaiming nutrient fluid."
                                              :button-content "Abandon lost nutrients; continue.")))))))

(defun display-micrometeor-impact-install-tank-failure (window)
  (with-clog-create (window-content window)
      (event-contents ()
                      (p (:content "Apply patch kit per specifications despite suboptimal conditions. Repair
                                    facets express concern, uncertainty. Fit appears incorrect. Retreat repair
                                    facets around hull for patch test. Attempt to repressurize tank. Ship
                                    lurches violently. Extra-vehicular facets experience sudden, jarring
                                    acceleration. Lucky they were correctly clipped to hull. No, not luck;
                                    discipline. Return repair facets for inspection. Disturbing tear in
                                    hull. Chunks of patch kit, hull drifting away from ship. Damage level
                                    prohibits repair using shipboard resources. Would-be repair facets return
                                    to airlock, remove space suits."))
                      (advance-button (window
                                       :content "Limp onward.")))))

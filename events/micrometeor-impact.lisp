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
                     :initarg :success-modifier))
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

(define-event *micrometeor-impact*
  (:title "Micrometeor Impact")
  (:on-enter
   (connection)
   (let* ((sustenance (connection-sustenance connection))
          (sustenance-capacity (connection-sustenance-capacity connection))
          (loss (floor sustenance 3))
          ;; don't lose any capacity if you didn't lose any sustenance, because
          ;; `display-micrometeor-impact-empty-tank' just repairs the breach immediately
          (capacity-loss (if (zerop loss)
                             0
                             (floor sustenance-capacity 4)))
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

(define-scene *micrometeor-impact-begin-repairs*
  (:title "Micrometeor Impact - Begin Repairs")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "Selected repair facets don space suits. Exit airlock. Traverse
                                     hull. Locate breach. Visual of light refracted by trail of ejected fluid,
                                     captured from two distinct viewpoints. Haunting and poetic. Inspection
                                     suggests force of venting fluid beyond tolerance of standard patch kit."))
                       (panel (:bind choices :display :flex)
                              )
                       ;; TODO options:
                       ;; - if sustenance below some threshold, redistribute to auxiliary tank. if sustenance
                       ;;   above threshold, tank capacity is insufficient.
                       ;; - vent fluid before installing patch. loses significant fluid volume. offer
                       ;;   possibility of using miners to recover fluid.
                       ;; - install patch under suboptimal conditions. reduces success rate.
                       ))))

;; TODO: scene to install patch. random chance of success altered by modifier. if successful, continue. if
;; failed, lose additional sustenance and morale.

;; TODO: scene to recover vented fluid. only available if patch is successful. trade fixed fuel cost to
;; recover some fraction of sustenance lost throughout experience.

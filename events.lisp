(uiop:define-package :journey-of-the-hive/events
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs :journey-of-the-hive/defscene :journey-of-the-hive/gui)
  (:local-nicknames (#:alex :alexandria)))
(in-package :journey-of-the-hive/events)

(predeclare-scene *glittering-nebula-low-thrusters*)
(predeclare-scene *glittering-nebula-gazing-eva*)
(predeclare-scene *glittering-nebula-ignore-and-continue*)

(define-event *glittering-nebula*
  (:title "Glittering Nebula")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (div (:bind contents :hidden t)
              (p (:content "Routine maintenance on ship. Thrusters down. Tall facet and long-armed facet don
                            space suits. Exit airlock. Climb along hull. Inspection largely delegated to
                            muscle memory. Distant visual of glittering nebula, ezperienced from two
                            near-identical points of view. Minor differences in color receptors yield one
                            image shifted toward blue, another heavier on red. Both beautiful. Snapshots
                            saved. Enter airlock."))

              (p (:content "Artistic facets express desire for better view, longer exposure to nebula."))

              (panel (:bind choices :display :flex :bottom 0)
                     (div (:style *style-event-option*)
                        (p (:content "Proposal: set thrusters low to prolong exposure to nebula. Gaze through
                                      viewports."))
                        (p (:content "Concern: increased duration of journey requires additional
                                      sustinance."))
                        (unordered-list ()
                                        (list-item (:content "+1 morale"))
                                        (list-item (:content "-1 sustinance")))
                        (button (:bind low-thrusters
                                 :content "Thrusters low.")))
                     (div (:style *style-event-option*)
                        (p (:content "Proposal: offer artistic facets extra-vehicular jaunt. Attempt to
                                      capture detailed visuals; create representations."))
                        (p (:content "Concern: artistic facets lack EVA training; risk of accident."))
                        (unordered-list ()
                                        (list-item (:content "+2 morale"))
                                        (list-item (:content "-2 sustinance")))
                        (button (:bind gazing-eva
                                 :content "Don space suits. Exit airlock. Stargaze.")))
                     (div (:style *style-event-option*)
                        (p (:content "Proposal: ignore. Mission parameters do not allow delay."))
                        (p (:content "Concern: single-minded focus on mission at expense of beauty and
                                      pleasure risks morale among artistic facets."))
                        (unordered-list ()
                                        (list-item (:content "-1 morale")))
                        (button (:bind ignore-and-continue
                                 :content "Thrusters up.")))))
     (declare (ignore ignore-and-continue))
     (set-on-click low-thrusters (thunk (advance-to-scene *glittering-nebula-low-thrusters* window)))
     (set-on-click gazing-eva (thunk (advance-to-scene *glittering-nebula-gazing-eva* window)))
     (center-children choices :horizontal t :vertical nil)
     (setf (visiblep contents) t))))

(defparameter *nebula-image*
  "/img/nasa_archives_helix.jpg"
  "Copyright NASA, 2020. From https://www.nasa.gov/mission_pages/chandra/images/helix-nebula.html")

(defun display-helix-nebula (container)
  (with-clog-create container
      (img (:url-src *nebula-image*
            :alt-text "Helix Nebula. (c) NASA, 2020. Retrieved from https://www.nasa.gov/mission_pages/chandra/images/helix-nebula.html"
            :style *style-img-dynamic-size-black-border*))
    (center-children container)))

(define-scene *glittering-nebula-low-thrusters*
  (:title "Glittering Nebula - Thrusters Low")
  (:on-enter
   (connection)
   (add-to-inventory connection "Helix Nebula" 'display-helix-nebula :height 302 :background-color "black")
   (with-edit-resources connection (sustenance morale)
     (incf morale)
     (decf sustenance)))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (div (:bind contents :hidden t :style *style-event-window-contents-div*)
            (p (:content "-1 sustenance / +1 morale."))
            (p (:content "Humming appreciation. Stardust swirls in distance. Eye shape with blue outline,
                          yellow sclera, deep blue iris and bright violet pupil."))
            (div (:bind image-container
                  :style *style-text-align-center*))
            (button (:bind continue-button
                     :content "Bask, and continue.")))
     (setf (height image-container)
           (* 0.3 (parse-float:parse-float (inner-height window))))
     (display-helix-nebula image-container)
     (set-on-click continue-button
                   (advance-to-random-event-function window))
     (setf (visiblep contents) t))))

(predeclare-scene *glittering-nebula-launch-shuttle*)
(predeclare-scene *glittering-nebula-abandon-facet*)

(define-scene *glittering-nebula-gazing-eva*
  (:title "Glittering Nebula - Gazing EVA")
  (:on-enter
   (connection)
   (with-edit-resources connection (sustenance)
     (decf sustenance 2)))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (div (:bind contents :hidden t :style *style-event-window-contents-div*)
            (p (:content "-2 sustenance."))
            (p (:content "Artistic facets don space suits. Gestalt places great focus on proper handling of
                          equipment. Arms through arms of thermal insulating layer, legs through legs. Zip
                          along torso. Pull straps tight. No, tighter than that. Yes, good. Step into lower
                          half of pressure shell. Suiting facet lifts arms, helper facet lowers upper
                          half. Ensure seam aligns. Engage seal. Place helmet. Ensure seam aligns. Engage
                          seal. Test pressurization. Four suits hold, one leaks. Helper facets inspect. Seam
                          between lower and upper suit portions improperly aligned. Unseal helmet,
                          remove. Unseal upper portion, lift, relocate, lower. Reseal. Replace helmet,
                          reseal. Test pressurization. All suits hold. Enter airlock. Cycle atmosphere. Exit
                          airlock."))
            ;; TODO: it would be cool to animate this part, but i don't know if i have time, honestly
            (p (:content "Airlock door opens slowly, gradually revealing incredible beauty. Stardust swirls in
                          distance. Eye shape with blue outline, yellow sclera, deep blue iris and bright
                          violet pupil."))
            (div (:bind image-container
                  :style *style-text-align-center*))
            (p (:content "Watcher facets exit airlock, spread along hull. Distracted; swept up in
                          nebula-viewing. Bright-eyed facet walks to stern. Gazes outward,
                          entranced. Stumbles. Boot magnets do not engage. Drifts. Bright-eyed facet doesn't
                          notice. Watcher facets don't notice. Gestalt doesn't notice. Rotational momentum
                          brings ship into view of bright-eyed facet. Slow realization, then panic."))
            (p (:content "Triangulate relative position of bright-eyed facet based on watcher facets'
                          perspectives. Calculate retrieval options."))
            (panel (:bind choices :display :flex :bottom 0)
                   (div (:style *style-event-option*)
                        (p (:content "Proposal: retrieve with shuttle."))
                        (p (:content "Concern: requires large volume of propellant to reach bright-eyed facet,
                                      then match velocity for pickup."))
                        (unordered-list ()
                                        (list-item (:content "-4 fuel")))
                        (button (:bind launch-shuttle-button
                                 :content "Launch shuttle.")))
                   (div (:style *style-event-option*)
                        (p (:content "Proposal: abandon bright-eyed facet."))
                        (p (:content "Concern: moral and morale implications."))
                        (unordered-list ()
                                        (list-item (:content "-4 morale")))
                        (button (:bind abandon-facet-button
                                 :content "With regret, abandon.")))))
     (setf (height image-container)
           (* 0.3 (parse-float:parse-float (inner-height window))))
     (display-helix-nebula image-container)
     (set-on-click launch-shuttle-button
                   (thunk (advance-to-scene *glittering-nebula-launch-shuttle* window)))
     (set-on-click abandon-facet-button
                   (thunk (advance-to-scene *glittering-nebula-abandon-facet* window)))
     (setf (visiblep contents) t))))

(define-scene *glittering-nebula-launch-shuttle*
  (:title "Glittering Nebula - Launch Shuttle")
  (:on-enter
   (connection)
   (with-edit-resources connection (fuel)
     (decf fuel 4)))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (div (:bind contents :hidden t :style *style-event-window-contents-div*)
            (p (:content "-4 fuel."))
            (p (:content "Fear; ship looks so distant. Launch shuttle. High thrust toward bright-eyed
                          facet. Hope; shuttle approaches, grows. Close distance. Decelerate to match
                          velocity. Delicate pulses with maneuvering thrusters to bring alongside. Confusion;
                          how will impotent tumbling facet board? Open airlock. Delicate pulses with
                          maneuvering thrusters to engulf stray facet. Realization, and fear again; imagined
                          image of facet burned by thruster fire, crushed by collision, pushed farther. Close
                          airlock around facet. Relief. Return shuttle to ship."))
            (button (:bind advance-button
                     :content "Celebrate retrieval, and continue.")))
     (set-on-click advance-button
                   (advance-to-random-event-function window))
     (setf (visiblep contents) t))))

(define-scene *glittering-nebula-abandon-facet*
  (:title "Glittering Nebula - Abandon Facet")
  (:on-enter
   (connection)
   (with-edit-resources connection (morale)
     (decf morale 4)))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (div (:bind contents :hidden t :style *style-event-window-contents-div*)
            (p (:content "-4 morale."))
            (p (:content "Fear; ship looks so distant. Sorrow; it will only grow
                          smaller. Shame. Regret. Guilt. Resignation. Sever connection."))
            (button (:bind advance-button
                     :content "Continue, less one.")))
     (set-on-click advance-button
                   (advance-to-random-event-function window))
     (setf (visiblep contents) t))))

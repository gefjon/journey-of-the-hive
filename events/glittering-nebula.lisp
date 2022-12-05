(uiop:define-package :journey-of-the-hive/events/glittering-nebula
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs :journey-of-the-hive/defscene :journey-of-the-hive/gui)
  (:local-nicknames (#:alex :alexandria))
  (:export #:*glittering-nebula*))
(in-package :journey-of-the-hive/events/glittering-nebula)

(predeclare-scene *glittering-nebula-low-thrusters*)
(predeclare-scene *glittering-nebula-gazing-eva*)
(predeclare-scene *glittering-nebula-ignore-and-continue*)

(define-event *glittering-nebula*
  (:title "Glittering Nebula")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents (:bind contents :hidden t)
                       (p (:content "Routine maintenance on ship. Thrusters down. Tall facet and long-armed
                                     facet don space suits. Exit airlock. Climb along hull. Inspection largely
                                     delegated to muscle memory. Distant visual of glittering nebula,
                                     ezperienced from two near-identical points of view. Minor differences in
                                     color receptors yield one image shifted toward blue, another heavier on
                                     red. Both beautiful. Snapshots saved. Enter airlock."))

                       (p (:content "Artistic facets express desire for better view, longer exposure to
                                     nebula."))

                       (panel (:bind choices :display :flex)
                              (event-option (window
                                             :proposal "set thrusters low to prolong exposure to nebula. Gaze
                                                        through viewports."
                                             :concern "increased duration of journey requires additional
                                                       sustinance."
                                             :effects '(:sustenance -1 :morale +1)
                                             :button-content "Thrusters low."
                                             :target-scene *glittering-nebula-low-thrusters*))
                              (event-option (window
                                             :proposal "offer artistic facets extra-vehicular jaunt. Attempt
                                                        to capture detailed visuals; create representations."
                                             :concern "artistic facets lack EVA training; risk of accident."
                                             :effects '(:sustenance -2 :morale +2)
                                             :button-content "Don space suits. Exit airlock. Stargaze."
                                             :target-scene *glittering-nebula-gazing-eva*))
                              (event-option (window
                                             :proposal "ignore. Mission parameters do not allow delay."
                                             :concern "single-minded focus on mission at expense of beauty and
                                                       pleasure risks morale among artistic facets."
                                             :effects '(:morale -1)
                                             :button-content "Thrusters up."
                                             :target-scene *glittering-nebula-ignore-and-continue*))))
     (center-children choices :horizontal t :vertical nil)
     (setf (visiblep contents) t))))

(define-image *nebula-image*
  :create-function create-helix-nebula
  :file-name "nasa_archives_helix"
  :nasa-title "Helix Nebula"
  :year 2020
  :source-url "https://www.nasa.gov/mission_pages/chandra/images/helix-nebula.html")

(define-scene *glittering-nebula-low-thrusters*
  (:title "Glittering Nebula - Thrusters Low")
  (:on-enter
   (connection)
   (add-to-inventory connection "Helix Nebula" 'create-helix-nebula :height 302 :background-color "black"))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "-1 sustenance / +1 morale."))
                       (p (:content "Humming appreciation. Stardust swirls in distance. Eye shape with blue
                                     outline, yellow sclera, deep blue iris and bright violet pupil."))
                       (helix-nebula (:height (* 0.3 (parse-float:parse-float (inner-height window)))))
                       (advance-button (window
                                        :content "Bask, and continue."))))))

(predeclare-scene *glittering-nebula-launch-shuttle*)
(predeclare-scene *glittering-nebula-abandon-facet*)

(define-scene *glittering-nebula-gazing-eva*
  (:title "Glittering Nebula - Gazing EVA")
  (:on-enter
   (connection)
   ;; counteract the morale benefit, because this doesn't go as planned
   (apply-effects connection :morale -2))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents (:bind contents)
                       (p (:content "-2 sustenance."))
                       (p (:content "Artistic facets don space suits. Gestalt places great focus on proper
                                     handling of equipment. Arms through arms of thermal insulating layer,
                                     legs through legs. Zip along torso. Pull straps tight. No, tighter than
                                     that. Yes, good. Step into lower half of pressure shell. Suiting facet
                                     lifts arms, helper facet lowers upper half. Ensure seam aligns. Engage
                                     seal. Place helmet. Ensure seam aligns. Engage seal. Test
                                     pressurization. Four suits hold, one leaks. Helper facets inspect. Seam
                                     between lower and upper suit portions improperly aligned. Unseal helmet,
                                     remove. Unseal upper portion, lift, relocate, lower. Reseal. Replace
                                     helmet, reseal. Test pressurization. All suits hold. Enter airlock. Cycle
                                     atmosphere. Exit airlock."))
                       ;; TODO: it would be cool to animate this part, but i don't know if i have time, honestly
                       (p (:content "Airlock door opens slowly, gradually revealing incredible
                                     beauty. Stardust swirls in distance. Eye shape with blue outline, yellow
                                     sclera, deep blue iris and bright violet pupil."))
                       (div (:bind image-container
                             :style *style-text-align-center*)
                            (helix-nebula ()))
                       (p (:content "Watcher facets exit airlock, spread along hull. Distracted; swept up in
                                     nebula-viewing. Bright-eyed facet walks to stern. Gazes outward,
                                     entranced. Stumbles. Boot magnets do not engage. Drifts. Bright-eyed
                                     facet doesn't notice. Watcher facets don't notice. Gestalt doesn't
                                     notice. Rotational momentum brings ship into view of bright-eyed
                                     facet. Slow realization, then panic."))
                       (p (:content "Triangulate relative position of bright-eyed facet based on watcher
                                     facets' perspectives. Calculate retrieval options."))
                       (panel (:bind choices :display :flex)
                              (event-option (window
                                             :proposal "retrieve with shuttle."
                                             :concern "requires large volume of propellant to reach
                                                       bright-eyed facet, then match velocity for pickup."
                                             :effects '(:fuel -4)
                                             :button-content "Launch shuttle."
                                             :target-scene *glittering-nebula-launch-shuttle*))
                              (event-option (window
                                             :proposal "abandon bright-eyed facet."
                                             :concern "moral and morale implications."
                                             :effects '(:morale -4)
                                             :button-content "With regret, abandon."
                                             :target-scene *glittering-nebula-abandon-facet*))))
     (setf (height image-container)
           (* 0.3 (parse-float:parse-float (inner-height window)))))))

(define-scene *glittering-nebula-launch-shuttle*
  (:title "Glittering Nebula - Launch Shuttle")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "-4 fuel."))
                       (p (:content "Fear; ship looks so distant. Launch shuttle. High thrust toward
                                     bright-eyed facet. Hope; shuttle approaches, grows. Close
                                     distance. Decelerate to match velocity. Delicate pulses with maneuvering
                                     thrusters to bring alongside. Confusion; how will impotent tumbling facet
                                     board? Open airlock. Delicate pulses with maneuvering thrusters to engulf
                                     stray facet. Realization, and fear again; imagined image of facet burned
                                     by thruster fire, crushed by collision, pushed farther. Close airlock
                                     around facet. Relief. Return shuttle to ship."))
                       (advance-button (window
                                        :content "Celebrate retrieval, and continue."))))))

(define-scene *glittering-nebula-abandon-facet*
  (:title "Glittering Nebula - Abandon Facet")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "-4 morale."))
                       (p (:content "Fear; ship looks so distant. Sorrow; it will only grow
                                     smaller. Shame. Regret. Guilt. Resignation. Sever connection."))
                       (advance-button (window
                                        :content "Continue, less one."))))))

(define-scene *glittering-nebula-ignore-and-continue*
  (:title "Glittering Nebula - Ignore and Continue")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "-1 morale."))
                       (p (:content "Humming disappointment. Swirling beauty fades away far too
                                     quickly. Resignation. Destination must take precedence over
                                     journey. Blackness of space. A new home becons."))
                       (advance-button (window
                                        :content "Journey onward."))))))

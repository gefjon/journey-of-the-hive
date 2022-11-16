(uiop:define-package :journey-of-the-hive/events
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs :journey-of-the-hive/defscene :journey-of-the-hive/gui)
  (:local-nicknames (#:alex :alexandria)))
(in-package :journey-of-the-hive/events)

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
     (declare (ignore low-thrusters gazing-eva ignore-and-continue))
     (center-children choices :horizontal t :vertical nil)
     (setf (visiblep contents) t))))

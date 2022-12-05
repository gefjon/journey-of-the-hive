(uiop:define-package :journey-of-the-hive/ending-scenes
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs :journey-of-the-hive/defscene :journey-of-the-hive/gui)
  (:local-nicknames (#:alex :alexandria)))
(in-package :journey-of-the-hive/ending-scenes)

(define-image *6-planets-artwork*
  :create-function create-6-planets-artwork
  :file-name "nasa_archives_6_planets"
  :nasa-title "K2-138 6 Planets Artwork (Artist's Illustration)"
  :year 2019
  :nasa-id "PIA23002"
  :source-url "https://images.nasa.gov/details-PIA23002")

(define-image *band-of-stars-image*
  :create-function create-band-of-stars-image
  :file-name "nasa_archives_band_of_stars"
  :nasa-title "Galaxy Evolution Explorer Spies Band of Stars"
  :year 2007
  :nasa-id "PIA09653"
  :source-url "https://images.nasa.gov/details-PIA09653")

(predeclare-scene *victory-planets-enter-view*)

(define-scene *victory*
  (:title "Arrival")
  (:on-enter
   (connection)
   (add-to-inventory connection "Band of Stars" 'create-band-of-stars-image :background-color "black" :height 483))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "Destination draws near. Yellow star grows in viewscreen. Once, twinkling
                                     spec among twinkling specs. Now, outshines its distant friends. Humming
                                     excitement."))
                       (band-of-stars-image (:height (* 0.3 (parse-float:parse-float (inner-height window)))))
                       (p (:content "Delicate burn to enter orbit coincidental with destination
                                     world. Patience challenged. Goal is so close, relatively, but still
                                     millions of kilometers distant. Not yet visible without
                                     magnification."))
                       (advance-button (window
                                        :target-scene *victory-planets-enter-view*
                                        :content "Wait."))))))

(predeclare-scene *victory-approach-destination*)

(define-scene *victory-planets-enter-view*
  (:title "Arrival - Planets Enter View")
  (:on-enter
   (connection)
   (add-to-inventory connection "6 Planets" 'create-6-planets-artwork :background-color "black" :height 256))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "Trace orbital path on readout. Star grows in viewscreen. Brighter spec
                                     becomes glowing orb. Crescent reflections from planets enter
                                     view. Destination world grows, becomes visible without magnification."))
                       (6-planets-artwork (:height (* 0.6 (parse-float:parse-float (inner-height window)))))
                       (advance-button (window
                                        :target-scene *victory-approach-destination*
                                        :content "Approach destination."))))))

(define-image *blue-planet-image*
  :create-function create-blue-planet-image
  :file-name "nasa_archives_blue_planet"
  :nasa-title "NASA Hubble Finds a True Blue Planet"
  :year 2017
  :nasa-id "GSFC_20171208_Archive_e001427"
  :source-url "https://images.nasa.gov/details-GSFC_20171208_Archive_e001427")

(predeclare-scene *victory-settle-new-world*)

(define-scene *victory-approach-destination*
  (:title "Arrival - Approach Destination")
  (:on-enter
   (connection)
   (add-to-inventory connection "Blue Planet" 'create-blue-planet-image :background-color "black" :height 315))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "Fall through space, nearer and nearer to new home. Grows on
                                     viewscreen. Sunlit crescent expands to semicircle, then gibbous, as
                                     orbital phases align. Blue surface takes on features; clouds swirl, ice
                                     formations loom."))
                       (blue-planet-image (:height (* 0.6 (parse-float:parse-float (inner-height window)))))
                       (p (:content "Finally, orbits align at libration point. Slight burn nudges to transfer
                                     orbit. Another nudges to stable orbit around new world."))
                       (advance-button (window
                                        :target-scene *victory-settle-new-world*
                                        :content "Descend to new home."))))))

(define-scene *victory-settle-new-world*
  (:title "Victory!")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents ()
                       (p (:content "My beloved children,"))
                       (p (:content "I am so proud of your success. You have arrived at a fresh, untouched
                                     world, and now it is time to build your home. Before you touch ground and
                                     begin construction, reflect on your values and your experiences, and ask
                                     yourselves:"))
                       (p (:content "How will we order our new society? Has the communalism of close-quarters
                                     space travel inspired us to live in a world of closeness, communication,
                                     deliberation and shared decision-making? Or does our new environment
                                     allow (or necessitate) a different ethos?"))
                       (p (:content "The end."))))))

(uiop:define-package :journey-of-the-hive/intro-scenes
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs :journey-of-the-hive/defscene :journey-of-the-hive/gui)
  (:local-nicknames (#:alex :alexandria)))
(in-package :journey-of-the-hive/intro-scenes)

(predeclare-scene *name-starship*)

;;; the first intro scene, which establishes stakes and handles character creation

(define-scene *intro-scene*
  (:title "Welcome!")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents (:bind contents)
                       (p (:content "My dear children,"))
                       (p (:content "You are about to embark on a journey through deep space. It will be a
                                     journey of exploration, of discovery, and, if all goes well, of
                                     creation. Your goal is to locate a pristine new world upon which to build
                                     a home. Along the way, you must learn coexistence and cooperation."))
                       (p (:content "Before you begin, introduce yourself to your comrades. Tell them your
                                     name, your pronouns, and the things you value on this journey. If you're
                                     having trouble thinking of values, consider:"))
                       (unordered-list ()
                                       (list-item (:content "Your relationship with nature, the inanimate, the
                                                             untouched, the pristine. Do you want to leave it
                                                             undisturbed and relish in its beauty? To study
                                                             and understand it? To guide and improve it? To
                                                             replace it with something better?"))
                                       (list-item (:content "Your relationship with strangers, foreigners and
                                                             aliens, either individually or as groups. Do you
                                                             want to avoid them? To coexist peacefully? Or
                                                             something more violent?  What do you think they
                                                             might have to offer you, and you to them? If you
                                                             have to choose between your own interests and
                                                             theirs, what will you do?"))
                                       (list-item (:content "Your relationship with technology, the created,
                                                             the unnatural. Is it a means to an end, or an end
                                                             in itself? Should it be embraced or rejected? Are
                                                             you comfortable entrusting your life to a
                                                             starship? Or would you rather keep your feet
                                                             planted on solid dirt, without worrying that an
                                                             air scrubber will break down and suffocate you?
                                                             Would you replace parts of your flesh with metal,
                                                             given the choice and the chance? How would you
                                                             react if someone else did?"))
                                       (list-item (:content "Your relationship with your comrades, your peers
                                                             and your friends. Do you long for hierarchy, and
                                                             if so, would you rather tell people what to do or
                                                             be told? How willing are you to accept a
                                                             difference of opinion? To defer to another for a
                                                             critical decision, or impose your own decision on
                                                             another? Would society be better off if each
                                                             individual was truly equal, perfectly
                                                             interchangeable and equivalent? Or deeply
                                                             specialized, with skills and attributes suited to
                                                             their role?")))
                       (p (:content "Your journey will be more interesting if differing, even conflicting,
                                     values exist among you. If your group is too like-minded, consider doing
                                     a bit of role-playing! Answer a few of the above questions, at least one
                                     from each category, in a way that differs from your own beliefs and those
                                     of your peers. Then, as you play, try to keep your behavior in line with
                                     those actions, especially when it makes things difficult or causes
                                     friction. Things will get boring pretty fast if you all agree all the
                                     time."))
                       (advance-button (window
                                        :content "We did it!"
                                        :target-scene *name-starship*))))))

;;; naming the starship, the players' first foray into collective decision-making

(predeclare-scene *introduce-gameplay*)

(define-scene *name-starship*
  (:title "Name your starship")
  (:on-enter
   (connection)
   (add-discussion-rules-button connection))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (event-contents (:bind contents :hidden t)
                       (p (:content "Once everyone has bared their psyche, there's one more thing to do before
                                     you get underway: you must name your starship. This will be your first
                                     decision as a group, so take it seriously and start off on the right
                                     foot! During your naming process, and each other decision you make, keep
                                     in mind the following rules:"))
                       (div (:bind rules-container))
                       (p (:content "All other guidance is intentionally elided. You must decide for
                                     yourselves how to balance speaking time so that everyone's voice is
                                     heard, how to cool things down when they get heated, how to convince each
                                     other what is right, when to allow yourself to be convinced, and when to
                                     stand in the way of a choice you cannot accept."))
                       (p (:content "With those rules in mind, each of you must propose a name for your
                                     starship, and the group must choose one of them."))
                       (form (:bind ship-name-form)
                             (form-element (:bind ship-name :text
                                             :label (create-label ship-name-form :content "Your starship's name: ")
                                             :value "the Hive"))
                             (br ())
                             (form-element (:submit :value "Christen!"))))
     (make-discussion-rules rules-container)
     (setf (visiblep contents) t)
     (set-on-submit ship-name-form
                    (thunk
                      (let* ((name (value ship-name))
                             (title (format nil "Journey of ~a" name)))
                        (setf (connection-starship-name window) name)
                        (setf (title (html-document (connection-body window))) title))
                      (advance-to-scene *introduce-gameplay* window)))
     contents)))

;;; the introduce-gameplay scene, which describes the event loop

(define-scene *introduce-gameplay*
  (:title "How to play")
  (:on-enter
   (connection)
   (add-resources-to-menu-bar connection)
   (add-event-rules-button connection))
  (:display-scene 
   (window)
   (with-clog-create (window-content window)
       (event-contents (:bind contents :hidden t)
                       (p (:content (format nil
                                            "Congratulations, crew of ~a! As you travel, check Status to keep
                                             track of your levels of fuel, sustenance and morale. Your journey
                                             will unfold as a series of rounds, each of which will take the
                                             following form:"
                                            (connection-starship-name window))))
                       (div (:bind event-rules))
                       (p (:content "Good luck and safe travels!"))
                       (advance-button (window
                                        :content "Embark!")))
     (make-event-rules event-rules)
     (setf (visiblep contents) t))))

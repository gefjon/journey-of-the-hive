(uiop:define-package #:journey-of-the-hive
  (:use #:cl #:clog #:clog-gui)
  (:local-nicknames (#:alex :alexandria))
  (:export start-app))

(in-package :journey-of-the-hive)

(defmacro thunk (&body body)
  (alex:with-gensyms (args)
    `(lambda (&rest ,args)
       (declare (ignore ,args))
       ,@body)))

(defun show-in-popup (clog-obj title make-contents &key (width 400) (height 200))
  (let* ((body (connection-body clog-obj))
         (popup (create-gui-window body
                                   :title title
                                   :hidden t
                                   :keep-on-top t
                                   :width width
                                   :height height))
         (popup-contents (window-content popup)))
    (window-center popup)
    (funcall make-contents popup-contents)
    (set-on-window-can-size popup (constantly nil))
    (setf (visiblep popup) t)
    popup))

(defparameter *main-window-outer-margin* 20)

(defun make-scene-window (body title)
  (let* ((menu-bar (find-menu-bar body))
         (menu-bar-height (parse-float:parse-float (outer-height menu-bar)))
         (container (window body))
         (container-width (inner-width container))
         (container-height (inner-height container))
         (my-width (- container-width (* *main-window-outer-margin* 2)))
         (my-height (- container-height (+ menu-bar-height (* *main-window-outer-margin* 2))))
         (main-window (create-gui-window body
                                         :hidden nil
                                         :width my-width
                                         :height my-height
                                         :hidden nil
                                         :title title)))
    (setf (top main-window) (+ menu-bar-height *main-window-outer-margin*))
    (setf (left main-window) *main-window-outer-margin*)
    (setf (visiblep main-window) t)
    main-window))

(defun find-main-window-contents-panel (body)
  (or (connection-data-item body "main-window-contents-panel")
      (make-main-window-contents-panel body)))

(defun find-main-window-and-contents-panel (body)
  (values (find-main-window body)
          (find-main-window-contents-panel body)))

(defun replace-main-window-contents-panel (body)
  (alex:when-let ((contents-panel (connection-data-item body "main-window-contents-panel")))
    (destroy contents-panel))
  (make-main-window-contents-panel body))

(defvar *events* '())

(defclass game-data ()
  ((starship-name :type string
                  :accessor game-data-starship-name)
   (fuel :type (and fixnum unsigned-byte)
         :accessor game-data-fuel
         :initarg :fuel)
   (sustenance :type (and fixnum unsigned-byte)
               :accessor game-data-sustenance
               :initarg :sustenance)
   (morale :type (and fixnum unsigned-byte)
           :accessor game-data-morale
           :initarg :morale)
   (current-scene :type scene
                  :accessor game-data-current-scene
                  :initarg :current-scene)
   (available-events :type list
                     :accessor game-data-available-events
                     :initarg :available-events))
  (:default-initargs :fuel 16 :sustenance 16 :morale 16 :current-scene *intro-scene* :available-events (alex:shuffle (copy-list *events*))))

(defun get-game-data (connection)
  (connection-data-item connection "game-data"))

(defun init-game-data (connection)
  (setf (connection-data-item connection "game-data")
        (make-instance 'game-data)))

(defun connection-starship-name (connection)
  (game-data-starship-name (get-game-data connection)))

(defun (setf connection-starship-name) (new-name connection)
  (setf (game-data-starship-name (get-game-data connection)) new-name))

(defun connection-fuel (connection)
  (game-data-fuel (get-game-data connection)))

(defun (setf connection-fuel) (new-fuel connection)
  (setf (game-data-fuel (get-game-data connection)) new-fuel))

(defun connection-sustenance (connection)
  (game-data-sustenance (get-game-data connection)))

(defun (setf connection-sustenance) (new-sustenance connection)
  (setf (game-data-sustenance (get-game-data connection)) new-sustenance))

(defun connection-morale (connection)
  (game-data-morale (get-game-data connection)))

(defun (setf connection-morale) (new-morale connection)
  (setf (game-data-morale (get-game-data connection)) new-morale))

(defun connection-current-scene (connection)
  (game-data-current-scene (get-game-data connection)))

(defun (setf connection-current-scene) (new-scene connection)
  (setf (game-data-current-scene (get-game-data connection)) new-scene))

(defun connection-available-events (connection)
  (game-data-available-events (get-game-data connection)))

(defun (setf connection-available-events) (new-list connection)
  (setf (game-data-available-events (get-game-data connection)) new-list))

(defun pop-random-event (connection)
  (or (cdr (pop (connection-available-events connection)))
      (error "still to write: final scene")))

(defun call-with-edit-resources (connection thunk)
  (funcall thunk (get-game-data connection))
  (update-resource-displays connection))

(defmacro with-edit-resources (connection (&rest game-data-slots) &body body)
  (alex:with-gensyms (game-data)
    `(call-with-edit-resources ,connection
                               (lambda (,game-data)
                                 (with-slots ,game-data-slots ,game-data
                                   ,@body)))))

(defun make-menu-bar (connection)
  (with-clog-create (connection-body connection)
      (gui-menu-bar (:bind menu-bar)
                    (gui-menu-full-screen ())
                    (gui-menu-item (:content "About"
                                    :on-click 'on-about))
                    (gui-menu-item (:content "Resume"
                                    :on-click 'redisplay-current-scene))
                    (gui-menu-drop-down (:content "Debug")
                                        (gui-menu-item (:content "+1 fuel"
                                                        :on-click (thunk (with-edit-resources connection
                                                                             (fuel)
                                                                           (incf fuel)))))
                                        (gui-menu-item (:content "-1 fuel"
                                                        :on-click (thunk (with-edit-resources connection
                                                                             (fuel)
                                                                           (decf fuel)))))))
    (setf (connection-data-item connection "menu-bar") menu-bar)
    menu-bar))

(defclass scene ()
  ((%title :type string
           :initarg :title
           :accessor scene-title)
   (%on-enter :type (function (clog-body) *)
              :initarg :on-enter
              :accessor scene-on-enter)
   (%display-scene :type (function (clog-gui-window) *)
                   :initarg :display-scene
                   :accessor scene-display-scene)))

(defun display-scene (scene window)
  (when (slot-boundp scene '%display-scene)
    (funcall (scene-display-scene scene) window)))

(defun display-scene-in-new-window (scene connection)
  (let* ((window (make-scene-window (connection-body connection)
                                    (scene-title scene))))
    (display-scene scene window)))

(defun enter-scene (scene connection)
  (when (slot-boundp scene '%on-enter)
    (funcall (scene-on-enter scene) (connection-body connection))))

(defun run-scene (scene connection)
  (enter-scene scene connection)
  (setf (connection-current-scene connection) scene)
  (display-scene-in-new-window scene connection))

(defun advance-to-scene (new-scene existing-window)
  (window-close existing-window)
  (run-scene new-scene existing-window)
  (destroy existing-window))

(defmacro named-lambda (name arglist &body body)
  `(labels ((,name ,arglist ,@body))
     #',name))

(eval-when (:compile-toplevel :load-toplevel)
  (defun parse-scene-clauses (name-symbol clauses)
    "returns (values TITLE ON-ENTER DISPLAY-SCENE) as nullable forms"
    (let* ((title (or (second (assoc :title clauses :test 'eq))
                      "Unititled scene"))
           (on-enter (alex:when-let ((clause (assoc :on-enter clauses :test 'eq)))
                       (destructuring-bind ((connection-var) &body body) (rest clause)
                         `(named-lambda ,(make-symbol (format nil "ON-ENTER-~A" name-symbol))
                              (,connection-var)
                            ,@body))))
           (display-scene (alex:when-let ((clause (assoc :display-scene clauses :test 'eq)))
                            (destructuring-bind ((window-var) &body body) (rest clause)
                              `(named-lambda ,(make-symbol (format nil "DISPLAY-SCENE-~A" name-symbol))
                                   (,window-var)
                                 ,@body)))))
      (check-type title string)
      (values title on-enter display-scene)))

  (defun make-scene-form (title on-enter display-scene)
    `(make-instance 'scene
                    :title ,title
                    ,@(when on-enter `(:on-enter ,on-enter))
                    ,@(when display-scene `(:display-scene ,display-scene)))))

(defmacro define-scene (special-var-name &body clauses)
  "Define a `scene' object named SPECIAL-VAR-NAME.

Each of the CLAUSES should take one of the forms:

(:title STRING), where STRING is a string literal to be used as a title for the scene.

(:on-enter (CONNECTION) BODY...) where CONNECTION is a symbol to be bound to a CLOG connection object while
                                 executing the BODY forms. BODY will be evaluated once upon entering the
                                 scene.

(:display-scene (GUI-WINDOW) BODY...) where GUI-WINDOW is a symbol to be bound to a clog-gui-window object
                                      while executing the BODY forms. BODY will be evaluated with a fresh
                                      clog-gui-window whenever the scene needs to be displayed."
  
  (multiple-value-bind (title on-enter display-scene)
      (parse-scene-clauses special-var-name clauses)
    `(defparameter ,special-var-name
       ,(make-scene-form title on-enter display-scene))))

(defmacro define-event (scene-name &body clauses)
  (multiple-value-bind (title on-enter display-scene)
      (parse-scene-clauses scene-name clauses)
    `(progn (setf *events* (remove ',scene-name *events* :key 'car))
            (push (cons ',scene-name ,(make-scene-form title on-enter display-scene))
                  *events*))))

(defun redisplay-current-scene (connection)
  (let* ((current-scene (connection-current-scene connection)))
    (maphash (lambda (key value)
               (print key)
               (print value)
               (print (window-title value)))
             (window-collection connection))
    (print (scene-title current-scene))
    
    (print (window-collection connection))
    (or (print (window-to-top-by-title connection (scene-title current-scene)))
        (display-scene-in-new-window current-scene connection))))

(defun find-menu-bar (connection)
  (or (connection-data-item connection "menu-bar")
      (make-menu-bar connection)))

(defun make-rules-dropdown (connection)
  (let* ((menu-bar (find-menu-bar connection))
         (dropdown (create-gui-menu-drop-down menu-bar :content "Rules")))
    (setf (connection-data-item connection "rules-dropdown") dropdown)
    dropdown))

(defun find-rules-dropdown (connection)
  (or (connection-data-item connection "rules-dropdown")
      (make-rules-dropdown connection)))

(defun make-hidden (obj)
  (setf (visiblep obj) nil))

(defun make-shown (obj)
  (setf (visiblep obj) t))

(defparameter *style-text-align-center* "text-align:center;")

(defun on-about (obj)
  (or (window-to-top-by-title obj "About")
      (show-in-popup obj
                     "About"
                     (lambda (popup)
                       (with-clog-create popup
                           (div ()
                                (div (:class "w3-black"
                                      :style *style-text-align-center*)
                                     (img (:url-src "/img/clogwicon.png"))
                                     (p ()
                                        (span (:content "Journey of the Hive"))
                                        (br ())
                                        (span (:content "(c) 2022 - Phoebe Goldman"))))
                                (p (:style "text-align:center;"
                                    :content "Journey of the Hive is a game for 4-ish players about democracy,
                                          cooperation, and space bugs."))))))))

(define-scene *intro-scene*
  (:title "Welcome!")
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (div (:bind contents :hidden t)
            (p (:content "My dear children,"))
            (p (:content "You are about to embark on a journey through deep space. It will be a journey of
                          exploration, of discovery, and, if all goes well, of creation. Your goal is to locate a
                          pristine new world upon which to build a home. Along the way, you must learn coexistence
                          and cooperation."))
            (p (:content "Before you begin, introduce yourself to your comrades. Tell them your name, your pronouns,
                          and the things you value on this journey. If you're having trouble thinking of values,
                          consider:"))
            (unordered-list ()
              (list-item (:content "Your relationship with nature, the inanimate, the untouched, the
                                    pristine. Do you want to leave it undisturbed and relish in its beauty?
                                    To study and understand it? To guide and improve it? To replace it with
                                    something better?"))
              (list-item (:content "Your relationship with strangers, foreigners and aliens, either
                                    individually or as groups. Do you want to avoid them? To coexist
                                    peacefully? Or something more violent?  What do you think they might have
                                    to offer you, and you to them? If you have to choose between your own
                                    interests and theirs, what will you do?"))
              (list-item (:content "Your relationship with technology, the created, the unnatural. Is it a
                                    means to an end, or an end in itself? Should it be embraced or rejected?
                                    Are you comfortable entrusting your life to a starship? Or would you
                                    rather keep your feet planted on solid dirt, without worrying that an air
                                    scrubber will break down and suffocate you?  Would you replace parts of
                                    your flesh with metal, given the choice and the chance? How would you
                                    react if someone else did?"))
              (list-item (:content "Your relationship with your comrades, your peers and your friends. Do you
                                    long for hierarchy, and if so, would you rather tell people what to do or
                                    be told? How willing are you to accept a difference of opinion? To defer
                                    to another for a critical decision, or impose your own decision on
                                    another? Would society be better off if each individual was truly equal,
                                    perfectly interchangeable and equivalent? Or deeply specialized, with
                                    skills and attributes suited to their role?")))
            (p (:content "Your journey will be more interesting if differing, even conflicting, values exist among
                          you. If your group is too like-minded, consider doing a bit of role-playing! Answer a few
                          of the above questions, at least one from each category, in a way that differs from your
                          own beliefs and those of your peers. Then, as you play, try to keep your behavior in line
                          with those actions, especially when it makes things difficult or causes friction. Things
                          will get boring pretty fast if you all agree all the time."))
            (button (:bind continue-button :content "We did it!")))
     (set-on-click continue-button (thunk (advance-to-scene *name-starship* window)))
     (setf (visiblep contents) t))))

(defun make-discussion-rules (parent)
  (with-clog-create parent
      (unordered-list (:bind contents)
        (list-item (:content "Any thought that pops into your head, you must express, however briefly."))
        (list-item (:content "In order to proceed, everyone must agree. When you feel ready to move on, hold a
                              vote. If the results are not unanimous, continue discussing until you're ready
                              to try again.")))))


(defun show-discussion-rules-in-popup (connection)
  (or (window-to-top-by-title connection "Rules - Discussion")
      (show-in-popup connection "Rules - Discussion" 'make-discussion-rules :height 250)))

(defun add-discussion-rules-button (connection)
  (let* ((dropdown (find-rules-dropdown connection)))
    (create-gui-menu-item dropdown
                          :content "Discussion"
                          :on-click 'show-discussion-rules-in-popup)))

(defun starship-name (connection)
  (starship-name (connection-data-item connection "game-data")))

(define-scene *name-starship*
  (:title "Name your starship")
  (:on-enter
   (connection)
   (add-discussion-rules-button connection))
  (:display-scene
   (window)
   (with-clog-create (window-content window)
       (div (:bind contents :hidden t)
            (p (:content "Once everyone has bared their psyche, there's one more thing to do before you get
                         underway: you must name your starship. This will be your first decision as a group,
                         so take it seriously and start off on the right foot! During your naming process, and
                         each other decision you make, keep in mind the following rules:"))
            (div (:bind rules-container))
            (p (:content "All other guidance is intentionally elided. You must decide for yourselves how to
                         balance speaking time so that everyone's voice is heard, how to cool things down when
                         they get heated, how to convince each other what is right, when to allow yourself to
                         be convinced, and when to stand in the way of a choice you cannot accept."))
            (p (:content "With those rules in mind, each of you must propose a name for your starship, and the
                         group must choose one of them."))
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

(defun show-resources (container)
  (with-clog-create container
      (panel (:style "margin-left:10px;")
           (p ()
              (span (:content (connection-fuel container)))
              (span (:content " fuel")))
           (p ()
              (span (:content (connection-sustenance container)))
              (span (:content " sustenance")))
           (p ()
              (span (:content (connection-morale container)))
              (span (:content " morale"))))))

(defun show-resources-in-popup (connection)
  (or (window-to-top-by-title connection "Status")
      (let* ((popup (show-in-popup connection
                                   "Status"
                                   (constantly nil)
                                   :width 140
                                   :height 160)))
        (update-resource-displays connection popup))))

(defun update-resource-displays (connection &optional (status-page (window-by-title connection "Status")))
  (when status-page
    (let* ((contents-panel (window-content status-page)))
      (destroy (first-child contents-panel))
      (show-resources contents-panel))))

(defun add-resources-to-menu-bar (connection)
  (create-gui-menu-item (find-menu-bar connection)
                        :content "Status"
                        :on-click (thunk (show-resources-in-popup connection))))

(defun make-event-rules (parent)
  (with-clog-create parent
      (unordered-list ()
        (list-item (:content "You will be presented with an event, in the form of some narrative text. Read it aloud."))
        (list-item (:content "You will be offered a few responses, each of which will have a short
                              description. Some of these will also list costs or rewards of fuel, sustenance
                              and/or morale. Read them aloud."))
        (list-item (:content "Discuss as a group, keeping in mind the above rules and your personal values,
                              and choose one of the responses to take. Your chosen response may also have
                              hidden effects, which will be unknown to you until after you choose.")))))

(defun show-event-rules-in-popup (connection)
  (or (window-to-top-by-title connection "Rules - Events")
      (show-in-popup connection
                     "Rules - Events"
                     'make-event-rules
                     :width 400
                     :height 330)))

(defun add-event-rules-button (connection)
  (let* ((dropdown (find-rules-dropdown connection)))
    (create-gui-menu-item dropdown :content "Events" :on-click 'show-event-rules-in-popup)))

(define-scene *introduce-gameplay*
  (:title "How to play")
  (:on-enter
   (connection)
   (add-resources-to-menu-bar connection)
   (add-event-rules-button connection))
  (:display-scene 
   (window)
   (with-clog-create (window-content window)
       (panel (:bind contents :hidden t)
              (p (:content (format nil
                                   "Congratulations, crew of ~a! As you travel, check Status to keep track of
                                    your levels of fuel, sustenance and morale. Your journey will unfold as a
                                    series of rounds, each of which will take the following form:"
                                   (connection-starship-name window))))
              (div (:bind event-rules))
              (p (:content "Good luck and safe travels!"))
              (button (:bind continue-button :content "Embark!")))
     (make-event-rules event-rules)
     (set-on-click continue-button
                   (thunk (advance-to-scene (pop-random-event window)
                                            window)))
     (setf (visiblep contents) t))))

(defparameter *style-event-option* "margin:10px;padding:10px;border-style:solid;border-color:black;")

(define-event glittering-nebula
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

(defun on-new-window (connection)
  (setf (title (html-document connection)) "Journey of the Hive")
  (clog-gui-initialize connection)
  (init-game-data connection)
  (add-class connection "w3-teal")
  (make-menu-bar connection)
  (run-scene *intro-scene* connection))

(defun start-app ()
  (initialize 'on-new-window
   :static-root (merge-pathnames "./www/"
                  (asdf:system-source-directory :journey-of-the-hive)))
  (open-browser))

(uiop:define-package :journey-of-the-hive/gui
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs)
  (:local-nicknames (#:alex :alexandria))
  (:export
   #:advance-to-scene
   #:redisplay-current-scene

   #:make-menu-bar #:find-menu-bar

   #:add-discussion-rules-button
   #:make-discussion-rules

   #:add-resources-to-menu-bar
   #:with-edit-resources

   #:add-event-rules-button
   #:make-event-rules

   #:*style-text-align-center*
   #:*style-event-option*))
(in-package :journey-of-the-hive/gui)

;;;; macro definitions
;; early for use in this file

(defmacro with-edit-resources (connection (&rest game-data-slots) &body body)
  (alex:with-gensyms (game-data)
    `(call-with-edit-resources ,connection
                               (lambda (,game-data)
                                 (with-slots ,game-data-slots ,game-data
                                   ,@body)))))

;;;; style defs

(defparameter *style-text-align-center* "text-align:center;")
(defparameter *style-event-option* "margin:10px;padding:10px;border-style:solid;border-color:black;")

;;;; the menu bar

(defun find-menu-bar (connection)
  (or (connection-data-item connection "menu-bar")
      (make-menu-bar connection)))

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

;;; the rules dropdown, which is added dynamically and offers game rules popups

(defun make-rules-dropdown (connection)
  (let* ((menu-bar (find-menu-bar connection))
         (dropdown (create-gui-menu-drop-down menu-bar :content "Rules")))
    (setf (connection-data-item connection "rules-dropdown") dropdown)
    dropdown))

(defun find-rules-dropdown (connection)
  (or (connection-data-item connection "rules-dropdown")
      (make-rules-dropdown connection)))

;;;; popup windows

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

;;; the status window, for displaying resources

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

(defun call-with-edit-resources (connection thunk)
  (funcall thunk (get-game-data connection))
  (update-resource-displays connection))

(defun add-resources-to-menu-bar (connection)
  (create-gui-menu-item (find-menu-bar connection)
                        :content "Status"
                        :on-click (thunk (show-resources-in-popup connection))))

;;; the about popup, for general information about the game

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

;;; the discussion rules window, for guidance on how to collaboratively make decisions

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

;;; the event rules window, for guidance on how the gameplay works

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

;;;; displaying main scenes

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

(defun display-scene-in-new-window (scene connection)
  (let* ((window (make-scene-window (connection-body connection)
                                    (scene-title scene))))
    (display-scene scene window)))

(defun run-scene (scene connection)
  (enter-scene scene connection)
  (setf (connection-current-scene connection) scene)
  (display-scene-in-new-window scene connection))

(defun advance-to-scene (new-scene existing-window)
  (let* ((new-scene (etypecase new-scene
                      (symbol (symbol-value new-scene))
                      (scene new-scene))))
    (check-type new-scene scene)
    (window-close existing-window)
    (run-scene new-scene existing-window)
    (destroy existing-window)))

(defun redisplay-current-scene (connection)
  (let* ((current-scene (connection-current-scene connection)))
    (maphash (lambda (key value)
               (print key)
               (print value)
               (print (window-title value)))
             (window-collection connection))
    (or (window-to-top-by-title connection (scene-title current-scene))
        (display-scene-in-new-window current-scene connection))))

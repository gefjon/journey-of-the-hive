(uiop:define-package :journey-of-the-hive/gui
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs)
  (:local-nicknames (#:alex :alexandria))
  (:export
   #:advance-to-scene
   #:redisplay-current-scene
   #:advance-to-random-event-function

   #:make-menu-bar #:find-menu-bar

   #:define-image

   #:add-to-inventory

   #:add-discussion-rules-button
   #:make-discussion-rules

   #:add-resources-to-menu-bar
   #:with-edit-resources
   #:apply-effects
   #:fuel #:sustenance #:morale

   #:add-event-rules-button
   #:make-event-rules

   #:*style-text-align-center*
   #:*style-event-option*
   #:*style-img-dynamic-size-black-border*
   #:*style-event-window-contents-div*

   #:create-advance-button
   #:create-event-contents
   #:create-effects-list
   #:create-event-option
   #:create-image))
(in-package :journey-of-the-hive/gui)

;;;; macro definitions
;; early for use in this file

(defmacro with-edit-resources (connection (&rest game-data-slots) &body body)
  (alex:with-gensyms (game-data)
    `(call-with-edit-resources ,connection
                               (lambda (,game-data)
                                 (with-slots ,game-data-slots ,game-data
                                   ,@body)))))

(defmacro define-image (variable-name &key create-function file-name (extension "jpg") year nasa-title nasa-id source-url)
  `(progn
     (defvar ,variable-name
       ,(format nil "/img/~a.~a" file-name extension)
       ,(format nil "~@[\"~a\"~%~]Copyright NASA~@[, ~d~].~@[ ID ~a.~]~@[ From ~a~]"
                nasa-title year nasa-id source-url))
     ,@(when create-function
         `((defun ,create-function (parent &key height)
             (create-image parent
                           ,variable-name 
                           (documentation ',variable-name 'variable)
                           :height height))))))

;;;; style defs

(defparameter *style-text-align-center* "text-align:center;")
(defparameter *style-event-option*
  "margin:10px;padding:10px;border-style:solid;border-color:black;")
(defparameter *style-img-dynamic-size-black-border*
  "max-width:100%;max-height:100%;border-style:solid;border-color:black;")
(defparameter *style-event-window-contents-div*
  "margin:10px;padding:10px;")

;;;; the menu bar

(defun find-menu-bar (connection)
  (or (connection-data-item connection "menu-bar")
      (make-menu-bar connection)))

(defun create-debug-menu (menu-bar)
  (let* ((drop-down (create-gui-menu-drop-down menu-bar
                                               :content "Debug")))
    (create-gui-menu-item drop-down
                          :content "Edit resources"
                          :on-click 'show-debug-edit-resources-in-popup)
    (create-gui-menu-item drop-down
                          :content "Jump to scenes"
                          :on-click 'show-debug-jump-to-scene-in-popup)
    drop-down))

(defun make-menu-bar (connection)
  (with-clog-create (connection-body connection)
      (gui-menu-bar (:bind menu-bar)
                    (gui-menu-full-screen ())
                    (gui-menu-item (:content "About"
                                    :on-click 'on-about))
                    (gui-menu-item (:content "Resume"
                                    :on-click 'redisplay-current-scene))
                    (debug-menu ()))
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

;;; the inventory, which is added dynamically and offers popups to review images and stuff

(defun make-inventory-dropdown (connection)
  (let* ((menu-bar (find-menu-bar connection))
         (dropdown (create-gui-menu-drop-down menu-bar :content "Memories")))
    (setf (connection-data-item connection "inventory-dropdown") dropdown)
    dropdown))

(defun find-inventory-dropdown (connection)
  (or (connection-data-item connection "inventory-dropdown")
      (make-inventory-dropdown connection)))

(defun add-to-inventory (connection title display-function &rest kwargs &key (width 400) (height 400) background-color)
  (declare (ignore width height background-color))
  (flet ((show-inventory-item-in-popup (connection)
           (apply #'show-in-popup connection title display-function kwargs)))
    (let* ((dropdown (find-inventory-dropdown connection)))
      (create-gui-menu-item dropdown :on-click #'show-inventory-item-in-popup :content title))))

;;;; popup windows

(defun show-in-popup (clog-obj title make-contents &key (width 400) (height 200) (background-color nil background-color-p))
  (let* ((body (connection-body clog-obj))
         (popup (create-gui-window body
                                   :title title
                                   :hidden t
                                   :keep-on-top t
                                   :width width
                                   :height height))
         (popup-contents (window-content popup)))
    (when (and background-color-p background-color)
      (setf (background-color popup-contents) background-color))
    (window-center popup)
    (funcall make-contents popup-contents)
    (set-on-window-can-size popup (constantly nil))
    (setf (visiblep popup) t)
    popup))

;;; debug popups

(defun create-edit-resource-list-item (ul keyword)
  (flet ((add-one (connection)
           (apply-effects connection keyword 1))
         (sub-one (connection)
           (apply-effects connection keyword -1)))
    (with-clog-create ul
        (list-item ()
                   (span (:content (string-capitalize keyword)))
                   (button (:bind +1-button
                             :content "+1"))
                   (button (:bind -1-button
                             :content "-1")))
      (set-on-click +1-button #'add-one)
      (set-on-click -1-button #'sub-one))))

(defun create-debug-edit-resources (parent)
  (let* ((ul (create-unordered-list parent)))
    (loop :for keyword :in '(:fuel :sustenance :morale :fuel-capacity :sustenance-capacity)
          :do (create-edit-resource-list-item ul keyword))))

(defun show-debug-edit-resources-in-popup (connection)
  (show-in-popup connection
                 "Debug - Edit Resources"
                 'create-debug-edit-resources))

(defun create-jump-to-scene-button (parent scene-name &aux (scene (symbol-value scene-name)))
  (flet ((go-to-scene (connection)
           (run-scene scene connection)))
    (with-clog-create parent
        (button (:bind go-button
                  :content (scene-name scene)))
      (set-on-click go-button #'go-to-scene))))

(defun create-debug-jump-to-scene (parent)
  (loop :for scene-name :in *all-scenes*
        :do (create-jump-to-scene-button parent scene-name)))

(defun show-debug-jump-to-scene-in-popup (connection)
  (show-in-popup connection
                 "Debug - Jump to Scene"
                 'create-debug-jump-to-scene))

;;; the status window, for displaying resources

(defun show-resources (container)
  (with-clog-create container
      (panel (:style "margin-left:10px;")
           (p (:content (format nil
                                "~d / ~d fuel"
                                (connection-fuel container)
                                (connection-fuel-capacity container))))
           (p (:content (format nil
                                "~d / ~d sustenance"
                                (connection-sustenance container)
                                (connection-sustenance-capacity container))))
           (p (:content (format nil
                                "~d morale"
                                (connection-morale container)))))))

(defun show-resources-in-popup (connection)
  (or (window-to-top-by-title connection "Status")
      (let* ((popup (show-in-popup connection
                                   "Status"
                                   (constantly nil)
                                   :width 180
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
                                     (p ()
                                        (span (:content "Journey of the Hive"))
                                        (br ())
                                        (span (:content "(c) 2022 - Phoebe Goldman"))
                                        (br ())
                                        (span (:content "Space images courtesy of and (c) NASA"))))
                                (p (:style "text-align:center;"
                                    :content "Journey of the Hive is a game for 4-ish players about democracy,
                                          cooperation, and space bugs.")))))
                     :height 200)))

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
(defparameter *main-window-width* 0.6)

(defun make-scene-window (body title)
  (let* ((menu-bar (find-menu-bar body))
         (menu-bar-height (parse-float:parse-float (outer-height menu-bar)))
         (container (window body))
         (container-width (inner-width container))
         (container-height (inner-height container))
         (my-width (- (* container-width *main-window-width*)
                      (* *main-window-outer-margin* 2)))
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

(defun advance-to-random-event-function (existing-window)
  (thunk (advance-to-scene (pop-random-event existing-window)
                           existing-window)))

;;; element builders

(defun apply-one-effect-without-updating-gui (connection delta resource-name &optional capacity-name)
  (let* ((old-value (game-data-slot-value connection resource-name))
         (new-value (+ old-value delta)))
    (if (<= new-value 0)
        (error 'out-of-resource :resource-name resource-name)
        (setf (game-data-slot-value connection resource-name)
              (if capacity-name
                  (let* ((capacity (game-data-slot-value connection capacity-name)))
                    (min new-value capacity))
                  new-value)))))

(defun apply-effects (connection &key (fuel 0) (sustenance 0) (morale 0) (fuel-capacity 0) (sustenance-capacity 0))
  (with-edit-resources connection ()
    (apply-one-effect-without-updating-gui connection fuel-capacity 'fuel-capacity)
    (apply-one-effect-without-updating-gui connection fuel 'fuel 'fuel-capacity)
    (apply-one-effect-without-updating-gui connection sustenance-capacity 'sustenance-capacity)
    (apply-one-effect-without-updating-gui connection sustenance 'sustenance 'sustenance-capacity)
    (apply-one-effect-without-updating-gui connection morale 'morale)))

(defun create-advance-button (parent window
                              &key (content "Continue.")
                                (target-scene :random)
                                effects
                                before-continuing)
  (let* ((button (create-button parent :content content)))
    (flet ((advance-button-callback (button)
             (apply 'apply-effects button effects)
             (when before-continuing
               (funcall before-continuing button))
             (if (eq target-scene :random)
                 (advance-to-scene (pop-random-event window)
                                   window)
                 (advance-to-scene target-scene window))))
      (set-on-click button
                    #'advance-button-callback
                    :one-time t))))

(defun create-event-contents (parent &key hidden)
  (create-div parent :hidden hidden :style *style-event-window-contents-div*))

(defun create-effects-list (parent
                            &key fuel sustenance morale
                              fuel-capacity sustenance-capacity)
  (when (or fuel sustenance morale fuel-capacity sustenance-capacity)
    (let* ((ul (create-unordered-list parent)))
      (when fuel
        (create-list-item ul :content (format nil "~@d fuel" fuel)))
      (when fuel-capacity
        (create-list-item ul :content (format nil "~@d fuel capacity" fuel-capacity)))
      (when sustenance
        (create-list-item ul :content (format nil "~@d sustenance" sustenance)))
      (when sustenance-capacity
        (create-list-item ul :content (format nil "~@d sustenance capacity" sustenance-capacity)))
      (when morale
        (create-list-item ul :content (format nil "~@d morale" morale))))))

(defun create-event-option (parent window
                            &key proposal
                              concern
                              effects
                              button-content
                              (target-scene :random)
                              before-continuing)
  (let* ((div (create-div parent :style *style-event-option*)))
    (when proposal
      (create-p div :content (concatenate 'string "Proposal: " proposal)))
    (when concern
      (create-p div :content (concatenate 'string "Concern: " concern)))
    (when effects
      (apply #'create-effects-list div effects))
    (create-advance-button div window
                           :content button-content
                           :target-scene target-scene
                           :before-continuing before-continuing
                           :effects effects)
    div))

(defun create-image (parent path alt-text &key height)
  (let* ((div (create-div parent :style *style-text-align-center*)))
    (when height
      (setf (height div) height))
    (create-img div
                :url-src path
                :alt-text alt-text
                :style *style-img-dynamic-size-black-border*)
    div))

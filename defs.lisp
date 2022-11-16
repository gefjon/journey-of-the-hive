(uiop:define-package :journey-of-the-hive/defs
  (:use :cl :clog :clog-gui)
  (:local-nicknames (#:alex :alexandria))
  (:export
   #:thunk #:named-lambda

   #:scene
   #:scene-name
   #:scene-title
   #:scene-on-enter
   #:scene-display-scene
   #:display-scene
   #:enter-scene

   #:*events*
   #:predeclare-scene

   #:game-data
   #:get-game-data
   #:init-game-data
   #:connection-starship-name
   #:connection-fuel
   #:connection-sustenance
   #:connection-morale
   #:connection-current-scene
   #:connection-available-events
   #:pop-random-event

   ;; predeclared; defined later
   #:*intro-scene*))
(in-package :journey-of-the-hive/defs)

(defmacro thunk (&body body)
  "A function which ignores its arguments and evaluates BODY."
  (alex:with-gensyms (args)
    `(lambda (&rest ,args)
       (declare (ignore ,args))
       ,@body)))

(defmacro named-lambda (name arglist &body body)
  "Like `lambda', but the local function is named NAME and can be recursive."
  `(labels ((,name ,arglist ,@body))
     #',name))

(defclass scene ()
  ((%name :type symbol
          :initarg :name
          :accessor scene-name)
   (%title :type string
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

(defun enter-scene (scene connection)
  (when (slot-boundp scene '%on-enter)
    (funcall (scene-on-enter scene) (connection-body connection))))

(defvar *events* '()
  "A list of symbols which name scenes, to be used as dynamically-generated events.

Symbols are added to this list by the `define-scene' macro defined in defscene.lisp.")

(defmacro predeclare-scene (name &optional (docstring "Scene predeclared by `predeclare-scene'; to be redefined later by `define-scene'."))
  `(progn (defvar ,name)
          (setf (documentation ',name 'variable)
                ,docstring)))

(predeclare-scene *intro-scene*)

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
   (current-scene-name :type symbol
                       :accessor game-data-current-scene-name
                       :initarg :current-scene-name)
   (available-events :type list
                     :accessor game-data-available-events
                     :initarg :available-events))
  (:default-initargs :fuel 16
                     :sustenance 16
                     :morale 16
                     :current-scene-name '*intro-scene*
                     :available-events (alex:shuffle (copy-list *events*))))

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
  (symbol-value (game-data-current-scene-name (get-game-data connection))))

(defun (setf connection-current-scene) (new-scene connection)
  (setf (game-data-current-scene-name (get-game-data connection)) (scene-name new-scene)))

(defun connection-available-events (connection)
  (game-data-available-events (get-game-data connection)))

(defun (setf connection-available-events) (new-list connection)
  (setf (game-data-available-events (get-game-data connection)) new-list))

(defun pop-random-event (connection)
  (or (pop (connection-available-events connection))
      (error "still to write: final scene")))

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

   #:*events* #:*all-scenes*
   #:predeclare-scene

   #:game-data
   #:get-game-data
   #:init-game-data
   #:game-data-slot-value
   #:connection-starship-name
   #:connection-fuel #:fuel
   #:connection-sustenance #:sustenance
   #:connection-morale #:morale
   #:connection-fuel-capacity #:fuel-capacity
   #:connection-sustenance-capacity #:sustenance-capacity
   #:connection-current-scene
   #:connection-available-events
   #:get-scene-data
   #:remove-scene-data
   #:pop-random-event

   #:out-of-resource #:out-of-resource-name
   
   ;; predeclared; defined later
   #:*intro-scene*
   #:*victory*))
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

Symbols are added to this list by the `define-event' macro defined in defscene.lisp.")

(defvar *all-scenes* '()
  "A list of symbols which name scenes, to be used in debugging for jump-to-scene.

Symbols are added to this list by the `define-scene' macro defined in defscene.lisp")

(defmacro predeclare-scene (name &optional (docstring "Scene predeclared by `predeclare-scene'; to be redefined later by `define-scene'."))
  `(progn (defvar ,name)
          (setf (documentation ',name 'variable)
                ,docstring)))

(predeclare-scene *intro-scene*)
(predeclare-scene *victory*)

(defparameter *starting-fuel* 8)
(defparameter *starting-sustenance* 8)
(defparameter *starting-morale* 8)

(defclass game-data ()
  ((starship-name :type string
                  :accessor game-data-starship-name)
   (fuel :type (and fixnum unsigned-byte)
         :accessor game-data-fuel
         :initarg :fuel)
   (fuel-capacity :type (and fixnum unsigned-byte)
                  :accessor game-data-fuel-capacity
                  :initarg :fuel-capacity)
   (sustenance :type (and fixnum unsigned-byte)
               :accessor game-data-sustenance
               :initarg :sustenance)
   (sustenance-capacity :type (and fixnum unsigned-byte)
                        :accessor game-data-sustenance-capacity
                        :initarg :sustenance-capacity)
   (morale :type (and fixnum unsigned-byte)
           :accessor game-data-morale
           :initarg :morale)
   (current-scene-name :type symbol
                       :accessor game-data-current-scene-name
                       :initarg :current-scene-name)
   (available-events :type list
                     :accessor game-data-available-events
                     :initarg :available-events)
   (scene-data :type hash-table
               :accessor game-data-scene-data
               :initarg :scene-data
               :documentation "An EQ hash table which maps scene symbols to their scene data, if any."))
  (:default-initargs :fuel *starting-fuel*
                     :sustenance *starting-sustenance*
                     :morale *starting-morale*
                     :fuel-capacity *starting-fuel*
                     :sustenance-capacity *starting-sustenance*
                     :current-scene-name '*intro-scene*
                     :available-events (alex:shuffle (copy-list *events*))
                     :scene-data (make-hash-table :test 'eq)))

(defun get-game-data (connection)
  (connection-data-item connection "game-data"))

(defun init-game-data (connection)
  (setf (connection-data-item connection "game-data")
        (make-instance 'game-data)))

(defun game-data-slot-value (connection slot-name)
  (slot-value (get-game-data connection) slot-name))

(defun (setf game-data-slot-value) (new-value connection slot-name)
  (setf (slot-value (get-game-data connection) slot-name)
        new-value))

(defun connection-starship-name (connection)
  (game-data-starship-name (get-game-data connection)))

(defun (setf connection-starship-name) (new-name connection)
  (setf (game-data-starship-name (get-game-data connection)) new-name))

(defun connection-fuel (connection)
  (game-data-fuel (get-game-data connection)))

(defun (setf connection-fuel) (new-fuel connection)
  (setf (game-data-fuel (get-game-data connection)) new-fuel))

(defun connection-fuel-capacity (connection)
  (game-data-fuel-capacity (get-game-data connection)))

(defun (setf connection-fuel-capacity) (new-capacity connection)
  (setf (game-data-fuel-capacity (get-game-data connection)) new-capacity))

(defun connection-sustenance (connection)
  (game-data-sustenance (get-game-data connection)))

(defun (setf connection-sustenance) (new-sustenance connection)
  (setf (game-data-sustenance (get-game-data connection)) new-sustenance))

(defun connection-sustenance-capacity (connection)
  (game-data-sustenance-capacity (get-game-data connection)))

(defun (setf connection-sustenance-capacity) (new-capacity connection)
  (setf (game-data-sustenance-capacity (get-game-data connection)) new-capacity))

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

(defun get-scene-data (connection scene-name)
  (check-type scene-name symbol)
  (gethash scene-name (game-data-scene-data (get-game-data connection))))

(defun (setf get-scene-data) (new-data connection scene-name)
  (check-type scene-name symbol)
  (setf (gethash scene-name (game-data-scene-data (get-game-data connection)))
        new-data))

(defun remove-scene-data (connection scene-name)
  (check-type scene-name symbol)
  (remhash scene-name (game-data-scene-data (get-game-data connection))))

(defun pop-random-event (connection)
  (or (pop (connection-available-events connection))
      *victory*))

(define-condition out-of-resource (error)
  ((resource-name :type (member 'fuel 'sustenance 'morale)
                  :initarg :resource-name
                  :accessor out-of-resource-name)))

(uiop:define-package :journey-of-the-hive/defscene
  (:use :cl :clog :clog-gui :journey-of-the-hive/defs)
  (:local-nicknames (#:alex :alexandria))
  (:export
   #:define-scene
   #:define-event))
(in-package :journey-of-the-hive/defscene)

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

  (defun make-scene-form (name title on-enter display-scene)
    `(make-instance 'scene
                    :name ',name
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
    `(progn (defparameter ,special-var-name
              ,(make-scene-form special-var-name title on-enter display-scene))
            (pushnew ',special-var-name *all-scenes*))))

(defmacro define-event (scene-name &body clauses)
  "Define a `scene' object and add its name to `*events*'"
  `(progn (define-scene ,scene-name ,@clauses)
          (pushnew ',scene-name *events*)))

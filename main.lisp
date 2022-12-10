(uiop:define-package :journey-of-the-hive/main
  (:nicknames :journey-of-the-hive)
  (:use
   :cl
   :clog
   :clog-gui
   :journey-of-the-hive/defs
   :journey-of-the-hive/gui
   :journey-of-the-hive/intro-scenes
   :journey-of-the-hive/ending-scenes
   :journey-of-the-hive/events)
  (:local-nicknames (#:alex :alexandria))
  (:export #:start-app))

(in-package :journey-of-the-hive/main)

(defun on-new-window (connection)
  (setf (title (html-document connection)) "Journey of the Hive")
  (clog-gui-initialize connection)
  (init-game-data connection)
  (add-class connection "w3-teal")
  (make-menu-bar connection)
  (redisplay-current-scene connection))

(defun start-app (&key (port 8080))
  (initialize 'on-new-window
   :static-root (merge-pathnames "./www/"
                  (asdf:system-source-directory :journey-of-the-hive))
   :port port))

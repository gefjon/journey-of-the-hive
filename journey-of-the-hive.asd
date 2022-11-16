(asdf:defsystem #:journey-of-the-hive
  :description "New CLOG System"
  :author "some@one.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :entry-point "journey-of-the-hive:start-app"  
  :depends-on (#:clog) ; add clog plugins here as #:plugin for run time
  :components ((:file "journey-of-the-hive")))

(asdf:defsystem #:journey-of-the-hive/tools
  :defsystem-depends-on (:clog)
  :depends-on (#:journey-of-the-hive #:clog/tools) ; add clog plugins here as #:plugin/tools for design time
  :components ())

(asdf:defsystem "journey-of-the-hive"
  :description "A game for 4-ish players about democracy, cooperation, and space bugs."
  :author "phoebe Goldman <phoebe@goldman-tribe.org>"
  :license  "MIT"
  :version "0.0.1"
  :class :package-inferred-system
  :entry-point "journey-of-the-hive:start-app"  
  :depends-on ("journey-of-the-hive/main"))

(asdf:register-system-packages "clog" '(:clog-gui))

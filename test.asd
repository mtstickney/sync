;;; ASDF definition for testing new components

(asdf:defsystem #:test
  :serial t
  :description "Describe probuild here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :defsystem-depends-on (#:probuild)
  :components ((:abl-module "gui"
                            :databases (("compass" :singleuser :pathname "dbase/compass"))
                            :pathname "bar/baz"
                            :components ((:abl-module "history"
                                                      :databases (("history" :alias "compass"))
                                                      :pathname "blargl"
                                                      :inherit-databases nil
                                                      :components ((:procedure-file "foo")))))))

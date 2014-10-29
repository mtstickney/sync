;;; ASDF definition for testing new components

(asdf:defsystem #:test
  :serial t
  :description "Describe probuild here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :defsystem-depends-on (#:probuild)
  :class :abl-system
  :components ((:abl-module "gui"
                            :databases (("compass" :singleuser t :pathname "dbase/compass"))
                            :pathname "bar/baz"
                            :components ((:abl-module "history"
                                                      :databases (("history" :pathname "compass.db"
                                                                             :port 4000
                                                                             :host "127.0.0.1"
                                                                             :username "sysprogress"
                                                                             :password "sysprogress"))
                                                      :pathname "blargl"
                                                      :inherit-databases nil
                                                      :components ((:procedure-file "foo")))))))

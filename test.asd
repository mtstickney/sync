;;; ASDF definition for testing new components

(asdf:defsystem #:test
  :serial t
  :description "Describe probuild here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :defsystem-depends-on (#:probuild)
  :components ((:db-unit "gui"
                         :databases (("compass" :singleuser :pathname "dbase/compass"))
                         :pathname "bar/baz"
                         :components ((:db-unit "history"
                                                :databases (("history" :alias compass))
                                                :pathname "blargl"
                                                :inherit-databases nil
                                                :components ((:procedure-file "foo")))))))

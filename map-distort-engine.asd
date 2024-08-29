(defsystem "map-distort-engine"
  :long-name ""
  :version "0.0.1"
  :author "eehah5ru"
  :maintainer "eehah5ru"
  :mailto "eeefff.org@riseup.net"
  :license "MIT"
  :homepage "eeefff.org"
  :bug-tracker ""
  :depends-on ("cl-dot"
               "cl-digraph"
               "access"
               "alexandria"
               "iterate"
               "log4cl"
               "trivial-signal"
               "bordeaux-threads"
               ;; html parsing
               "lquery"
               ;; pathname utils
               "cl-fad"
               "str"
               "plump"
               "cl-ppcre"
               "parse-number"
               "osc"
               "usocket"
               "lparallel"
               "3d-math"
               :cl-cgal
               :sb-sprof
               :flamegraph)  
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "ofx"
                  :depends-on ("shake"
                               "shift"
                               "contours-keyer"))
                 (:file "svg-file")
                 (:file "svg-map")
                 (:file "shake"
                  :depends-on ("svg-file"
                               "svg-map"))
                 (:file "shift"
                  :depends-on ("svg-file"
                               "svg-map"))

                 (:file "contours-keyer"
                  :depends-on ("svg-file"
                               "svg-map"
                               "shake"))
                 
                 )))
  :description ""
  :long-description ""
  :in-order-to ((test-op (test-op :map-distort-engine/test)))
  )

(asdf:defsystem map-distort-engine/test
  :depends-on (
               :fiveam
               :map-distort-engine
               )
  :components ((:module "tests"
                :components
                ((:file "prelude")
                 (:file "main"
                  :depends-on ("prelude"))
                 (:file "lquery"
                  :depends-on ("prelude")))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :map-distort-engine :map-distort-engine.test))))



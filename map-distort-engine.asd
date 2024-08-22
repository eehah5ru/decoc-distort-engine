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
               "3d-math"
               :cl-cgal)  
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "ofx"
                  :depends-on ("shake"
                               "shift"))
                 (:file "svg-file")
                 (:file "svg-map")
                 (:file "shake"
                  :depends-on ("svg-file"
                               "svg-map"))
                 (:file "shift"
                  :depends-on ("svg-file"
                               "svg-map")))))
  :description ""
  :long-description "")


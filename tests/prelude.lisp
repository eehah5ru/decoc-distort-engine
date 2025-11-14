(defpackage map-distort-engine.test
  (:use :cl
        :fiveam
        :lquery
        :map-distort-engine
        :map-distort-engine.svg-file
        )

  )
(in-package :map-distort-engine.test)


;; root test suite
(def-suite map-distort-engine
  :description "Test map distort engine")

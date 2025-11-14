;; (defpackage map-distort-engine.test
;;   (:use :cl
;;         :fiveam
;;         :map-distort-engine
;;         )

;;   ;; (:import-from #:cl-cgal.ffi
;;   ;;               #:point)
;;   )
(in-package :map-distort-engine.test)

(use-package :map-distort-engine)

;;; root test suite
;; (def-suite map-distort-engine
;;   :description "Test map distort engine")

;;; example
(def-suite dummy
  :description "test dummy"
  :in map-distort-engine)

(in-suite dummy)

(test should-car
  (is (= 1
         (car '(1 2)))))

;; (defpackage map-distort-engine.test.lquery
;;   (:use :cl
;;         :fiveam
;;         :lquery
;;         :map-distort-engine
;;         )

  ;; (:import-from #:cl-cgal.ffi
  ;;               #:point)
  ;; )

(in-package :map-distort-engine.test)

(use-package :lquery)


;;; root test suite
;; (def-suite map-distort-engine
;;   :description "Test map distort engine")

;;; example
(def-suite lquery
  :description "test lquery features"
  :in map-distort-engine)

(in-suite lquery)

(test should-car2
  (is (= 1
         (car '(1 2)))))



(def-suite* lquery-list-function
  :description "test behaviour of lquery-list-function"
  :in lquery)

;;; what to test
(define-lquery-list-function leave-only-first-node (nodes)
  (let ((r (elt nodes 0)))
    (delete-if (lambda (x) t)
               nodes)
    nodes)
  )


(test should-return-nodes
  (let* ((sf (mk-svg-file (asdf:system-relative-pathname
                           :map-distort-engine
                           "data/diagrams/map.svg")))
         (d (xml-doc sf))
         (d* (lquery:$ d
               "text"
               (leave-only-first-node)
               (root))))
    (is (= 0
           (length (lquery:$ d*
                     "text"))))))

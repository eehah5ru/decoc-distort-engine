(defpackage map-distort-engine.focus-keyer
  (:use :cl
   :alexandria
   :access
   :iterate
   :lquery
   :parse-number

   :lparallel

   :map-distort-engine.svg-file
   :map-distort-engine.svg-map)


  (:import-from :map-distort-engine.shaker
                 #:randomise-text-position*
                 #:randomise-polyline-position*
                 #:randomise-path-position*)

  (:import-from :map-distort-engine.shifter
                shift-out-text-position
                shift-out-polyline-position
                shift-out-path-position)


  )

(in-package :map-distort-engine.focus-keyer)

(defvar *texts* nil)
(defvar *polylines* nil)
(defvar *pathes* nil)

;;;
;;; PARALLEL PROCESS ALL TEXTS
;;;
(defun process-texts (nodes sf focus radius &key (strategy :shift))
  (when (not (member strategy '(:shift :randomise)))
    (error "unknown process strategy: ~a" strategy))

  (let* ((focus-x (cl-cgal:x focus))
         (focus-y (cl-cgal:y focus))
         ;; in diagram coords
         (focus* (view->diagram* sf focus))
         (radius* (view->diagram* sf (cons radius radius)))
         (radius* (min (cl-cgal:x radius*) (cl-cgal:y radius*)))
         )
    (lparallel:pmap 'vector
                    (lambda (node)
                      (when (text-nearby-p* sf
                                            node
                                            focus-x
                                            focus-y
                                            radius*)
                        (alexandria:switch (strategy)
                          (:shift
                           (shift-out-text-position node focus* radius*))
                          (:randomise
                           (randomise-text-position* node radius*)))
                        )
                      node)
                    nodes))
  )

;; ;;; lquery variant
;; (define-lquery-list-function process-texts (nodes sf contour radius)
;;   (process-texts* nodes sf contour radius))

;;;
;;; PARALLEL PROCESS ALL POLYLINES
;;;
(defun process-polylines (nodes sf focus radius &key (strategy :shift))
  (when (not (member strategy '(:shift :randomise)))
    (error "unknown process strategy: ~a" strategy))

  (let* ((focus-x (cl-cgal:x focus))
         (focus-y (cl-cgal:y focus))
         ;; in diagram coords
         (focus* (view->diagram* sf focus))
         (radius* (view->diagram* sf (cons radius radius)))
         (radius* (min (cl-cgal:x radius*) (cl-cgal:y radius*))))
    (lparallel:pmap 'vector
                    (lambda (node)
                      (when (polyline-nearby-p* sf
                                                node
                                                focus-x
                                                focus-y
                                                radius*)
                        (alexandria:switch (strategy)
                          (:shift
                           (shift-out-polyline-position node focus* radius))
                          (:randomise
                           (randomise-polyline-position* node radius*)))
                        )
                      node)
                    nodes)))

;; ;;; lquery version
;; (define-lquery-list-function process-polylines (nodes sf contour radius)
;;   (process-polylines* nodes sf contour radius))

;;;
;;; PATHES /  PARALLEL PROCESS WITH LQUERY
;;;
(defun process-pathes (nodes sf focus radius &key (strategy :shift))
  (when (not (member strategy '(:shift :randomise)))
    (error "unknown process strategy: ~a" strategy))

  (let* ((focus-x (cl-cgal:x focus))
         (focus-y (cl-cgal:y focus))
         ;; in diagram coords
         (focus* (view->diagram* sf focus))
         (radius* (view->diagram* sf (cons radius radius)))
         (radius* (min (cl-cgal:x radius*) (cl-cgal:y radius*))))
    (lparallel:pmap 'vector
                    (lambda (node)
                      (when (path-nearby-p* sf
                                            node
                                            focus-x
                                            focus-y
                                            radius*)
                        (alexandria:switch (strategy)
                          (:shift
                           (shift-out-path-position node focus* radius*))
                          (:randomise
                           (randomise-path-position* node radius*)))
                        )
                      node)
                    nodes)))

;; ;;; lquery version
;; (define-lquery-list-function process-pathes (nodes sf contour radius)
;;   (process-pathes* nodes sf contour radius))


;;;
;;;
;;; svg file lazy accessors
;;;
;;;
;;; TODO: extract in separate file

;;;
;;; TEXTS / lazy
;;;
(defmacro with-svg-file-texts (sf &body body)
  (let ((d (gensym)))
    `(progn
       (let* ((,d (xml-doc ,sf))
              (*texts* (lquery:$ ,d
                         "text")))
         ,@body))))
;;;
;;; POLYLINES / lazy
;;;
(defmacro with-svg-file-polylines (sf &body body)
  (let ((d (gensym)))
    `(progn
       (let* ((,d (xml-doc ,sf))
              (*polylines* (lquery:$ ,d
                             "polyline")))
         ,@body))))


;;;
;;; PATHES / lazy
;;;
(defmacro with-svg-file-pathes (sf &body body)
  (let ((d (gensym)))
    `(progn
       (let* ((,d (xml-doc ,sf))
              (*pathes* (lquery:$ ,d
                          "path")))
         ,@body))))


;;;
;;;
;;; SHAKE POSITIONS NEAR FOCUS
;;;
;;; coords of focus and radius in 0..1
;;;
(defun shake-positions-near-focus (sf focus-point focus-radius)
  (check-type focus-point cl-cgal:point-like)
  (check-type focus-radius float)

  (log:info "shaking map near focus: ~a, ~a" focus-point focus-radius)

  (with-svg-file-texts sf
   (with-svg-file-polylines sf
     (with-svg-file-pathes sf
       (process-texts *texts* sf focus-point (* 1.5 focus-radius) :strategy :randomise)
       (process-polylines *polylines* sf focus-point (* 1.5 focus-radius) :strategy :randomise)
       (process-pathes *pathes* sf focus-point (* 1.5 focus-radius) :strategy :randomise)

       (process-texts *texts* sf focus-point focus-radius :strategy :shift)
       (process-polylines *polylines* sf focus-point focus-radius :strategy :shift)
       (process-pathes *pathes* sf focus-point focus-radius :strategy :shift)

       )))

  )

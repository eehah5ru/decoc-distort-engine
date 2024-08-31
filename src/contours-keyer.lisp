(defpackage map-distort-engine.contours-keyer
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
  

  ;; (:export
  ;;  shake-position-nearby-f)
  )

(in-package :map-distort-engine.contours-keyer)

(defvar *texts* nil)
(defvar *polylines* nil)
(defvar *pathes* nil)

;;; 
;;; PARALLEL PROCESS ALL TEXTS
;;; 
(defun process-texts* (nodes sf contour radius)
  (lparallel:pmap 'vector
                  (lambda (node)
                    (when (text-inside-p* sf node contour)
                      (randomise-text-position* node radius))
                    node)
                  nodes))
;;; lquery variant
(define-lquery-list-function process-texts (nodes sf contour radius)
  (process-texts* nodes sf contour radius))

;;; 
;;; PARALLEL PROCESS ALL POLYLINES
;;; 
(defun process-polylines* (nodes sf contour radius)
  (lparallel:pmap 'vector
                  (lambda (node)
                    (when (polyline-inside-p* sf node contour)
                      (randomise-polyline-position* node radius))
                    node)
                  nodes))

;;; lquery version
(define-lquery-list-function process-polylines (nodes sf contour radius)
  (process-polylines* nodes sf contour radius))

;;; 
;;; PATHES /  PARALLEL PROCESS WITH LQUERY
;;;
(defun process-pathes* (nodes sf contour radius)
  (lparallel:pmap 'vector
                  (lambda (node)
                    (when (path-inside-p* sf node contour)
                      (randomise-path-position* node radius))
                    node)
                  nodes))

;;; lquery version
(define-lquery-list-function process-pathes (nodes sf contour radius)
  (process-pathes* nodes sf contour radius))


;;;
;;;
;;; svg file lazy accessors
;;;
;;;

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
;;; SHAKE POSITIONS INSIDE OF EACH CONTOUR
;;;
;;; coords of contour in 0..1
;;;
(defun shake-positions-inside-contours (sf contours)
  (check-type contours cons)

  (log:info "shaking map inside ~a contours" (length contours))

  (with-svg-file-texts sf
    (with-svg-file-polylines sf
      (with-svg-file-pathes sf
        (iter (for c in contours)
          (shake-positions-inside-contour sf c)))))
  )


(defun shake-positions-inside-contour (sf contour)
  (check-type contour cons)

  (let* (;; (d (xml-doc sf))
         ;; TODO: replace with (sqrt (cl-cgal:polygon_area contour))
         (radius 500))
    (process-texts* *texts* sf contour radius)
    (process-polylines* *polylines* sf contour radius)
    (process-pathes* *pathes* sf contour radius)
    ;; (lquery:$ d
    ;;   ;; texts
    ;;   "text"
    ;;   ;; (filter #'(lambda (el)
    ;;   ;;             (text-inside-p* sf el contour)))
    ;;   ;; (randomise-text-position radius)
    ;;   (process-texts sf contour radius)
    ;;   (root)
    ;;   ;; polylines
    ;;   "polyline"
    ;;   ;; (filter #'(lambda (el)
    ;;   ;;             (polyline-inside-p* sf el contour)))
    ;;   ;; (randomise-polyline-position radius)
    ;;   (process-polylines sf contour radius)
    ;;   (root)
    ;;   ;; pathes
    ;;   "path"
    ;;   ;; (filter #'(lambda (el)
    ;;   ;;             (path-inside-p* sf el contour)))
    ;;   ;; (randomise-path-position radius)
    ;;   (process-pathes sf contour radius)
    ;;   (root)
    ;;   )
    )
  )

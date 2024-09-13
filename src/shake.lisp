(defpackage map-distort-engine.shaker
  (:use :cl
   :alexandria
   :access
   :iterate
   :lquery
   :parse-number

   :map-distort-engine.svg-file
   :map-distort-engine.svg-map
   )

  (:import-from :cl-cgal
                #:point-x
                #:point-y
                #:point-like
                #:p
                #:p-p
                #:p-x
                #:p-y
                #:make-p)


  (:export
   shake-position-nearby-f
   randomise-text-position
   randomise-polyline-position
   randomise-path-position
   randomise-text-position*
   randomise-polyline-position*
   randomise-path-position*

   ))

(in-package :map-distort-engine.shaker)

;;;
;;;
;;; PARAMS
;;;
;;;

(defparameter *delta-position* 1000)

;;;
;;;
;;; LQUERY FUNCTIONS
;;;
;;;

;;;
;;; LQUERY / RANDOMIZE / TEXT POSITION
;;;
;;; list version
(defun randomise-text-position* (n radius)
  ;; (log:info "randomising / text")

  (let* ((x (or (plump:attribute n "x")
                (error 'svg-attr-not-found :attr-name "x")))
         (y (or (plump:attribute n "y")
                (error 'svg-attr-not-found :attr-name "y")))
         (x (coerce (parse-number x) 'float))
         (y (coerce (parse-number y) 'float))
         (dx (- (random radius)
                (/ radius 2)))
         (dy (- (random radius)
                (/ radius 2)))
         (nx (+ x dx))
         (ny (+ y dy)))
    (setf (plump:attribute n "x")
          (format nil "~d" nx))
    (setf (plump:attribute n "y")
          (format nil "~d" ny))
    ;; return node
    n))

;;; lquery version
(define-lquery-function randomise-text-position (n radius)
  (randomise-text-position* n radius))

;;;
;;; LQUERY / RANDOMIZE / POLYLINE POSITION
;;;
(defun randomise-polyline-position* (n radius)
  ;; (log:info "randomising / polyline")

  (let* ((pline (svg->polyline n))
         (translate-vec (mk-rand-position-vector radius)))
    (translate pline translate-vec)
    (polyline->svg pline n)))

;;; lquery version
(define-lquery-function randomise-polyline-position (n radius)
  (randomise-polyline-position* n radius))

;;;
;;; LQUERY / RANDOMIZE / PATH POSITION
;;;
(defun randomise-path-position* (n radius)
  ;; (log:info "randomising / path")

  (let* ((pth (svg->path n))
         (translate-vec (mk-rand-position-vector radius)))
    (translate pth translate-vec)
    (path->svg pth n)))

;;; lquery version
(define-lquery-function randomise-path-position (n radius)
  (randomise-path-position* n radius))

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
;;; SHAKE FUNCTIONS
;;;
;;;

;;;
;;; SHAKE POSITIONS NEAR POINT
;;; coords are in float 0..1
;;;
(defun shake-position-nearby-f (sf x y radius)
  (check-type x float)
  (check-type y float)

  (log:info "shaking positions")
  ;; (with-svg-file-texts sf
  ;;   (with-svg-file-polylines sf
  ;;     (with-svg-file-pathes sf
  ;;       )))
  (let* ((d (xml-doc sf)))
    (lquery:$ d
      ;; texts
      "text"
      (filter #'(lambda (el)
                  (text-nearby-p* sf el x y radius)))
      (randomise-text-position radius)
      (root)
      ;; polylines
      "polyline"
      (filter #'(lambda (el)
                  (polyline-nearby-p* sf el x y)))
      (randomise-polyline-position radius )
      (root)
      ;; pathes
      "path"
      (filter #'(lambda (el)
                  (path-nearby-p* sf el x y radius)))
      (randomise-path-position radius)
      (root)

      )))

;;;
;;;
;;; GEOMETRY UTILS
;;;
;;;

;;;
;;; MAKE RANDOMIZING VECTOR
;;; -> cgal:p x y
;;;
(defun mk-rand-position-vector (radius)
  (make-p :x (- (random radius)
                (/ radius 2))
          :y (- (random radius)
                (/ radius 2))))


;;;
;;;
;;; TEST FUNCS
;;;
;;;
(defun test-shake-position-nearby ()
  (let* ((sf (mk-svg-file "data/test_map.svg")))
    (shake-position-nearby-f sf
                             (random 1.0)
                             (random 1.0)
                             (+ 100
                                (random 1000.0)))
    (save-svg-to-file sf "data/test_map_out.svg")))

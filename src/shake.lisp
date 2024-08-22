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

  (:export
   shake-position-nearby-f))

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
(define-lquery-function randomise-text-position (n radius)
  ;; (log:info "randomising / text")

  (let* ((x (or (plump:attribute n "x")
                (error 'svg-attr-not-found)))
         (y (or (plump:attribute n "y")
                (error 'svg-attr-not-found)))
         (x (parse-number x))
         (y (parse-number y))
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

;;;
;;; LQUERY / RANDOMIZE / POLYLINE POSITION
;;;
(define-lquery-function randomise-polyline-position (n radius)
    ;; (log:info "randomising / polyline")

  (let* ((pline (svg->polyline n))
         (translate-vec (mk-rand-position-vector radius)))
    (translate pline translate-vec)
    (polyline->svg pline n)))

;;;
;;; LQUERY / RANDOMIZE / PATH POSITION
;;;
(define-lquery-function randomise-path-position (n radius)
  (let* ((p (svg->path n))
         (translate-vec (mk-rand-position-vector radius)))
    (translate p translate-vec)
    (path->svg p n)))

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
;;; -> cons x y
;;;
(defun mk-rand-position-vector (radius)
  (cons (- (random radius)
           (/ radius 2))
        (- (random radius)
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

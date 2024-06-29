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
   shake-font-sizes-nearby-f
   shake-position-nearby-f))

(in-package :map-distort-engine.shaker)

;;;
;;;
;;; PARAMS
;;;
;;;

(defparameter *delta-position* 200)

;;;
;;;
;;; LQUERY FUNCTIONS
;;;
;;;

;;;
;;; LQUERY / RANDOMIZE / TEXT POSITION
;;;
;;; TODO GENERALIZE for non-text els
(define-lquery-function randomise-text-position (n &rest args)
  ;; (log:info "randomising / text")

  (let* ((x (or (plump:attribute n "x")
                (error 'svg-attr-not-found)))
         (y (or (plump:attribute n "y")
                (error 'svg-attr-not-found)))
         (x (parse-number x))
         (y (parse-number y))
         (dx (- (random *delta-position*)
                (/ *delta-position* 2)))
         (dy (- (random *delta-position*)
                (/ *delta-position* 2)))
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
(define-lquery-function randomise-polyline-position (n &rest args)
    ;; (log:info "randomising / polyline")

  (let* ((pline (svg->polyline n))
         (dx (- (random *delta-position*)
                (/ *delta-position* 2)))
         (dy (- (random *delta-position*)
                (/ *delta-position* 2)))
)
    (translate pline (cons dx dy))
    (polyline->svg pline n)))

;;;
;;;
;;; SHAKE FUNCTIONS
;;;
;;;

;;;
;;; SHAKE POSITIONS NEAR POINT
;;; coords are in float 0..1
;;;
(defun shake-position-nearby-f (sf x y)
  (check-type x float)
  (check-type y float)

  (log:info "shaking positions")
  (let* ((d (xml-doc sf)))
    (lquery:$ d
      ;; texts
      "text"
      (filter #'(lambda (el)
                  (text-nearby-p* sf el x y)))
      (randomise-text-position)
      (root)
      ;; polylines
      "polyline"
      (filter #'(lambda (el)
                  (polyline-nearby-p* sf el x y)))
      (randomise-polyline-position)
      (root)
      ;; pathes
      ;; TODO: add pathes randomisation
      )))

;;;
;;;
;;; TEST FUNCS
;;;
;;;
(defun test-shake-position-nearby ()
  (let* ((sf (mk-svg-file "data/test_map.svg")))
    (shake-position-nearby-f sf 0.5 0.5)
    (save-svg-to-file sf "data/test_map_out.svg")))

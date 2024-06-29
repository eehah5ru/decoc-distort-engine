(defpackage map-distort-engine.svg-map
  (:use
   :cl
   :alexandria
   :access
   :iterate
   :lquery
   :parse-number
   ;; :str
   :map-distort-engine.svg-file)
  (:export
   text-nearby-p*
   polyline-nearby-p*

   ;; polyline
   polyline
   polyline-points
   polyline-svg
   svg->polyline
   polyline->svg

   ;; geometry functions
   translate
   )
  )

(in-package :map-distort-engine.svg-map)

;;;
;;;
;;;
;;; representaiton and utils of map specifix svg els
;;;
;;;

;;;
;;;
;;; parameters
;;;
;;;

;;;
;;; max distance for nearby points
;;;
(defparameter *max-nearby-distance* 500)

;;;
;;;
;;; POLYLINE WRAPPER
;;;
;;;
(defclass polyline ()
  (
   ;; svg underlying el
   (svg :initarg :svg
        :initform nil
        :accessor polyline-svg)
   (points
    :initarg :points
    :initform nil
    :accessor polyline-points)))


;;;
;;; factory method from SVG
;;;
(defun svg->polyline (svg)
  (let* ((points (or (plump:attribute svg "points")
                     (error 'svg-attr-not-found "points")))
         (points (str:words points))
         ;; get coords structure in text
         (points (mapcar (lambda (xy)
                           (str:split "," xy))
                         points))
         ;; convert to numbers
         (points (mapcar (lambda (xy)
                           (cons (parse-number (car xy))
                                 (parse-number (cadr xy))))
                         points)))
    ;; (log:info points)
    (make-instance 'polyline
                   :svg svg
                   :points points)))

;;;
;;;
;;; polyline object -> SVG back
;;; need to sync changes: ex. points
;;;
(defun polyline->svg (pl svg)
  ;; (log:info (polyline-points pl))
  (let* ((points (polyline-points pl))
         (points (mapcar
                  (lambda (point)
                    (format nil
                            "~a,~a"
                            (car point)
                            (cdr point)))
                  points))
         (points (str:unwords points)))
    (setf (plump:attribute svg "points")
          points)
    svg))


;;;
;;;
;;; GEOMETRY FUNCS
;;;
;;;
(defgeneric translate (o vec)
  (:documentation
   "translate object accordign to vector")

  ;;
  ;; translate polyline
  ;;
  (:method ((o polyline) (vec cons))
    (let* ((points (polyline-points o))
           (points (mapcar
                    (lambda (point)
                      (cons (+ (car point) (car vec))
                            (+ (cdr point) (cdr vec))))
                    points)))
      ;; modifying source object!
      (setf (access o 'points)
            points)
      ;; return object
      o)))


;;;
;;;
;;; COORDINATES CONVERSION
;;;
;;; apply scale and transform
;;;
(defun view->diagram (sf point)
  (check-type point cons)

  (let* ((v-x (car point))
         (v-y (cdr point))
         ;; (x v-x)
         ;; (y v-y)
         (x (/ v-x (svg-file-scale sf)))
         (y (/ v-y (svg-file-scale sf)))
         (x (- x (svg-file-translate-x sf)))
         (y (- y (svg-file-translate-y sf)))

         )
    (cons x y)))


;;;
;;;
;;; NEARBY PREDICATES
;;;
;;;


;;;
;;; NEARBY / TEXT
;;; in doc coordinates
;;;
(defun text-nearby-p (sf el x y)
  (let* ((el-x (or (plump:attribute el "x")
                   (error 'svg-attr-not-found "x")))
         (el-y (or (plump:attribute el "y")
                   (error 'svg-attr-not-found "y")))
         (el-x (parse-number el-x))
         (el-y (parse-number el-y))
         ;; point in diagam coords
         (d-point (view->diagram sf (cons x y)))
         (dist (sqrt (+ (expt (- el-x (car d-point)) 2)
                        (expt (- el-y (cdr d-point)) 2)))))
    ;; check if inside of given circle
    (< dist *max-nearby-distance*)))

;;;
;;; NEARBY / TEXT / FLOAT COORDS
;;; in float 0..1
;;;
;;; float args 0..1
;;; then converted to vide width & height of diagram
(defun text-nearby-p* (sf el x y)
  (check-type x float)
  (check-type y float)
  (text-nearby-p sf
               el
               (* (svg-file-width sf) x)
               (* (svg-file-height sf) y)))



;;;
;;; NEARBY / POLYLINE / FLOAT COORDS
;;; x y in float 0 .. 1
;;; points format
;;; points="778.884,106.071 776.574,108.987"
(defun polyline-nearby-p* (sf svg-pline x y)
  (check-type x float)
  (check-type y float)

  (let* ((x (* x (svg-file-width sf)))
         (y (* y (svg-file-height sf)))
         (point (cons x y))
         (point (view->diagram sf point))
         (pline (svg->polyline svg-pline)))
    ;; true when at least one is nearby
    (some (lambda (pline-point)
            (points-nearby-p point pline-point))
          (polyline-points pline))))


;;;
;;; NEARBY / UTILS
;;;
(defun points-nearby-p (p1 p2)
  (check-type p1 cons)
  (check-type p2 cons)
  (let* ((dist (sqrt (+ (expt (- (car p2) (car p1)) 2)
                        (expt (- (cdr p2) (cdr p1)) 2)))))
    (< dist *max-nearby-distance*)))

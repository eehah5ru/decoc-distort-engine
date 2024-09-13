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
   text-nearby-p*
   polyline-nearby-p*
   path-nearby-p*

   text-inside-p*
   polyline-inside-p*
   path-inside-p*

   ;; polyline
   polyline
   polyline-points
   polyline-svg
   svg->polyline
   polyline->svg

   ;; path
   path
   path-d
   path-points
   svg->path
   path->svg

   ;; geometry functions
   view->diagram
   view->diagram*
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
(defparameter *max-nearby-distance* 500.0)


(define-condition svg-malformed-attr (error)
  ((attr-name
    :initarg :attr-name)))

(define-condition svg-attr-not-found (error)
  ((attr-name
    :initarg :attr-name)))

;;;
;;; global optimisation rules
;;;
;; (declaim (optimize (speed 3) (safety 0)))

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
    :type (or null
              (array p))
    :initform nil
    :accessor polyline-points)))

;;;
;;; factory method from SVG
;;;
;; (declaim (inline svg->polyline))
(defun svg->polyline (svg)
  (let* ((points (or (plump:attribute svg "points")
                     (error 'svg-attr-not-found :attr-name "points")))
         (points (str:words points))
         ;; get coords structure in text
         (points (mapcar (lambda (xy)
                           (str:split "," xy))
                         points))
         ;; convert to numbers
         (points (mapcar (lambda (xy)
                           (make-p :x (coerce (parse-number (car xy)) 'float)
                                   :y (coerce (parse-number (cadr xy)) 'float)))
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
                            (p-x point)
                            (p-y point)))
                  points))
         (points (str:unwords points)))
    (setf (plump:attribute svg "points")
          points)
    svg))

;;;
;;;
;;; PATH WRAPPER
;;;
;;;
(defclass path ()
  (
   ;; d svg element
   (d :initarg :d
      :initform nil
      :accessor path-d)
   ))

;;;
;;; SVG -> PATH
;;;
(defun svg->path (svg)
  (let* ((d (or (plump:attribute svg "d")
                (error 'svg-attr-not-found :attr-name "d")))
         (d (parse-raw-d d)))
    (make-instance 'path
                   :d d)))

;;;
;;; PATH -> SVG
;;;
(defun path->svg (pt svg)
  (let* ((d (path-d pt))
         (d (mapcar
             (lambda (d-el)
               (cond
                 ;; cmds -> just return
                 ((stringp d-el)
                  d-el)
                 ;; coord pair -> to string
                 ((p-p d-el)
                  (format nil "~a ~a" (p-x d-el) (p-y d-el)))

                 (t
                  (error "malformed path d"))
                 ))
             d))
         (d (str:unwords d)))
    (setf (plump:attribute svg "d")
          d)
    svg))

;;;
;;; parse raw path's d attr
;;; return list of Ms Ls (cons x y) Z
;;;
(defun parse-raw-d (raw-d)
  (let* ((d (str:words raw-d)))
    (reverse (reduce
              (lambda (r x)
                (cond
                  ;; move
                  ((string= "M" x)
                   (cons x r))
                  ;; line
                  ((string= "L" x)
                   (cons x r))
                  ;; end
                  ((string= "Z" x)
                   (cons x r))
                  ;; end and move
                  ((string= "ZM" x)
                   (cons x r))
                  ;; we have number
                  (t
                   (cond
                     ;; storing x in r
                     ((stringp (car r))
                      (let* ((x (coerce (parse-number x) 'float)))
                        (cons x r)))
                     ;; x is already in r
                     ;; making coord pair
                     ((numberp (car r))
                      (let* ((y (coerce (parse-number x) 'float))
                             (x (car r))
                             (r (cdr r))
                             (coords (make-p :x x :y y)))
                        (cons coords r)))
                     ;; error
                     (t
                      (error 'svg-malformed-attr
                             :attr-name "path-d"))))))
              d
              :initial-value '()))))

;;;
;;; return path points
;;;
(defun path-points (pth)
  (check-type pth path)

  (let* ((d (path-d pth))
         (d (remove nil (mapcar
                         (lambda (i)
                           (cond
                             ((stringp i)
                              nil)
                             ((p-p i)
                              i)))
                         d))))
    ;; return only points
    d))

;;;
;;;
;;; GEOMETRY FUNCS
;;;
;;;
(defgeneric translate (o vec)
  (:documentation
   "translate object accordign to vector")

  ;;
  ;; translate single point
  ;;
  (:method ((o p) (vec p))
    (make-p :x (+ (p-x o) (p-y vec))
            :y (+ (p-x o) (p-y vec))))
  ;;
  ;; translate polyline
  ;;
  (:method ((o polyline) (vec p))
    (let* ((points (polyline-points o))
           (points (mapcar
                    (lambda (point)
                      (translate point vec))
                    points)))
      ;; modifying source object!
      (setf (access o 'points)
            points)
      ;; return object
      o))

  ;;
  ;; translate path
  ;;
  (:method ((o path) (vec p))
    (let* ((d (path-d o))
           (d (mapcar
               (lambda (d-el)
                 (cond
                   ;; cmd -> do nothing
                   ((stringp d-el)
                    d-el)
                   ;; coords -> translate
                   ((p-p d-el)
                    (translate d-el vec))))
               d)))
      (setf (access o 'd)
            d)
      ;; return object
      o))
  )


;;;
;;;
;;; COORDINATES CONVERSION
;;;
;;; apply scale and transform
;;;
(defgeneric view->diagram (sf
                           o
                           &key
                           &allow-other-keys)
  (:documentation "converts values from view coords to diagram ones")

  ;; number
  (:method ((sf svg-file)
            (o float)
            &key
              ;; do not translate by default
              (translate 0.0)
            &allow-other-keys)

    (let* ((r (/ o (svg-file-scale sf)))
           (r (- r translate))
           )
      r))

  ;; point
  (:method ((sf svg-file)
            (point p)
            &key
            &allow-other-keys)

    (check-type point p)

    (let* ((v-x (p-x point))
           (v-y (p-y point))
           (x (view->diagram
               sf
               v-x
               :translate (svg-file-translate-x sf)))
           (y (view->diagram
               sf
               v-y
               :translate (svg-file-translate-y sf)))

           ;; (x v-x)
           ;; (y v-y)
           )
      (make-p :x x :y y))))

;;;
;;; convert from 0..1 to diagram
;;;
(defun view->diagram* (sf pt)
  (check-type sf svg-file)
  (check-type pt p)

  (view->diagram sf (make-p
                     :x (* (svg-file-width sf)
                           (p-x pt))
                     :y (* (svg-file-height sf)
                        (p-y pt))))
  )

;;;
;;;
;;; NEARBY PREDICATES
;;;
;;;


;;;
;;; NEARBY / TEXT
;;; in doc coordinates
;;;
(defun text-nearby-p (sf
                      el
                      x
                      y
                      &optional
                        (max-distance *max-nearby-distance*))
  (handler-case
      (let* ((el-x (or (plump:attribute el "x")
                       (error 'svg-attr-not-found :attr-name "x")))
             (el-y (or (plump:attribute el "y")
                       (error 'svg-attr-not-found :attr-name "y")))
             (el-x (coerce (parse-number el-x) 'float))
             (el-y (coerce (parse-number el-y) 'float))
             ;; point in diagam coords
             (d-point (view->diagram sf (make-p :x x :y y)))
             (dist (sqrt (+ (expt (- el-x (p-x d-point)) 2)
                            (expt (- el-y (p-y d-point)) 2)))))
        ;; check if inside of given circle
        (< dist max-distance))
    (svg-attr-not-found (e)
      (declare (ignore e))
      ;; if no attr - return nil
      nil)))

;;;
;;; NEARBY / TEXT / FLOAT COORDS
;;; in float 0..1
;;;
;;; float args 0..1
;;; then converted to view width & height of diagram
(defun text-nearby-p* (sf
                       el
                       x
                       y
                       &optional
                         (max-distance *max-nearby-distance*))
  (check-type x float)
  (check-type y float)
  (check-type max-distance float)
  (text-nearby-p sf
                 el
                 (* (svg-file-width sf) x)
                 (* (svg-file-height sf) y)
                 max-distance))



;;;
;;; NEARBY / POLYLINE / FLOAT COORDS
;;; x y max-distance in float 0 .. 1
;;; points format
;;; points="778.884,106.071 776.574,108.987"
(defun polyline-nearby-p* (sf
                           svg-pline
                           x
                           y
                           &optional
                             (max-distance *max-nearby-distance*))
  (check-type x float)
  (check-type y float)

  (let* ((x (* x (svg-file-width sf)))
         (y (* y (svg-file-height sf)))
         (point (make-p :x x :y y))
         (point (view->diagram sf point))
         (pline (svg->polyline svg-pline)))
    ;; true when at least one is nearby
    (some (lambda (pline-point)
            (points-nearby-p point
                             pline-point
                             max-distance))
          (polyline-points pline))))

;;;
;;; NEARBY / PATH / FLOAT COORDS
;;; x y in range 0..1
;;; points in 'd' attr
;;;
(defun path-nearby-p* (sf
                       svg-path
                       x
                       y
                       &optional
                         (max-distance *max-nearby-distance*))
  (check-type x float)
  (check-type y float)

  (let* ((x (* x (svg-file-width sf)))
         (y (* y (svg-file-height sf)))
         (point (make-p :x x :y y))
         (point (view->diagram sf point))
         (pth (svg->path svg-path)))
    (some (lambda (path-d-el)
            (cond
              ((consp path-d-el)
               (points-nearby-p point
                                path-d-el
                                max-distance))
              ;; false for path cmds
              (t
               nil)))
          (path-d pth))))

;;;
;;; NEARBY / UTILS
;;;
(defun points-nearby-p (p1
                        p2
                        &optional
                          (max-distance *max-nearby-distance*))
  (check-type p1 p)
  (check-type p2 p)
  (let* ((dist (sqrt (+ (expt (- (p-x p2) (p-x p1)) 2)
                        (expt (- (p-y p2) (p-y p1)) 2)))))
    (< dist max-distance)))

;;;
;;;
;;; INSIDE PREDICATES
;;;
;;;

;;;
;;; INSIDE / TEXT
;;;
;;; contour coordinates are 0..1
(defun text-inside-p* (sf el contour)
  ;; (declare (optimize (speed 3) (safety 0)))
  (let* ((d-contour (mapcar (lambda (pt)
                              (view->diagram* sf pt))
                            contour))
         (el-x (or (plump:attribute el "x")
                   (error 'svg-attr-not-found :attr-name "x")))
         (el-y (or (plump:attribute el "y")
                   (error 'svg-attr-not-found :attr-name "y")))
         (el-x (coerce (parse-number el-x) 'float))
         (el-y (coerce (parse-number el-y) 'float)))
    (cl-cgal:is-inside d-contour (make-p :x el-x :y el-y))))

;;;
;;; INSIDE / POLYLINE
;;;
;;; contour coordinates are 0..1
(defun polyline-inside-p* (sf svg-pline contour)
  ;; (declare (optimize (speed 3) (safety 0)))
  (let* ((d-contour (mapcar (lambda (pt)
                              (view->diagram* sf pt))
                            contour))
         ;; to polyline object
         (pline (svg->polyline svg-pline)))
    (cl-cgal:intersect-p d-contour (polyline-points pline))
    ;; true when at least one point of pline is inside contour
    ;; (some (lambda (pline-point)
    ;;         (cl-cgal:is-inside d-contour pline-point))
    ;;       (polyline-points pline))
    ))

;;;
;;; INSIDE / PATH
;;;
;;; contour coordinates are 0..1
(defun path-inside-p* (sf svg-path contour)
  ;; (declare (optimize (speed 3) (safety 0)))
  (let* ((d-contour (mapcar (lambda (pt)
                              (view->diagram* sf pt))
                            contour))
         ;; to path object
         (pth (svg->path svg-path)))
    (cl-cgal:intersect-p d-contour (path-points pth))
    ;; ;; at least one point of path is inside
    ;; (some (lambda (path-d-el)
    ;;         (cond
    ;;           ((consp path-d-el)
    ;;            (cl-cgal:is-inside d-contour path-d-el))
    ;;           ;; not relevant to path cmds
    ;;           (t nil)))
    ;;       (path-d p))
    ))


;;;
;;;
;;; COORD UTILS
;;;
;;;

;; ;;;
;; ;;; 0..1 -> diagram coords
;; ;;;
;; (defun f-point->diagram-point (sf p)
;;   (check-type p point-like)
;;   (check-type (point-x p) float)
;;   (check-type (point-y p) float)

;;   (let* ((x (point-x p))
;;          (y (point-y p))
;;          ;; to view coords
;;          (x (* x (svg-file-width sf)))
;;          (y (* y (svg-file-height sf)))
;;          (d-point (view->diagram* sf p)))))

;;;
;;;
;;; TEST FUNCS
;;;
;;;
(defun test-parse-raw-d ()
  (let* ((raw-d "M 534.827 1062.08 L 534.827 1006.62 L 644.359 1006.62 L 644.359 1062.08 Z")
         (d (parse-raw-d raw-d)))
    (format t "~a~%~a" raw-d d)))

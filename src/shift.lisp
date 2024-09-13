(defpackage map-distort-engine.shifter
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


  ;; (:import-from #:cl-cgal
  ;;               #:x
  ;;               #:y)

  (:export
   shift-out-text-position
   shift-out-polyline-position
   shift-out-path-position
   shift-position-nearby-f))

(in-package :map-distort-engine.shifter)

;;;
;;;
;;; LQUERY FUNCTIONS
;;;
;;;

;;;
;;; LQUERY / SHIFT OUT OF CIRCLE / TEXT POSITION
;;;
(defun shift-out-text-position (n center radius)
  (check-type center p)
  (check-type radius float)

  (let* ((x (or (plump:attribute n "x")
                (error 'svg-attr-not-found :attr-name "x")))
         (y (or (plump:attribute n "y")
                (error 'svg-attr-not-found :attr-name "y")))
         (x (parse-number x))
         (y (parse-number y))
         (d (cl-cgal:distance (make-p :x x :y y) center))
         (d (/ radius d))
         (np (cl-cgal:with-transformation
                 tr
                 (cl-cgal:make-translate
                  (make-p :x 0.0 :y 0.0)
                  (make-p :x (* d
                                x )
                          :y (* d y)))
               (cl-cgal:transform tr (make-p :x x :y y))))

         (nx (cl-cgal:x np))
         (ny (cl-cgal:y np))
         )
    (setf (plump:attribute n "x")
          (format nil "~d" nx))
    (setf (plump:attribute n "y")
          (format nil "~d" ny))
    ;; return node
    n))


;;; lquery version
(define-lquery-function shift-out-text-position* (n center radius)
  (shift-out-text-position n center radius))

;; ;;;
;; ;;; LQUERY / SHIFT OUT OF CIRCLE / POLYLINE POSITION
;; ;;;
(defun shift-out-polyline-position (n center radius)

  (let* ((pline (svg->polyline n))
         (pline-points (polyline-points pline))
         (pline-center (cl-cgal:centroid pline-points))
         (d (cl-cgal:distance center pline-center))

         (d (/ radius d))
         (translate-vec (make-p :x (* d
                                      (- (cl-cgal:x pline-center)
                                         (cl-cgal:x center)))
                                :y (* d
                                      (- (cl-cgal:y pline-center)
                                         (cl-cgal:y center)))))
         )
    (translate pline translate-vec)
    (polyline->svg pline n)))

;;; lquery version
(define-lquery-function shift-out-polyline-position* (n center radius)
  (shift-out-polyline-position n center radius))

;; ;;;
;; ;;; LQUERY / SHIFT OUT OF CIRCLE / PATH POSITION
;; ;;;
(defun shift-out-path-position (n center radius)
  (let* ((pth (svg->path n))
         (p-points (path-points pth))
         (p-center (cl-cgal:centroid p-points))
         (d (cl-cgal:distance center p-center))
         (d (/ radius d))
         (translate-vec (make-p :x (* d
                                      (- (cl-cgal:x p-center)
                                         (cl-cgal:x center)))
                                :y (* d
                                      (- (cl-cgal:y p-center)
                                         (cl-cgal:y center)))))


         )
    (translate pth translate-vec)

    (path->svg pth n)))

;;; lquery version
(define-lquery-function shift-out-path-position* (n center radius)
  (shift-out-polyline-position n center radius))


;;;
;;; shift position outside of circle
;;; coords are in float 0..1
;;; radius in view coords
;;;
(defun shift-positions-out-of-circle-f (sf x y r)
  (check-type x float)
  (check-type y float)
  (check-type r float)

  (log:info "shifting out of circle")
  (let* ((d (xml-doc sf)))
    (lquery:$ d
      ;; texts
      "text"
      (filter #'(lambda (el)
                  (text-nearby-p* sf
                                  el
                                  x
                                  y
                                  r)))
      (shift-out-text-position* (view->diagram*
                                sf
                                (make-p :x x :y y)) r)
      (root)
      ;; polylines
      "polyline"
      (filter #'(lambda (el)
                  (polyline-nearby-p* sf
                                      el
                                      x
                                      y
                                      r)))
      (shift-out-polyline-position* (view->diagram*
                                    sf
                                    (make-p :x x :y y)) r)
      (root)
      ;; pathes
      "path"
      (filter #'(lambda (el)
                  (path-nearby-p* sf
                                  el
                                  x
                                  y
                                  r)))
      (shift-out-path-position* (view->diagram*
                                sf
                                (make-p :x x :y y)) r)
      (root)

      )))


;;;
;;;
;;; test shift
;;;
;;;
;; (defun test-shift-oot-of-circle ()
;;   (let* ((sf (mk-svg-file "data/test_map.svg")))
;;     (shift-positions-out-of-circle-f sf
;;                              0.5
;;                              0.5
;;                              100.0)
;;     (save-svg-to-file sf "data/test_map_out.svg")))

(defun test-shift-oot-of-circle ()
  (loop
    do
    (progn
      (sleep 1.5)
      (let* ((sf (mk-svg-file "data/test_map.svg"))
             (x (random 1.0))
             (y (random 1.0))
             (radius 200.0))
        (map-distort-engine.shaker::shake-position-nearby-f
         sf
         x
         y
         radius)
        (shift-positions-out-of-circle-f sf
                                         x
                                         y
                                         radius)
        (save-svg-to-file sf "data/test_map_out.svg")))))

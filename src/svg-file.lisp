(defpackage map-distort-engine.svg-file
  (:use :cl
        :alexandria
        :access
        :iterate
        :lquery
        :parse-number)
  (:export
   mk-svg-file
   ;; select-text-nearby-f
   xml-doc
   el-nearby-p*
   el-nearby-p
   save-svg-to-file)
  )

(in-package :map-distort-engine.svg-file)

;;;
;;; SVG FILE CLASS
;;;
(defclass svg-file ()
  ((path  :initarg :path
          :initform nil
          :accessor path)
   ;; underlying xml
   (xml-doc
    :initarg :xml-doc
    :initform nil
    :accessor xml-doc)

   (width
    :initarg :width
    :accessor svg-file-width)

   (height
    :initarg :height
    :accessor svg-file-height)

   (scale
    :initarg :scale
    :initform 1.0
    :accessor svg-file-scale)

   (translate-x
    :initarg :translate-x
    :initform 0
    :accessor svg-file-translate-x)

   (translate-y
    :initarg :translate-y
    :initform 0
    :accessor svg-file-translate-y)))

;;;
;;; factory method
;;;
(defun mk-svg-file (path)
  (when (not (uiop:file-exists-p path))
    (log:error "file does not exist: ~a" path)
    (error 'file-does-not-exist))

  (let* ((abs-path (truename path))
         (d (lquery:$ (initialize abs-path)))
         (dims (get-svg-dims d))
         (width (car dims))
         (height (cdr dims))
         (f (make-instance 'svg-file
                           :path abs-path
                           :xml-doc d
                           :width width
                           :height height
                           ;; there are no scale and translate in map files
                           :scale 1
                           :translate-x 0
                           :translate-y 0)))
    (fix-svg f)
    f))

;;;
;;;
;;; COORDINATES CONVERSION
;;;
;;;
(defun view->diagram (sf point)
  (check-type point cons)

  (let* ((v-x (car point))
         (v-y (cdr point))
         (x v-x)
         (y v-y)
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
;;; in doc coordinates
;;;
(defun el-nearby-p (sf el x y)
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
    ;; TODO: move to parameter
    (< dist 100)))

;;;
;;; in float 0..1
;;;
;;; float args 0..1
;;; then converted to vide width & height of diagram
(defun el-nearby-p* (sf el x y)
  (check-type x float)
  (check-type y float)
  (el-nearby-p sf
               el
               (* (svg-file-width sf) x)
               (* (svg-file-height sf) y)))



;;;
;;;
;;; svg utils
;;;
;;;

;;;
;;; get dims
;;;
(defun get-svg-dims (doc)
  (let* ((width (or (lquery:$1 doc
                      "svg"
                      (attr "width")
                      )
                    (error 'svg-attr-not-found "width")))
         (height (or (lquery:$1 doc
                       "svg"
                       (attr "height"))
                     (error 'svg-attr-not-found "height")))
         (width (ppcre:regex-replace "pt" width ""))
         (height (ppcre:regex-replace "pt" height ""))
         (width (parse-integer width))
         (height (parse-integer height)))
    (cons width height)))

;; ;;;
;; ;;; patch svg attrs
;; ;;;
;; (defun fix-svg (an-svg-file)
;;   (log:info "fixing svg file")
;;   (let ((d (xml-doc an-svg-file)))
;;     (lquery:$1 d
;;       "svg"
;;       (fix-svg-viewbox* an-svg-file)
;;       (root)
;;       "#graph0"
;;       (fix-svg-transform* an-svg-file))

;;     ))


(defun save-svg-to-file (an-svg-file path)
  (log:info "writing svg data to file ~a" path)
  (let* ((d (xml-doc an-svg-file))
         (content
           (with-output-to-string (out)
             (lquery:$ d
               (serialize out :XML))))
         (content (ppcre:regex-replace-all "<!--.+-->" content "")))
    (with-output-to-file (out path :if-exists :supersede)
      (format out "~a" content))))


;;;
;;;
;;; TEST FUNCS
;;;
;;;

(defun test-svg-file ()
  (let* ((sf (mk-svg-file "data/generated.svg")))
    (format t "~a" (get-svg-dims (xml-doc sf)))
    (save-svg-to-file sf "data/patched.svg")
    ;; (xml-doc sf)
    ))

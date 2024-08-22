(defpackage map-distort-engine.svg-file
  (:use :cl
        :alexandria
        :access
        :iterate
        :lquery
        :parse-number)
  (:export
   mk-svg-file
   svg-file
   svg-file-scale
   svg-file-height
   svg-file-width
   svg-file-translate-x
   svg-file-translate-y
   ;; select-text-nearby-f
   xml-doc
   el-nearby-p*
   el-nearby-p
   save-svg-to-file)
  )

(in-package :map-distort-engine.svg-file)

;;;
;;;
;;; CONDITIONS
;;;
;;;
(define-condition svg-file-does-not-exist (file-error)
  ())

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
    (error 'svg-file-does-not-exist))

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
    ;; (fix-svg f)
    f))


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
  (let* ((sf (mk-svg-file "data/test_map.svg")))
    (format t "~a" (get-svg-dims (xml-doc sf)))
    (save-svg-to-file sf "data/test_map_out.svg")
    ;; (xml-doc sf)
    ))

(defpackage map-distort-engine.svg-file
  (:use :cl
        :alexandria
        :access
        :iterate
        :lquery
        :parse-number)
  (:export
   mk-svg-file
   clone-svg-file
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
(declaim (ftype (function (svg-file)
                          float)
                svg-file-scale)
         (ftype (function (svg-file)
                          float)
                svg-file-width)
         (ftype (function (svg-file)
                          float)
                svg-file-height)
         (ftype (function (svg-file)
                          float)
                svg-file-translate-x)
         (ftype (function (svg-file)
                          float)
                svg-file-translate-y))

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
    :type float
    :accessor svg-file-width)

   (height
    :initarg :height
    :type float
    :accessor svg-file-height)

   (scale
    :initarg :scale
    :type float
    :initform 1.0
    :accessor svg-file-scale)

   (translate-x
    :type float
    :initarg :translate-x
    :initform 0.0
    :accessor svg-file-translate-x)

   (translate-y
    :type float
    :initarg :translate-y
    :initform 0.0
    :accessor svg-file-translate-y)))


;;;
;;; factory method
;;;
(defun mk-svg-file (path)
  (when (not (uiop:file-exists-p path))
    (log:error "file does not exist: ~a" path)
    (error 'svg-file-does-not-exist))

  (let* ((abs-path (truename path))
         (raw (uiop:read-file-string abs-path))
         (raw (ppcre:regex-replace-all "<!--.+-->" raw ""))
         (d (lquery:$ (initialize raw)))
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
;;; factory / clone svg file
;;; without rereading xml data from disk
;;;
(defun clone-svg-file (sf)
  (let* ((d (plump:clone-node (xml-doc sf) t)))
    (make-instance 'svg-file
                   :path (access sf 'path)
                   :xml-doc d
                   :width (access sf 'width)
                   :height (access sf 'height)
                   :scale (access sf 'scale)
                   :translate-x (access sf 'translate-x)
                   :translate-y (access sf 'translate-y))))


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
  (let* ((d (xml-doc an-svg-file)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (lquery:$ d
               (serialize out :XML)))))


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

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

(define-condition  svg-file-error (simple-error)
  ())

(define-condition svg-file-does-not-exist (file-error svg-file-error)
  ())

(define-condition  svg-attr-not-found (svg-file-error)
  ((attr-name :initarg :attr-name :reader attr-name))

  (:report (lambda (condition stream)
             (format stream
                     "missing svg attr '~a'"
                     (access:access condition 'attr-name)))))

(define-condition  svg-wrong-attr-value (svg-file-error)
  ((attr-name :initarg :attr-name)
   (value :initarg  :value))

  (:report (lambda (condition stream)
             (format stream
                     "wrong svg attr value '~a': ~a"
                     (access:access condition 'attr-name)
                     (access:access condition 'value)))))

(define-condition svg-parse-error (svg-file-error)
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

;;; parse string and return
(defun parse-view-box (doc)
  "parse string and return width and height"

  (handler-case
      (let* ((view-box-str (or (lquery:$1 doc
                                 "svg"
                                 (attr "viewBox"))
                               (error 'svg-parse-error)))
             (vals (or (mapcar #'parse-integer
                               (split-sequence:split-sequence #\Space
                                                              view-box-str))
                       (error  'svg-parse-error))))

        (unless (and (= 0 (nth 0 vals))
                     (= 0 (nth 1 vals)))
          (error 'svg-wrong-attr-value
                 :attr-name "viewBox"
                 :value view-box-str))
        ;; return width and height
        (list (nth 2 vals) (nth 3 vals)))
    ;; return '(nil nil) in case of error
    (svg-parse-error (e)
      '(nil nil)))
  )

;;;
;;; parse width from svg file
;;;
(defun parse-width (doc)
  "integer width or nil"
  (handler-case (let* ((width (or (lquery:$1 doc
                                    "svg"
                                    (attr "width"))
                                  (error 'svg-parse-error)))
                       (width (ppcre:regex-replace "pt" width ""))
                       (width (parse-integer width)))
                  width)
    (svg-parse-error (e)
      ;; return nil
      nil)))

;;;
;;; parse height from svg file
;;;
(defun parse-heigth (doc)
  "integer height or nil"

  (handler-case (let* ((height (or (lquery:$1 doc
                                     "svg"
                                     (attr "height"))
                                   (error 'svg-parse-error)))
                       (height (ppcre:regex-replace "pt" height ""))
                       (height (parse-integer height))
)
                  height)
    (svg-parse-error (e)
      ;; retunr nil
      nil)))

;;;
;;; get dims
;;;
(defun get-svg-dims (doc)
  (let* ((view-box-width-height (parse-view-box doc))
         (width (or (parse-width doc)
                    (nth 0 view-box-width-height)
                    (error 'svg-attr-not-found :attr-name "width or viewBox:widht")))
         (height (or (parse-heigth doc)
                     (nth 1 view-box-width-height)
                     (error 'svg-attr-not-found :attr-name "height or viewBox:height"))))
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

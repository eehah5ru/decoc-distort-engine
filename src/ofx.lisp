(defpackage map-distort-engine.ofx
  (:use
   :cl
   :alexandria
   :access
   :iterate
   :parse-number
   :osc
   :usocket
   ;; :lparallel
   :map-distort-engine.shaker
   :map-distort-engine.svg-file
   :map-distort-engine.contours-keyer
   )

  (:import-from :cl-cgal
                #:point-x
                #:point-y
                #:point-like
                #:p
                #:p-p
                #:p-x
                #:p-y
                #:make-p))

(in-package :map-distort-engine.ofx)


;;;
;;;
;;; PARAMETERS
;;;
;;;

(defvar *profiling-ofx* nil)

(defparameter *in-file-path*
  (asdf:system-relative-pathname
   :map-distort-engine
   "data/diagrams/map.svg"))

(defparameter *out-file-path*
  (asdf:system-relative-pathname
   :map-distort-engine
   "data/diagrams/map_patched.svg"))

(defvar *svg-file* nil)

(defparameter *mouse-x* 0)

(defparameter *mouse-y* 0)

;;; focus poosition
;;;
(declaim (type (or null p) *focus-point*))
(defparameter *focus-point* nil)

;;; focus radius
;;; float
(declaim (type (or null float) *focus-radius*))
(defparameter *focus-radius* nil)


;;; list of current contours
(defparameter *contours* (list))

;;;
;;; out socket for osc messages
;;;
(defparameter *osc-out* nil)

;;;
;;;
;;; CONDITIONS
;;;
;;;

(define-condition unknown-ofx-cmd (error)
  ())

;;;
;;; profiling helper
;;;
;;; accurate version
;;; TODO: need to have functions to profile
(defmacro with-accurate-profiling-ofx (&body body)
  `(if *profiling-ofx*
       (unwind-protect
            (progn
              (sb-profile:reset)
              (sb-profile:profile map-distort-engine.svg-map:text-nearby-p*
                                  map-distort-engine.svg-map:polyline-nearby-p*
                                  map-distort-engine.svg-map:path-nearby-p*
                                  map-distort-engine.shaker:randomise-text-position*
                                  map-distort-engine.shaker:randomise-polyline-position*
                                  map-distort-engine.shaker:randomise-path-position*
                                  map-distort-engine.shifter:shift-out-text-position
                                  map-distort-engine.shifter:shift-out-polyline-position
                                  map-distort-engine.shifter:shift-out-path-position
                                  )
              ,@body)
         (sb-profile:report))
       (progn
         ,@body)))

;; flamegraph version
(defmacro with-profiling-ofx (out-file &body body)
  `(if *profiling-ofx*
       (let ((sb-sprof:*max-samples* 50000)
             (sb-sprof:*sample-interval* 0.0001))
           (flamegraph:save-flame-graph
               (,out-file
                :mode :time
                :threads :all)
             ,@body))
       (progn
         ,@body)))

;;;
;;;
;;; OSC COMMAND HANDLERS
;;;
;;;

;;;
;;; CMD / CONTOURS
;;;
(defun cmd-contours (msg)
  (declare (ignore msg))
  ;; gonna get new contours. empty current ones
  (setf *contours* (list))
  ;; (log:info "args are: ~a" (osc-args msg))
  )


;;;
;;; CMD / CONTOUR
;;; args are:
;;; - size
;;; - point x
;;; - point y
;;; - point x
;;; - pooint y
;;; - ...
;;;
;;; coords are in float 0..1
(defun cmd-contour (msg)
  (let* ((data (osc-args msg))
         ;; (size (car data))
         (raw-points (cdr data))
         (points (raw-points->points raw-points)))
    ;; (log:info "countour size: ~a" size)
    ;; (log:info "contour points: ~a" raw-points)

    (push points *contours*)
    ;; (setf *contours* (list (list (cons 0.25 0.25) (cons 0.75 0.25) (cons 0.75 0.75) (cons 0.25 0.75))
    ;;                        (list (cons 0.8 0.8) (cons 0.8 1.0) (cons 1.0 1.0) (cons 1.0 0.8))
    ;;                        (list (cons 0.0 0.0) (cons 0.0 0.2) (cons 0.2 0.2) (cons 0.2 0.0))))
    ))

;;;
;;; CMD / UPDATE MOUSE
;;;
(defun cmd-update-mouse (msg)
  (log:info "updating mouse")
  (let* ((point (osc-args msg))
         (x (car point))
         (y (cadr point)))
    (update-mouse x y)))

;;;
;;; CMD / UPDATE MOUSE
;;;
(defun cmd-update-focus (msg)
  (log:info "updating focus")
  (let* ((focus (osc-args msg))
         (x (car focus))
         (y (cadr focus))
         (radius (caddr focus)))
    (update-focus x y radius)))

;;;
;;; CMD / SHAKE POSITIONS
;;;
(defun cmd-shake-positions-in-contours (msg)
  (declare (ignore msg))
  ;; (log:info "shaking positions around mouse point: ~d ~d"
  ;;           *mouse-x*
  ;;           *mouse-y*)

  (with-profiling-ofx "tmp/cmd-shake-positions.stack"
    (time
     (patch-svg
      (lambda (sf)
        ;; (shake-position-nearby-f sf *mouse-x* *mouse-y* 200.0)
        ;; (map-distort-engine.shifter::shift-positions-out-of-circle-f
        ;;  sf
        ;;  *mouse-x*
        ;;  *mouse-y*
        ;;  200.0)

        (map-distort-engine.contours-keyer::shake-positions-inside-contours sf *contours*)
        )
      :on-patched (lambda ()
                    (send-osc-map-updated)))))
  ;; (sb-sprof:report :type :flat))
  )

;;;
;;; CMD / SHAKE POSITIONS
;;;
(defun cmd-shake-positions-near-focus (msg)
  (declare (ignore msg))
  (log:info "shaking positions near focus point: ~d ~d, radius ~d"
            (cl-cgal:x *focus-point*)
            (cl-cgal:y *focus-point*)
            *focus-radius*)

  (with-profiling-ofx "tmp/cmd-shake-positions-near-focus.stack"
    (time
     (patch-svg
      (lambda (sf)
        ;; (shake-position-nearby-f sf *mouse-x* *mouse-y* 200.0)
        ;; (map-distort-engine.shifter::shift-positions-out-of-circle-f
        ;;  sf
        ;;  *mouse-x*
        ;;  *mouse-y*
        ;;  200.0)

        (map-distort-engine.focus-keyer::shake-positions-near-focus sf *focus-point* *focus-radius*)
        )
      :on-patched (lambda ()
                    (send-osc-map-updated)))))
  )


;;;
;;;
;;; OSC ROUTER
;;;
;;;
(defun route-osc-message (msg)
  (let* ((cmd (osc-command msg)))
    (cond
      ;;
      ;; new contours set
      ;;
      ((string= "/contours" cmd)
       (cmd-contours msg))
      ;;
      ;; single contour data
      ;;
      ((string= "/contour" cmd)
       (cmd-contour msg))

      ;; mouse
      ((string= "/mouse/position" (osc-command msg))
       (cmd-update-mouse msg))

      ;; focus point with radius
      ((string= "/focus/position" (osc-command msg))
       (cmd-update-focus msg))

      ;; shake position in contours
      ((string= "/shake/positions/in-contours" (osc-command msg))
       (cmd-shake-positions-in-contours msg))

      ;; shake position near focus
      ((string= "/shake/positions/near-focus" (osc-command msg))
       (cmd-shake-positions-near-focus msg))


      (t
       (log:error "unknown osc cmd: ~a" cmd)
       (error 'unknown-ofx-cmd)))))

;;;
;;;
;;; MAIN LOOP
;;;
;;;
(defun ofx-shaker ()
  (log:info "starting shaker")
  (let* ((*svg-file* (mk-svg-file *in-file-path*))
         (lparallel:*kernel* (lparallel:make-kernel 32))
         (*profiling-ofx* nil)
         (port 12345)
         (send-port 12346)
         (s (socket-connect nil nil
                            :local-port port
                            :local-host #(127 0 0 1)
                            :protocol :datagram
                            :element-type '(unsigned-byte 8)))
         (buffer (make-sequence
                  '(vector (unsigned-byte 8))
                  ;; length of buffer
                  4096)))

    (unwind-protect
         (progn
           (setf *osc-out* (socket-connect #(127 0 0 1) send-port
                                           :protocol :datagram
                                           :element-type '(unsigned-byte 8)))
           (loop do
                 (socket-receive s buffer (length buffer))
                 (let* ((m (osc:decode-bundle buffer)))
                   ;; (format t "received -=> ~S~%" m)
                   ;; (format t "addr: ~a~%" (osc-command m))
                   ;; (format t "args: ~a~%" (osc-args m))

                   (route-osc-message m)
                   )))
      ;; cleanup
      (progn
        (when s
          (socket-close s))
        (when *osc-out*
          (socket-close *osc-out*))))))

;;;
;;;
;;; ofx local simulation without OSC
;;;
;;;
(defun ofx-shaker-sim ()
  (log:info "ofx-shaker-sim: starting")

  (let*
      ((*svg-file* (mk-svg-file *in-file-path*))
       (lparallel:*kernel* (lparallel:make-kernel 32))
       (*profiling-ofx* t))
    (loop do
          (with-accurate-profiling-ofx
            (let* ((x (random 1.0))
                   (y (random 1.0))
                   (radius 0.2))
              (update-focus x y radius)
              (patch-svg
               (lambda (sf)
                 (time (map-distort-engine.focus-keyer::shake-positions-near-focus sf *focus-point* *focus-radius*)))))))))

;;;
;;;
;;; UTILS
;;;
;;;

;;;
;;; update mouse position
;;;
;;; mouse position is float in 0..1
(defun update-mouse (x y)
  (check-type x float)
  (check-type y float)

  (setf *mouse-x* x)
  (setf *mouse-y* y))

;;;
;;; update focus params
;;;
;;; focus  position and radius is float in 0..1
(defun update-focus (x y radius)
  (check-type x float)
  (check-type y float)
  (check-type radius float)

  (setf *focus-point* (make-p :x x :y y))
  (setf *focus-radius* radius))

;;;
;;;
;;;

;;;
;;; patch svg file with patcher-func
;;;
(defun patch-svg (patcher-func &key (on-patched nil))
  (let* ((sf (clone-svg-file *svg-file*)))
    (apply patcher-func (list sf))
    (save-svg-to-file sf *out-file-path*)
    (when on-patched
      (apply on-patched ()))))


;;;
;;;
;;; send osc mapUpdated message
;;;
;;;
(defun send-osc-map-updated ()
  (when (not *osc-out*)
    (error "*osc-out* is not open"))
  (let ((updated-msg (osc:encode-message
                       "/mapUpdated"
                       '())))
    (socket-send *osc-out* updated-msg (length updated-msg))))

;;;
;;;
;;; UTILS
;;;
;;;
(defun osc-command (msg)
  (check-type msg cons)
  (car msg))

(defun osc-args (msg)
  (check-type msg cons)
  (cdr msg))


;;; return list of (x . y)
(defun raw-points->points (raw-points)
  (labels ((fn (points rest)
           (cond
             ;; nothing to process
             ((emptyp rest)
              (cons points nil))
             ;; error - only one el. need a pair of x and y
             ((= 1 (length rest))
              (error "not enought data to build a point!"))
             (t
              (let* ((x (car rest))
                     (y (cadr rest))
                     (rest (cddr rest))
                     (point (make-p :x x :y y))
                     (points (cons point points)))
                (fn points rest))))))

    (car (fn '() raw-points))))

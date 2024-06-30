(defpackage map-distort-engine.ofx
  (:use :cl
        :alexandria
        :access
        :iterate
        :parse-number
        :osc
        :usocket

        :map-distort-engine.shaker
        :map-distort-engine.svg-file
        ))

(in-package :map-distort-engine.ofx)


;;;
;;;
;;; PARAMETERS
;;;
;;;
(defparameter *in-file-path* "data/diagrams/map.svg")

(defparameter *out-file-path* "data/diagrams/map_patched.svg")

(defparameter *mouse-x* 0)

(defparameter *mouse-y* 0)


;;;
;;;
;;; CONDITIONS
;;;
;;;

(define-condition unknown-ofx-cmd (error)
  ())


;;;
;;;
;;; OSC COMMAND HANDLERS
;;;
;;;

;;;
;;; CMD / CONTOURS
;;;
(defun cmd-contours (msg)
  (log:info "args are: ~a" (osc:args msg)))


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
(defun cmd-contour (msg)
  (let* ((data (osc:args msg))
         (size (car data))
         (raw-points (cdr data)))
    (log:info "countour size: ~a" size)
    (log:info "contour points: ~a" raw-points)))

;;;
;;; CMD / UPDATE MOUSE
;;;
(defun cmd-update-mouse (msg)
  (log:info "updating mouse")
  (let* ((point (osc:args msg))
         (x (car point))
         (y (cadr point)))
    (update-mouse x y)))

;;;
;;; CMD / SHAKE POSITIONS
;;;
(defun cmd-shake-positions (msg)
  (log:info "shaking positions around mouse point: ~d ~d"
            *mouse-x*
            *mouse-y*)

  (patch-svg
   (lambda (sf)
     (shake-position-nearby-f sf *mouse-x* *mouse-y*))))


;;;
;;;
;;; OSC ROUTER
;;;
;;;
(defun route-osc-message (msg)
  (let* ((cmd (osc:command msg)))
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
      ((string= "/mouse/position" (osc:command msg))
       (cmd-update-mouse msg))

      ;; shake position around mouse
      ((string= "/shake/positions" (osc:command msg))
       (cmd-shake-positions msg))

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
  (let* ((port 12345)
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
         (loop do
               (socket-receive s buffer (length buffer))
               (let* ((m (osc:decode-bundle buffer)))
                 ;; (format t "received -=> ~S~%" m)
                 ;; (format t "addr: ~a~%" (osc:command m))
                 ;; (format t "args: ~a~%" (osc:args m))

                 (route-osc-message m)
                 ))
      (when s
        (socket-close s)))))


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
;;; patch svg file with patcher-func
;;;
(defun patch-svg (patcher-func)
  (let* ((sf (mk-svg-file *in-file-path*)))
    (apply patcher-func (list sf))
    (save-svg-to-file sf *out-file-path*)))

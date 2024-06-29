(defpackage map-distort-engine.ofx
  (:use :cl
        :alexandria
        :access
        :iterate
        :parse-number
        :osc
        :usocket
        ))

(in-package :map-distort-engine.ofx)

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

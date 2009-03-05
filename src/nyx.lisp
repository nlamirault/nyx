;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          nyx.lisp
;;;; Purpose:       Nyx main entry
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of nyx, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; nyx users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(in-package :nyx)



(defun process-new-window (win)
  "Apply events mask for a new window."
  (setf (xlib:window-event-mask win) '(:structure-notify
                                       :property-change
                                       :colormap-change
                                       :focus-change)))



(defun handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (with-xlib-protect
    (when *debug*
      (format *standard-output*
              "~%Event key : ~A" event-key))
    (force-output)
;;;     (xlib:event-case (display :discard-p t :force-output-p t)
;;; 		   (:configure-request 
;;; 		    (window x y width height border-width value-mask)
;;; 		    (print "Configure request")
;;; 		    (handle-configure-request window x y width height border-width value-mask)
;;; 		    nil)
;;; 		   (:map-request 
;;; 		    (window)
;;; 		    (print "Map request")
;;; 		    (handle-map-request window)
;;; 		    nil)
;;; 		   (:map-notify
;;; 		    (window event-window)
;;; 		    (unless (eql event-window window)
;;; 		      (print "map notify")
;;; 		      (handle-map-notify window))
;;; 		    nil)
;;; 		   (:unmap-notify
;;; 		    (window event-window send-event-p)
;;; 		    (unless (or send-event-p
;;; 				(xlib:window-equal window event-window))
;;; 		      (print "Unmap notify")
;;; 		      ;; There are two kinds of unmap notify events:
;;; 		      ;; the straight up ones where event-window and
;;; 		      ;; window are the same, and substructure unmap
;;; 		      ;; events when the event-window is the parent
;;; 		      ;; of window. Since the parent of all stumpwm
;;; 		      ;; windows is the root window, use it to find
;;; 		      ;; the screen.
;;; 		      (handle-unmap-notify (find-screen event-window) window))
;;; 		    nil)
;;; 		   (:create-notify
;;; 		    (window x y width height border-width override-redirect-p)
;;; 		    (print "Create notify")
;;; 		    (handle-create-notify window x y width height border-width override-redirect-p)
;;; 		    nil)
;;; 		   (:destroy-notify
;;; 		    (event-window window send-event-p)
;;; 		    (unless (or send-event-p
;;; 				(xlib:window-equal event-window window))
;;; 		      ;; Ignore structure destroy notifies and only
;;; 		      ;; use substructure destroy notifiers. This way
;;; 		      ;; event-window is the root window.
;;; 		      (print "Destroy notify")
;;; 		      (handle-destroy-notify (find-screen event-window) window))
;;; 		    nil)
;;; 		   (:key-press
;;; 		    (root window state code)
;;; 		    (print "Key press event")
;;; 		    (print window)
;;; 		    (print state)
;;; 		    (print code)
;;; 		    (print "---")
;;; 		    (handle-key-press (find-screen root) window code state)
;;; 		    nil))

    (case event-key
      (:button-press (apply #'handle-button-press event-slots))
      (:button-release (apply #'handle-button-release event-slots))
      (:key-press (apply #'handle-key-press event-slots))
      (:key-release (apply #'handle-key-release event-slots))
      (:configure-request (apply #'handle-configure-request event-slots))
      (:configure-notify (apply #'handle-configure-notify event-slots))
      (:create-notify (apply #'handle-create-notify event-slots))
      (:destroy-notify (apply #'handle-destroy-notify event-slots))
      (:property-notify (apply #'handle-property-notify event-slots))
      (:motion-notify (apply #'handle-motion-notify event-slots))
      (:map-notify t)
      (:unmap-notify t)
      (:map-request (apply #'handle-map-request event-slots))
      (:unmap-request (apply #'handle-unmap-request event-slots))
      ;;(:exposure t)
      (t))))
        

;; (defun start-wm-old (&key (host "127.0.0.1") (display 0) threaded-p)
;;   (setf *display* (xlib:open-display host :display display :protocol nil))
;;   (let* ((dpy *display*)
;;          (screen (xlib:display-default-screen dpy))
;;          (root (xlib:screen-root screen))
;;          )
;;     (setf (xlib:window-event-mask root)
;;           (xlib:make-event-mask :button-press
;;                                 :button-release
;;                                 :key-press
;;                                 :key-release)
;;           (xlib:window-border root) 5)
;;     (create-x-window dpy screen (random 800) (random 800) 100 100)
;;     (if threaded-p
;;         (setf *wm-thread*
;;               (sb-thread:make-thread 
;;                (lambda ()
;;                  (loop
;;                    (with-xlib-protect
;;                      (xlib:display-finish-output *display*)
;;                      (xlib:process-event *display* :handler #'handle-event))))
;;                :name "Nyx - Window Manager"))
;;         (loop
;;           (with-xlib-protect
;;             (xlib:display-finish-output *display*)
;;             (xlib:process-event *display* :handler #'handle-event))))))


(defun start-wm (&key (host "127.0.0.1") (display 0) threaded-p)
  (declare (ignore threaded-p))
  (setf *display* (xlib:open-display host :display display :protocol nil))
  (unwind-protect
      (progn
        (setf *screens* (mapcar #'init-screen (xlib:display-roots *display*)))
        ;;(mapcar #'process-existing-windows *screens*)
        (create-x-window *display* 
                         (screen-number (first *screens*))
                         (random 800) (random 800) 100 100)
        (xlib:display-finish-output *display*)
        (loop
          (xlib:process-event *display* :handler #'handle-event)))
    (xlib:close-display *display*)))


(defun stop-wm ()
  "Close the X display."
  (when (and (not (null *wm-thread*))
             (sb-thread:thread-alive-p *wm-thread*))
    (sb-thread:terminate-thread *wm-thread*))
  (unless (null *display*)
    (xlib:close-display *display*)
    (setf *display* nil)))

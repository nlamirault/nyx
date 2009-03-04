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



(defun handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (with-xlib-protect
    (format *standard-output*
            "~%Event key : ~A~%" event-key)
    (force-output)
    (case event-key
      (:button-press (apply #'handle-button-press event-slots))
      (:button-release (apply #'handle-button-release event-slots))
      (:key-press (apply #'handle-key-press event-slots))
      (:create-notify (apply #'handle-create-notify event-slots))
      (t))))
        

(defun start-wm (&key (host "127.0.0.1") (display 0) threaded-p)
  (setf *display* (xlib:open-display host :display display :protocol nil))
  (let* ((dpy *display*)
         (screen (xlib:display-default-screen dpy))
         (root (xlib:screen-root screen))
         )
    (setf (xlib:window-event-mask root)
          (xlib:make-event-mask :button-press
                                :button-release
                                :key-press
                                :key-release)
          (xlib:window-border root) 5)
    (create-x-window dpy screen (random 800) (random 800) 100 100)
    (if threaded-p
        (setf *wm-thread*
              (sb-thread:make-thread 
               (lambda ()
                 (loop
                   (with-xlib-protect
                     (xlib:display-finish-output *display*)
                     (xlib:process-event *display* :handler #'handle-event))))
               :name "Nyx - Window Manager"))
        (loop
          (with-xlib-protect
            (xlib:display-finish-output *display*)
            (xlib:process-event *display* :handler #'handle-event))))))


(defun stop-wm ()
  "Close the X display."
  (when (and (not (null *wm-thread*))
             (sb-thread:thread-alive-p *wm-thread*))
    (sb-thread:terminate-thread *wm-thread*))
  (unless (null *display*)
    (xlib:close-display *display*)))

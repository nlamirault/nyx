;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools.lisp
;;;; Purpose:       Nyx tools.
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


;; Stolen from stumpwm
#+sbcl
(defun run-shell-command (command args &key (output *standard-output*)
                          (wait t))
  "Execute COMMAND using ARGS."
  (format t "~%Execute : ~A ~A~%" command args)
  (sb-ext:process-exit-code
   (sb-ext:run-program 
    command args 
    :output output :error t :wait wait
    ;; inject the DISPLAY variable in so programs show up
    ;; on the right screen.
    :environment (list (format nil "DISPLAY=:~A"
                                    (xlib:display-display *display*))))))



;; Stolen from clfswm
(defmacro with-xlib-protect (&body body)
  "Prevent Xlib errors"
  `(handler-case
       (progn
         ,@body)
     ((or xlib:match-error xlib:window-error xlib:drawable-error) (c)
       (declare (ignore c)))))


(defun create-x-window (display screen x y width height)
  (let* ((black (xlib:screen-black-pixel screen))
         (white (xlib:screen-white-pixel screen))
         (w (xlib:create-window :parent (xlib:screen-root screen)
                                :background black
                                :border white
                                :event-mask '(:structure-notify
                                              :property-change
                                              :colormap-change
                                              :focus-change
                                              :pointer-motion
                                              :button-press
                                              :button-release
                                              :key-press
                                              :key-release
                                              :exposure
                                              )
;;;                                 '(:key-press
;;;                                               :key-release
;;;                                               :button-press
;;;                                               :button-release
;;;                                               :exposure 
;;;                                               :pointer-motion)
                                :x x :y y :width width :height height
                                :border-width 1))
;;;          (gc (xlib:create-gcontext :drawable (xlib:screen-root screen)
;;;                                    :foreground white
;;;                                    :background black)))
         )
    (xlib:map-window w)
    (xlib:display-finish-output display)
    (sleep 1)
    (setf (xlib:window-priority w) :above)
    ;;(xlib:destroy-window w)
    (xlib:display-finish-output display)
    w))
    


;; Stolen from Eclipse
(defun xwin-send-configuration-notify (xwin x y w h bw)
  "Send a synthetic configure notify event to the given window (ICCCM 4.1.5)"
  (xlib:send-event xwin :configure-notify nil
                   :event-window xwin
                   :window xwin
                   :x x :y y
                   :width w
                   :height h
                   :border-width bw
                   :propagate-p nil))

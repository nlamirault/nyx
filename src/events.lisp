;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          events.lisp
;;;; Purpose:       Nyx XLib events management
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


(defun handle-button-press (&rest event-slots
                            &key code state window root-x root-y child
                            &allow-other-keys)
  (format *standard-output*
          "Button Press: Code=~A X=~A Y=~A ~%"
          code root-x root-y)
  (force-output)
  (case code
    (1 (format *standard-output* "want to move ?"))
    (2 (format *standard-output* "want to resize ?"))
    (3 (format *standard-output* "what do you want ?"))))


(defun handle-button-release (&rest event-slots
                            &key code state window root-x root-y child
                            &allow-other-keys)
  (format *standard-output*
          "Button Release: Code=~A X=~A Y=~A ~%"
          code root-x root-y)
  (force-output)
  (case code
    (1 (progn
         (format *standard-output* "to Here ?")
         (setf (xlib:drawable-x window) root-x
               (xlib:drawable-y window) root-y)
         (xlib:display-finish-output *display*)))
    (2 (format *standard-output* "want to resize ?"))
    (3 (format *standard-output* "what do you want ?"))))



(defun handle-key-press (&rest event-slots &key root code state
                         &allow-other-keys)
  (declare (ignore event-slots))
  (format *standard-output*
          "[Key Press] Code=~A Root=~A State=~A~%"
          code root state)
  (run-shell-command "/usr/bin/xterm" '() :wait nil)
  (force-output))




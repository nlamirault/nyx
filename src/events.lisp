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
          "~%[Button Press] Code=~A X=~A Y=~A "
          code root-x root-y)
  (force-output)
  (case code
    (1 (format *standard-output* "want to move ?"))
    (2 (format *standard-output* "want to resize ?"))
    (3 (progn
         (format *standard-output* "start application ")
         (run-shell-command "/usr/bin/gnome-terminal" '() :wait nil)
         (force-output))))
  t)


(defun handle-button-release (&rest event-slots
                            &key code state window root-x root-y child
                            &allow-other-keys)
  (format *standard-output*
          "~%[Button Release] Code=~A X=~A Y=~A "
          code root-x root-y)
  (force-output)
  (case code
    (1 (xlib:with-state (window)
         (setf (xlib:drawable-x window) root-x
               (xlib:drawable-y window) root-y)))
    ;;(xlib:display-finish-output *display*)))
    (2 (format *standard-output* "want to resize ?"))
    (3 (format *standard-output* "what do you want ?")))
  (xlib:display-finish-output *display*)
  t)


(defun handle-key-press (&rest event-slots &key root code state
                         &allow-other-keys)
  (declare (ignore event-slots))
  (xlib:ungrab-pointer *display*)
  (xlib:ungrab-keyboard *display*)
  (format *standard-output*
          "~%[Key Press] Code=~A Root=~A State=~A"
          code root state)
  t)


(defun handle-key-release (&rest event-slots &key root code state
                         &allow-other-keys)
;;;   (declare (ignore event-slots))
;;;   (format *standard-output*
;;;           "~%[Key Release] Code=~A Root=~A State=~A"
;;;           code root state)
;;;   (force-output))
  t)


(defun handle-create-notify (&rest event-slots &key
                             window root-x root-y
                             width height border-width override-redirect-p
                             &allow-other-keys)
  (declare (ignore event-slots))
  (format *standard-output*
          "~%[Create Notify] Win=~A X=~A Y=~A"
          window root-x root-y)
  (force-output)
  (unless override-redirect-p
    (setf (xlib:window-event-mask window) '(:structure-notify
                                            :property-change
                                            :colormap-change
                                            :focus-change)))
  t)


(defun handle-destroy-notify (&rest event-slots &key
                             window root-x root-y
                             width height border-width override-redirect-p
                             &allow-other-keys)
  (declare (ignore event-slots))
  (format *standard-output*
          "~%[Destro Notify] Win=~A X=~A Y=~A"
          window root-x root-y)
  (force-output)
  t)


(defun handle-configure-notify (&rest event-slots &key
                             window root-x root-y
                             width height border-width override-redirect-p
                             &allow-other-keys)
  (declare (ignore event-slots))
  (format *standard-output*
          "~%[Configure Notify] Win=~A X=~A Y=~A"
          window root-x root-y)
  (force-output)
  t)


(defun handle-property-notify (&rest event-slots &key
                             window root-x root-y
                             width height border-width override-redirect-p
                             &allow-other-keys)
  (declare (ignore event-slots))
  (format *standard-output*
          "~%[Property Notify] Win=~A X=~A Y=~A"
          window root-x root-y)
  (force-output)
  t)


(defun handle-motion-notify (&rest event-slots &key
                             window root-x root-y
                             width height border-width override-redirect-p
                             &allow-other-keys)
  "Handler for the event :motion-notify which is generated when the pointer
moves."
;;;   (declare (ignore event-slots))
;;;   (format *standard-output*
;;;           "~%[Property Notify] Win=~A X=~A Y=~A"
;;;           window root-x root-y)
;;;   (force-output)
  t)
  

(defun handle-configure-request (&rest event-slots &key
                                 window root-x root-y width height
                                 border-width value-mask
                                 &allow-other-keys)
  (declare (ignore event-slots value-mask))
  (format *standard-output*
          "~%[Configure request] Window=~A X=~A Y=~A Width=~a Height=~A Border=~A"
          window root-x root-y width height border-width)
  (force-output)
  (let ((screen (find-screen (xlib:drawable-root window))))
    (xlib:with-state (window)
      (setf (xlib:drawable-x window) 50 ;root-x
            (xlib:drawable-y window) 80 ;root-y
            (xlib:drawable-height window) height
            (xlib:drawable-width window) width
            (xlib:drawable-border-width window) border-width)))
  t)
      

(defun handle-map-request (&rest event-slots &key
                           window root-x root-y width height
                           border-width value-mask
                           &allow-other-keys)
  (format *standard-output*
          "~%[Map Request] Window=~A X=~A Y=~A"
          window root-x root-y)
  (force-output)
  (let ((screen (find-screen (xlib:drawable-root window))))
    (setf (screen-mapped-windows screen)
          (adjoin window (screen-mapped-windows screen)))
    (xlib:map-window window)
    ;; Run the map window hook on it
    ;;(run-hook-with-args *map-window-hook* w)
    ;; Give it focus
    ;;(focus-window w)))
    )
  t)


(defun handle-unmap-request (&rest event-slots &key
                            window root-x root-y width height
                            border-width value-mask
                            &allow-other-keys)
  (format *standard-output*
          "~%[Unmap request] Window=~A X=~A Y=~A"
          window root-x root-y)
  (force-output)
  (let ((screen (find-screen (xlib:drawable-root window))))
    (setf (screen-mapped-windows screen)
          (adjoin window (screen-mapped-windows screen)))
    (xlib:unmap-window window))
  t)



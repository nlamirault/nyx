;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          screen.lisp
;;;; Purpose:       Nyx screens
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


(defstruct screen
  number
  mapped-windows
  message-window
  )


(defun screen-height (screen)
  "Get height of the screen."
  (let ((root (xlib:screen-root (screen-number screen))))
    (xlib:drawable-height root)))

(defun screen-width (screen)
  "Get width of the screen."
  (let ((root (xlib:screen-root (screen-number screen))))
    (xlib:drawable-width root)))

(defun find-screen (root)
  "Return the screen containing the root window"
  (find-if (lambda (s)
             (eql (xlib:screen-root (screen-number s))
                  root))
           *screens*))


(defun init-screen (screen-number)
  "Creates a new screen."
  (let* ((white (xlib:screen-white-pixel screen-number))
         (black (xlib:screen-black-pixel screen-number))
         (message-window
          (xlib:create-window :parent (xlib:screen-root screen-number)
                              :x 0 :y 0 :width 100 :height 100
                              :background black
                              :border white
                              :border-width 1
                              :colormap (xlib:screen-default-colormap
                                         screen-number)
                              :event-mask '())))
    (xlib:map-window message-window)
    (setf (xlib:window-event-mask (xlib:screen-root screen-number))
          '(:substructure-redirect
            :substructure-notify
            :property-change
            :button-press
            :button-release
            :key-press
            :key-release
            :button-press
            :button-release
            :exposure 
            :pointer-motion))
    (make-screen :number screen-number
                 :message-window message-window
                 )))



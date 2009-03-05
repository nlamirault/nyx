;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Nyx specials variables.
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



(defparameter *version* (asdf:component-version (asdf:find-system "nyx")))

(defparameter *debug* nil "When T, active some logs for debugging.")

(defparameter *nyx-directory*
  (namestring (asdf:component-relative-pathname (asdf:find-system :nyx)))
  "Directory with contains Nyx source files.")

(defparameter *wm-thread* nil)

(defparameter *display* nil 
  "The XLib display.")

(defparameter *screens* '()
  "A list of all screens.")

;; Coordinates

(defparameter *from-x* nil)

(defparameter *from-y* nil)

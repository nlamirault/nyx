;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          nyx.asd
;;;; Purpose:       ASDF definition for nyx
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of nyx, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; nyx users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(in-package :asdf)


(defsystem nyx
    :name "nyx"
    :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :version "0.0.1"
    :licence "MIT License"
    :description "Nyx : Another Window Manager written in Common Lisp."
    :depends-on (:clx)
    :components
    ((:module :src
              :components
              ((:file "package")
               (:file "specials" :depends-on ("package"))
               (:file "tools" :depends-on ("package"))
               (:file "events" :depends-on ("tools"))
               (:file "screen" :depends-on ("package"))
               (:file "nyx" :depends-on ("specials" "events" "screen"))))))


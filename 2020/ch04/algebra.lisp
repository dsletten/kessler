;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               algebra.lisp
;;;;
;;;;   Started:            Wed Aug 12 02:49:28 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :algebra (:use :common-lisp :test))

(in-package :algebra)

;; (dolist (ch '(#\SUPERSCRIPT_ZERO #\SUPERSCRIPT_ONE #\SUPERSCRIPT_TWO #\SUPERSCRIPT_THREE #\SUPERSCRIPT_FOUR #\SUPERSCRIPT_FIVE #\SUPERSCRIPT_SIX #\SUPERSCRIPT_SEVEN #\SUPERSCRIPT_EIGHT #\SUPERSCRIPT_NINE))
;;  (format t "~C" ch))

(defvar *superscripts* "⁰¹²³⁴⁵⁶⁷⁸⁹")

(defun print-x (n)
  (format t "x~C~%" (char *superscripts* n)))

;(coerce '(#\x #\superscript_one #\superscript_nine) 'string) => "x¹⁹"


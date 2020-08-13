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

(defun infix-to-prefix (tokens)
  (labels ((start (tokens)
             (cond ((null tokens) (fail "Empty token list."))
                   ((numberp (first tokens)) (num0 (rest tokens) (list (first tokens))))
                   ((symbolp (first tokens)) (var (first tokens) 1 (rest tokens) '()))
                   (t (fail "Malformed token list."))))
           (fail (msg)
             (error msg))
           (num0 (tokens result)
             (cond ((null tokens) result)
                   (t (case (first tokens)
                        (+ (term0 (rest tokens) result))
                        (* (coeff0 (first result) (rest tokens) (rest result)))
                        (otherwise (fail "Malformed token list.")))) ))
           (term0 (tokens result)
             (cond ((null tokens) (fail "Malformed token list."))
                   ((numberp (first tokens)) (num0 (rest tokens) (cons (first tokens) result)))
                   ((symbolp (first tokens)) (var (first tokens) 1 (rest tokens) result))
                   (t (malformed))))
           (malformed ()
             (fail "Malformed token list."))
           (coeff0 (coeff tokens result)
             (cond ((null tokens) (malformed))
                   ((symbolp (first tokens)) (var (first tokens) coeff (rest tokens) result))
                   (t (malformed))))
           (var (symbol coeff tokens result)
             (if (validate-var symbol result)
                 (cond ((null tokens) (cons `(* ,coeff ,symbol) result))
                       ((eq (first tokens) '**) (exponent `(* ,coeff ,symbol) (rest tokens) result))
                       (t (malformed)))
                 (fail (format nil "Variable does not match ~A" symbol))))
           (validate-var (symbol result)
             t)
           (exponent (term tokens result)
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) 
                    (destructuring-bind (op coeff sym) term
                      (num1 (rest tokens) (cons `(,op ,coeff (** ,sym ,(first tokens))) result))))
                   (t (malformed))))
           (num1 (tokens result)
             (cond ((null tokens) result)
                   (t (case (first tokens)
                        (+ (term1 (rest tokens) result))
                        (* (coeff1 (first result) (rest tokens) (rest result)))
                        (otherwise (malformed)))) )))

    (start tokens)))

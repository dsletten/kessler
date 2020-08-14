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
;;;;   - Subtraction -> additive inverse
;;;;   - Make * optional? (3 * x ** 2 + 9) => (3 x ** 2 + 9)
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
                   (t (malformed))))
           (fail (msg)
             (error msg))
           (num0 (tokens result)
             (cond ((null tokens) result)
                   (t (case (first tokens)
                        (+ (term0 (rest tokens) result))
                        (- (term0- (rest tokens) result))
                        (* (coeff0 (first result) (rest tokens) (rest result)))
                        (otherwise (malformed)))) ))
           (term0 (tokens result)
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) (num0 (rest tokens) (cons (first tokens) result)))
                   ((symbolp (first tokens)) (var (first tokens) 1 (rest tokens) result))
                   (t (malformed))))
           (term0- (tokens result)
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) (num0 (rest tokens) (cons (- (first tokens)) result)))
                   ((symbolp (first tokens)) (var (first tokens) -1 (rest tokens) result))
                   (t (malformed))))
           (malformed ()
             (fail "Malformed token list."))
           (coeff0 (coeff tokens result)
             (cond ((null tokens) (malformed))
                   ((symbolp (first tokens)) (var (first tokens) coeff (rest tokens) result))
                   (t (malformed))))
           (var (symbol coeff tokens result)
             (cond ((null tokens) (cons `(* ,coeff (** ,symbol 1)) result))
                   (t (case (first tokens)
                        (+ (term1 (rest tokens) (cons `(* ,coeff (** ,symbol 1)) result)))
                        (- (term1- (rest tokens) (cons `(* ,coeff (** ,symbol 1)) result)))
                        (** (exponent `(* ,coeff ,symbol) (rest tokens) result))
                        (otherwise (malformed)))) ))
           (validate-var (symbol terms)
             (every #'(lambda (term) (cond ((numberp term) t)
                                           ((consp term) (destructuring-bind (times coeff (exp sym pow)) term
                                                           (declare (ignore times coeff exp pow))
                                                           (eq sym symbol)))
                                           (t (malformed))))
                    terms))
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
                        (- (term1- (rest tokens) result))
                        (* (coeff1 (first result) (rest tokens) (rest result)))
                        (otherwise (malformed)))) ))
           (coeff1 (coeff tokens result)
             (cond ((null tokens) (malformed))
                   ((symbolp (first tokens))
                    (if (validate-var (first tokens) result)
                        (var (first tokens) coeff (rest tokens) result)
                        (fail (format nil "Variable does not match: ~A" (first tokens)))) )
                   (t (malformed))))
           (term1 (tokens result)
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) (num1 (rest tokens) (cons (first tokens) result)))
                   ((symbolp (first tokens))
                    (if (validate-var (first tokens) result)
                        (var (first tokens) 1 (rest tokens) result)
                        (fail (format nil "Variable does not match: ~A" (first tokens)))) )
                   (t (malformed))))
           (term1- (tokens result)
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) (num1 (rest tokens) (cons (- (first tokens)) result)))
                   ((symbolp (first tokens))
                    (if (validate-var (first tokens) result)
                        (var (first tokens) -1 (rest tokens) result)
                        (fail (format nil "Variable does not match: ~A" (first tokens)))) )
                   (t (malformed)))) )
    (start tokens)))

(defun process-prefix (terms)
  (let ((term-map (make-hash-table))
        (degree 0))
    (dolist (term terms)
      (cond ((numberp term) (incf (gethash 0 term-map 0) term))
            ((consp term) (destructuring-bind (times coeff (exp sym pow)) term
                            (declare (ignore times exp sym))
                            (incf (gethash pow term-map 0) coeff)
                            (when (> pow degree)
                              (setf degree pow))))
            (t (error "Huh?"))))
    (loop for power from 0 upto degree collect (gethash power term-map 0))))

(defun print-power (n)
  (cond ((> n 10) 
         (print-power (floor n 10))
         (print-power (mod n 10)))
        (t (format t "~C" (char *superscripts* n)))) )

(defun print-poly (coefficients)
  (let ((n (1- (length coefficients))))
    (loop for i from n downto 0
          for coeff in (reverse coefficients)
          unless (zerop coeff)
          do (cond ((minusp coeff) (unless (= i n) (format t " - ")) (print-term (- coeff) i))
                   (t (unless (= i n) (format t " + ")) (print-term coeff i)))) ))

(defun print-term (coeff i)
  (case i
    (0 (format t "~D" coeff))
    (1 (format t "~Dx" coeff))
    (otherwise (format t "~Dx" coeff)
               (print-power i))))



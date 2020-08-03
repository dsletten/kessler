;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Wed Jul 22 03:29:02 2020
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
;(load "/home/slytobias/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch02 (:use :common-lisp :test))

(in-package :ch02)

;;;
;;;    2.14
;;;    Polynomial ax + b -> (b a)
;;;    
(defun multiply-first-degree-polynomials (p q)
  (destructuring-bind (b a) p
    (destructuring-bind (d c) q
      (list (* b d) (+ (* a d) (* b c)) (* a c)))) )

(deftest test-multiply-first-degree-polynomials ()
  (check
   (equal (multiply-first-degree-polynomials '(2 3) '(1 3)) '(2 9 9))
   (equal (multiply-first-degree-polynomials '(1 1) '(1 1)) '(1 2 1))
   (equal (multiply-first-degree-polynomials '(4 3) '(-2 7)) '(-8 22 21))
   (equal (multiply-first-degree-polynomials '(-2 7) '(4 3)) '(-8 22 21))))

;;;
;;;    Ex. 1
;;;    
;; (defun quadratic (a b c)
;;   (let ((discriminant (- (* b b) (* 4 a c))))
;;     (if (minusp discriminant)
;;         (progn
;;           (assert (not (minusp discriminant)) (a b c) "Discriminant must be non-negative.")
;;           (quadratic a b c))
;;         (values (/ (+ (- b) (sqrt discriminant)) (* 2 a))
;;                 (/ (- (- b) (sqrt discriminant)) (* 2 a)))) ))

(defun quadratic (a b c)
  (assert (every #'numberp (list a b c)) (a b c) "A, B, and C must all be numbers.")
  (symbol-macrolet ((discriminant (- (* b b) (* 4 a c))))
    (assert (not (minusp discriminant)) (a b c) "Discriminant must be non-negative.")
    (values (/ (+ (- b) (sqrt discriminant)) (* 2 a))
            (/ (- (- b) (sqrt discriminant)) (* 2 a)))) )

;;;
;;;    Ex. 2
;;;    
(defun extract (p degree)
  "Extract coefficient of term DEGREE from second-degree polynomial."
  (destructuring-bind (c b a) p
    (ecase degree
      (0 c)
      (1 b)
      (2 a))))

(deftest test-extract ()
  (check
   (= (extract '(-6 1 2) 0) -6)
   (= (extract '(-6 1 2) 1) 1)
   (= (extract '(-6 1 2) 2) 2)))

;;;
;;;   Ex. 3
;;;   
(defun roots (p)
  "Compute real roots of 2nd-degree polynomial P."
  (destructuring-bind (c b a) p
    (quadratic a b c)))

(deftest test-roots ()
  (check
   (equal (multiple-value-list (roots '(-6 1 1))) '(2.0 -3.0))))


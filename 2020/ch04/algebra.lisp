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

(deftest test-infix-to-prefix ()
  (check
   (equal (infix-to-prefix '(55)) '(55))
   (equal (infix-to-prefix '(-34 * x)) '((* -34 (** x 1))))
   (equal (infix-to-prefix '(3 * x ** 2 + 7 * x + 8)) '(8 (* 7 (** X 1)) (* 3 (** X 2))))
   (equal (infix-to-prefix '(x ** 3 - 5 * x ** 2 - 17)) '(-17 (* -5 (** X 2)) (* 1 (** X 3))))
   (equal (infix-to-prefix '(x ** 3 - 4 * x ** 2 - 17 + 9 * x ** 12)) '((* 9 (** X 12)) -17 (* -4 (** X 2)) (* 1 (** X 3))))
   (equal (infix-to-prefix '(2 + x ** 2 + 9 * x - 4 * x ** 2)) '((* -4 (** X 2)) (* 9 (** X 1)) (* 1 (** X 2)) 2))
   (equal (infix-to-prefix '(2 + x ** 2 + 9 * x - 4 * x ** 2 + 4 - x)) '((* -1 (** X 1)) 4 (* -4 (** X 2)) (* 9 (** X 1)) (* 1 (** X 2)) 2))))

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

(deftest test-process-prefix ()
  (check
   (equal (process-prefix (infix-to-prefix '(55))) '(55))
   (equal (process-prefix (infix-to-prefix '(-34 * x))) '(0 -34))
   (equal (process-prefix (infix-to-prefix '(3 * x ** 2 + 7 * x + 8))) '(8 7 3))
   (equal (process-prefix (infix-to-prefix '(x ** 3 - 5 * x ** 2 - 17))) '(-17 0 -5 1))
   (equal (process-prefix (infix-to-prefix '(x ** 3 - 4 * x ** 2 - 17 + 9 * x ** 12))) '(-17 0 -4 1 0 0 0 0 0 0 0 0 9))
   (equal (process-prefix (infix-to-prefix '(2 + x ** 2 + 9 * x - 4 * x ** 2))) '(2 9 -3))
   (equal (process-prefix (infix-to-prefix '(2 + x ** 2 + 9 * x - 4 * x ** 2 + 4 - x))) '(6 8 -3))))

(defun print-power (s n)
  (cond ((> n 10) 
         (print-power s (floor n 10))
         (print-power s (mod n 10)))
        (t (format s "~C" (char *superscripts* n)))) )

(defun print-poly (coefficients)
  (with-output-to-string (s)
    (let ((n (1- (length coefficients))))
      (loop for i from n downto 0
            for coeff in (reverse coefficients)
            unless (zerop coeff)
            do (cond ((minusp coeff) 
                      (if (= i n)
                          (format s "-")
                          (format s " - "))
                      (print-term s (- coeff) i))
                     (t (unless (= i n) (format s " + ")) (print-term s coeff i)))) )))

(defun print-term (s coeff i)
  (case i
    (0 (format s "~D" coeff))
    (1 (format s "~[~;~:;~:*~D~]x" coeff))
    (otherwise (format s "~[~;~:;~:*~D~]x" coeff)
               (print-power s i))))

(deftest test-print-poly ()
  (check
   (equal (print-poly (process-prefix (infix-to-prefix '(55)))) "55")
   (equal (print-poly (process-prefix (infix-to-prefix '(-34 * x)))) "-34x")
   (equal (print-poly (process-prefix (infix-to-prefix '(3 * x ** 2 + 7 * x + 8)))) "3x² + 7x + 8")
   (equal (print-poly (process-prefix (infix-to-prefix '(x ** 3 - 5 * x ** 2 - 17)))) "x³ - 5x² - 17")
   (equal (print-poly (process-prefix (infix-to-prefix '(x ** 3 - 4 * x ** 2 - 17 + 9 * x ** 12)))) "9x¹² + x³ - 4x² - 17")
   (equal (print-poly (process-prefix (infix-to-prefix '(2 + x ** 2 + 9 * x - 4 * x ** 2)))) "-3x² + 9x + 2")
   (equal (print-poly (process-prefix (infix-to-prefix '(2 + x ** 2 + 9 * x - 4 * x ** 2 + 4 - x)))) "-3x² + 8x + 6")))


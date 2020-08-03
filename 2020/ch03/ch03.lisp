;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Sat Aug  1 03:45:40 2020
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

(defpackage :ch03 (:use :common-lisp :test) (:shadow :assoc :reverse :nth))

(in-package :ch03)

;;;
;;;    3.12
;;;    
(defun assoc (item alist &rest args &key (key nil key-supplied-p) (test #'eql))
  (if (null alist)
      nil
      (destructuring-bind (entry . more) alist
        (cond (key-supplied-p (if (funcall test item (funcall key (car entry)))
                                  entry
                                  (apply #'assoc item more args)))
              ((funcall test item (car entry)) entry)
              (t (apply #'assoc item more args)))) ))

(deftest test-assoc ()
  (check
   (equal (assoc 'a '((c . 3) (b . 2) (a . 1))) '(a . 1))
   (equal (assoc 'd '((c . 3) (b . 2) (a . 1))) nil)
   (equal (assoc 2 '((3 . c) (2 . b) (1 . a)) :test #'(lambda (item elt) (< item elt))) '(3 . c))
   (equal (assoc "BOB" '(("Tom" . 28) ("Bob" . 42) ("Mary" . 19)) :key #'string-upcase :test #'string=) '("Bob" . 42))))

(defun reverse (l)
  (labels ((reverse-aux (l result)
             (cond ((endp l) result)
                   (t (reverse-aux (rest l) (cons (first l) result)))) ))
    (reverse-aux l '())))

(deftest test-reverse ()
  (check
   (equal (reverse #1='()) (cl:reverse #1#))
   (equal (reverse #2='(a)) (cl:reverse #2#))
   (equal (reverse #3='(a b)) (cl:reverse #3#))
   (equal (reverse #4='(a b c d e)) (cl:reverse #4#))
   (equal (reverse #5='((a 1) (b 2) (c 3) (d 4))) (cl:reverse #5#))))
  
;;;
;;;    3.13
;;;    
(defun super-reverse (l)
  (labels ((reverse-aux (l result)
             (cond ((endp l) result)
                   ((atom (first l)) (reverse-aux (rest l) (cons (first l) result)))
                   (t (reverse-aux (rest l) (cons (super-reverse (first l)) result)))) ))
    (reverse-aux l '())))

(defun super-reverse (l)
  (labels ((reverse-aux (obj)
             (cond ((atom obj) obj)
                   (t (super-reverse obj)))) )
    (let ((result '()))
      (dolist (elt l result)
        (push (reverse-aux elt) result)))) )

(deftest test-super-reverse ()
  (check
   (equal (super-reverse #1='()) (cl:reverse #1#))
   (equal (super-reverse #2='(a)) (cl:reverse #2#))
   (equal (super-reverse #3='(a b)) (cl:reverse #3#))
   (equal (super-reverse #4='(a b c d e)) (cl:reverse #4#))
   (equal (super-reverse '((a 1) (b 2) (c 3) (d 4))) '((4 d) (3 c) (2 b) (1 a)))
   (equal (super-reverse '(a b (c (d (((f g)) e)))) ) '((((e ((g f))) d) c) b a))))

;;;
;;;    3.14 (See FLATTEN-2011 in flatten.lisp)
;;;    
(defun flatten (tree)
  (cond ((null tree) '())
        ((null (first tree)) (flatten (rest tree)))
        ((atom (first tree)) (cons (first tree) (flatten (rest tree))))
        (t (flatten (list* (first (first tree)) (rest (first tree)) (rest tree)))) ))

(deftest test-flatten ()
  (check
   (equal (flatten '()) '())
   (equal (flatten '(a b c d)) '(a b c d))
   (equal (flatten '(a b (c d (e f) g) h)) '(a b c d e f g h))))

;;;
;;;    3.15
;;;    
(defun make-same-list (n obj)
  "Create list with N copies of OBJ"
  (check-type n (integer 0))
  (if (zerop n)
      '()
      (cons obj (make-same-list (1- n) obj))))

(deftest test-make-same-list ()
  (check
   (equal (make-same-list 0 'foo) '())
   (equal (make-same-list 1 'foo) '(foo))
   (equal (make-same-list 5 'foo) '(foo foo foo foo foo))))

;;;
;;;    3.16
;;;    
(defun nth (n l)
  (check-type n (integer 0))
  (if (zerop n)
      (first l)
      (nth (1- n) (rest l))))

(deftest test-nth ()
  (check
   (equal (nth #1=0 #10='(a b c d)) (cl:nth #1# #10#))
   (equal (nth #2=1 #10#) (cl:nth #2# #10#))
   (equal (nth #3=2 #10#) (cl:nth #3# #10#))
   (equal (nth #4=3 #10#) (cl:nth #4# #10#))
   (equal (nth #5=4 #10#) (cl:nth #5# #10#))))

;;;
;;;    3.17
;;;    
(defun add-1-to-tree (tree)
  (cond ((null tree) '())
        ((numberp tree) (1+ tree))
        ((atom tree) tree)
        (t (cons (add-1-to-tree (car tree))
                 (add-1-to-tree (cdr tree)))) ))

(deftest test-add-1-to-tree ()
  (check
   (equal (add-1-to-tree '(1 2 3 4)) '(2 3 4 5))
   (equal (add-1-to-tree '((2 3 (4 5) 6) 7 (8))) '((3 4 (5 6) 7) 8 (9)))
   (equal (add-1-to-tree '((3 4 (x y) 3))) '((4 5 (x y) 4)))) )

;;;
;;;    3.18
;;;    
(defun internal-to-external (internal-form)
  (labels ((internal-to-external-aux (coefficients current-exponent)
             (cond ((endp coefficients) '())
                   ((zerop (first coefficients)) (internal-to-external-aux (rest coefficients) (1+ current-exponent)))
                   (t (cons (build-term (first coefficients) 'x current-exponent) (internal-to-external-aux (rest coefficients) (1+ current-exponent)))) ))
           (build-term (coefficient variable exponent)
             (cond ((zerop exponent) coefficient)
                   ((= coefficient 1) `(** ,variable ,exponent))
                   (t `(* ,coefficient (** ,variable ,exponent)))) ))
    (let ((terms (internal-to-external-aux internal-form 0)))
      (cond ((null terms) 0)
            ((null (rest terms)) (first terms))
            (t (cons '+ terms)))) ))

(deftest test-internal-to-external ()
  (check
   (equal (internal-to-external '(0)) 0)
   (equal (internal-to-external '(0 0)) 0)
   (equal (internal-to-external '(4 5 6)) '(+ 4 (* 5 (** x 1)) (* 6 (** x 2))))
   (equal (internal-to-external '(5 0 1 -4)) '(+ 5 (** x 2) (* -4 (** x 3)))) ))

;;;
;;;    3.19
;;;    
(defun slope (p1 p2)
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (if (= x1 x2)
          'infinity
          (/ (- y2 y1) (- x2 x1)))) ))

(deftest test-slope ()
  (check
   (eq (slope '(3 5) '(3 8)) 'infinity)
   (= (slope '(0 0) '(1 4)) 4)
   (= (slope '(1 4) '(0 0)) 4)
   (= (slope '(5 3) '(8 3)) 0)))

;;;
;;;    3.20
;;;    
(defun collinearp (p1 p2 p3)
  (let ((slope1 (slope p1 p2))
        (slope2 (slope p1 p3)))
    (or (and (eq slope1 'infinity)
             (eq slope2 'infinity))
        (= slope1 slope2))))

(deftest test-collinearp ()
  (check
   (collinearp '(0 0) '(2 2) '(4 4))
   (collinearp '(4 4) '(0 0) '(2 2))
   (collinearp '(2 2) '(0 0) '(4 4))
   (collinearp '(-1 2) '(7 26) '(14 47)) ; (defun f (x) (+ 5 (* 3 x)))
   (collinearp '(-1 3) '(2 -3) '(3 -5)) ; (defun f (x) (+ 1 (* -2 x)))
   (not (collinearp '(-2 1) '(4 25) '(6 32)))) ) ; (defun f (x) (+ 9 (* 4 x)))

;;;
;;;    3.21
;;;    
(defun between (p1 p2 mid)
  "Is the point MID between points P1 and P2? All 3 points must be distinct."
  (destructuring-bind (x1 y1) p1
    (destructuring-bind (x2 y2) p2
      (destructuring-bind (x3 y3) mid
        (and (collinearp p1 p2 mid)
             (if (= x1 x2) ; Vertical line
                 (or (< y1 y3 y2)
                     (< y2 y3 y1))
                 (or (< x1 x3 x2)
                     (< x2 x3 x1)))) ))))

(deftest test-between ()
  (check
   (not (between '(2 2) '(2 2) '(2 2)))
   (between '(0 0) '(4 4) '(2 2))
   (between '(4 4) '(0 0) '(2 2))
   (not (between '(0 0) '(2 2) '(4 4)))
   (between '(-1 2) '(14 47) '(7 26))
   (not (between '(-2 1) '(6 32) '(4 25)))
   (between '(3 -1) '(3 7) '(3 4))
   (not (between '(3 -1) '(3 4) '(3 7)))
   (not (between '(3 -1) '(3 4) '(3 4)))
   (between '(-9 7) '(2 7) '(0 7))
   (not (between '(-9 7) '(2 7) '(2 7)))
   (not (between '(-9 7) '(0 7) '(2 7)))) )

;;;
;;;    3.22
;;;    
(defvar *variable* 'x)
(defun internal-to-external (internal-form)
  (labels ((internal-to-external-aux (coefficients current-exponent)
             (cond ((endp coefficients) '())
                   ((zerop (first coefficients)) (internal-to-external-aux (rest coefficients) (1+ current-exponent)))
                   (t (cons (build-term (first coefficients) *variable* current-exponent) (internal-to-external-aux (rest coefficients) (1+ current-exponent)))) ))
           (build-term (coefficient variable exponent)
             (cond ((zerop exponent) coefficient)
                   ((= coefficient 1) `(** ,variable ,exponent))
                   (t `(* ,coefficient (** ,variable ,exponent)))) ))
    (let ((terms (internal-to-external-aux internal-form 0)))
      (cond ((null terms) 0)
            ((null (rest terms)) (first terms))
            (t (cons '+ terms)))) ))

(deftest test-internal-to-external ()
  (check
   (equal (internal-to-external '(0)) 0)
   (equal (internal-to-external '(0 0)) 0)
   (let ((*variable* 'y))
     (equal (internal-to-external '(4 5 6)) '(+ 4 (* 5 (** y 1)) (* 6 (** y 2)))) )
   (equal (internal-to-external '(4 5 6)) '(+ 4 (* 5 (** x 1)) (* 6 (** x 2))))
   (let ((*variable* 'z))
     (equal (internal-to-external '(5 0 1 -4)) '(+ 5 (** z 2) (* -4 (** z 3)))) )
   (equal (internal-to-external '(5 0 1 -4)) '(+ 5 (** x 2) (* -4 (** x 3)))) ))


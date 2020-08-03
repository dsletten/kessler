;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch03.lisp
;;;
;;;   STARTED:            Thu Aug  1 22:50:27 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;
(load "~/lisp/programs/utils.lisp")
;;;
;;;    Ex. 3.8
;;;
(defun const-term (poly)
  "Return the value of the constant term from a given polynomial."
  (cond ((numberp poly) poly)
	((symbolp poly) 0)
	((eq (car poly) '+)
	 (let ((term (car (last poly))))
	   (cond ((numberp term) term)
		 ((symbolp term) 0)
		 ((and (eq (car term) '*)
		       (listp (third term))
		       (zerop (third (third term))))
		  (second term))
		 (t 0))))
	(t 0)))

(test 'const-term '((((+ (** x 1) (* 4 (** x 0)))) 4)
		    (((* 4 (** x 2))) 0)
		    (((+ (* 3 x) 4)) 4)
		    (((+ x 4)) 4)
		    (((+ (* 5 (** x 3)) (** x 2) (* -2 x))) 0)
		    ((5) 5)
		    ((x) 0)))

(defun term-coefficient (term)
  (cond ((numberp term) term)
	((symbolp term) 1)
	((eq (car term) '*) (cadr term))
	(t 1)))

(defun term-exponent (term)
  (cond ((numberp term) 0)
	((symbolp term) 1)
	((eq (car term) '*) (term-exponent (third term)))
	(t (third term))))

;;;
;;;    Returns nil if term is not found.
;;;    
(defun poly-nth (poly n)
  "Find the term of specified degree in a given polynomial."
  (cond	((and (listp poly) (eq (car poly) '+))
	 (dolist (term (cdr poly) nil)
	   (when (= n (term-exponent term))
	     (return term))))
	((= (term-exponent poly) n) poly)
	(t nil)))

(test 'poly-nth '((((+ (** x 1) (* 4 (** x 0))) 2) nil)
		  (((+ (** x 1) (* 4 (** x 0))) 1) (** x 1))
		  (((+ (** x 1) (* 4 (** x 0))) 0) (* 4 (** x 0)))
		  (((* 4 (** x 2)) 2) (* 4 (** x 2)))
		  (((* 4 (** x 2)) 1) nil)
		  (((* 4 (** x 2)) 0) nil)
		  (((+ (* 3 x) 4) 1) (* 3 x))
		  (((+ (* 3 x) 4) 0) 4)
		  (((+ (* 3 x) 4) 2) nil)
		  (((+ x 4) 1) x)
		  (((+ (* 5 (** x 3)) (** x 2) (* -2 x)) 3) (* 5 (** x 3)))
		  (((+ (* 5 (** x 3)) (** x 2) (* -2 x)) 2) (** x 2))
		  (((+ (* 5 (** x 3)) (** x 2) (* -2 x)) 1) (* -2 x))
		  ((5 0) 5)
		  ((x 1) x)))

(defun const-term (poly)
  (term-coefficient (or (poly-nth poly 0) 0)))

(test 'const-term '((((+ (** x 1) (* 4 (** x 0)))) 4)
		    (((* 4 (** x 2))) 0)
		    (((+ (* 3 x) 4)) 4)
		    (((+ x 4)) 4)
		    (((+ (* 5 (** x 3)) (** x 2) (* -2 x))) 0)
		    ((5) 5)
		    ((x) 0)))

;;;
;;;    Ex. 3.12
;;;
(defun my-assoc (key a-list)
  (cond ((null a-list) nil)
	((eql (caar a-list) key) (car a-list))
	(t (my-assoc key (cdr a-list)))) )

(defun my-reverse (l)
  (labels ((reverse-aux (l result)
	     (cond ((null l) result)
		   (t (reverse-aux (cdr l) (cons (car l) result)))) ))
    (reverse-aux l ())))

;;;
;;;    Ex. 3.13
;;;
(defun super-reverse (l)
  (labels ((reverse-aux (obj result)
	     (cond ((null obj) result)
		   ((atom obj) obj)
		   (t (reverse-aux (cdr obj)
				   (cons (reverse-aux (car obj) ())
					 result)))) ))
    (reverse-aux l ())))

;;;
;;;    Ex. 3.14
;;;
(defun flatten (obj)
  (cond ((null obj) ())
	((atom obj) (list obj))
	(t (append (flatten (car obj))
		   (flatten (cdr obj))))))

(defun flatten (l)
  (labels ((flatten-aux (obj result)
	     (cond ((null obj) ())
		   ((atom obj) (cons obj result))
		   (t (flatten-aux (car obj)
				   (flatten-aux (cdr obj) result)))) ))
    (flatten-aux l ())))

;;;
;;;    Ex. 3.15
;;;
(defun make-same-list (n obj)
  (cond ((zerop n) ())
	(t (cons obj (make-same-list (1- n) obj)))) )

;;;
;;;    Ex. 3.16
;;;
(defun my-nth (n l)
  (cond ((zerop n) (car l))
	(t (my-nth (1- n) (cdr l)))) )

;;;
;;;    Ex. 3.17
;;;
(defun add-1-to-list (obj)
  (cond ((numberp obj) (1+ obj))
	((atom obj) obj)
	(t (cons (add-1-to-list (car obj))
		 (add-1-to-list (cdr obj)))) ))

;;;
;;;    Ex. 3.19
;;;
(defun slope (p1 p2)
  (destructuring-bind ((x0 y0) (x1 y1)) (list p1 p2)
    (if (= x0 x1)
	'infinity
	(/ (- y0 y1) (- x0 x1)))) )

;;;
;;;    Ex. 3.20
;;;
(defun collinearp (p1 p2 p3)
  (equal (slope p1 p2) (slope p1 p3)))

;;;
;;;    Ex. 3.21
;;;
(defun between (p1 p2 p3)
  (destructuring-bind ((x0 y0) (x1 y1) (x2 y2)) (list p1 p2 p3)
    (and (collinearp p1 p2 p3)
	 (or (< x0 x2 x1)
	     (> x0 x2 x1)
	     (> y0 y2 y1)
	     (< y0 y2 y1)))) )

(test 'between '((((0 0) (4 4) (2 2)) t)
		 (((0 0) (2 2) (4 4)) nil)
		 (((2 2) (0 0) (4 4)) nil)
		 (((4 4) (0 0) (2 2)) t)
		 (((0 1) (0 5) (0 3)) t)
		 (((0 4) (0 2) (0 3)) t)
		 (((0 0) (0 5) (0 9)) nil)))

;;;
;;;    Ex. 3.23
;;;
(defun my-map (op l)
  (cond ((null l) ())
	(t (cons (apply op (list (car l)))
		 (my-map op (cdr l)))) ))

;;;
;;;    Ex. 3.24
;;;
(let ((op-map '((greeble +)
		(ngreeble -)
		(prozg *))))
  (defun gzorp (op &rest args)
    (let ((lisp-op (second (assoc op op-map))))
      (if lisp-op
	  (apply lisp-op args)
	  'no-op))))
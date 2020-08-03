;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               algebra.lisp
;;;
;;;   STARTED:            Sat Aug  3 12:53:08 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE: This is a redo of Kessler's algebra system using structures
;;;   to implement polynomials. (See algebra.original)
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
;;;   Assumptions--
;;;     -Polynomials are immutable
;;;     -The highest-degree coefficient is never 0!
;;;

(load "~/lisp/programs/utils")

(defstruct (polynomial (:print-function print-polynomial))
  (coefficients (vector 0))
  (variable 'x))

(defun polynomial-degree (poly)
  (1- (length (polynomial-coefficients poly))))

(defun coefficient-of-degree (poly n)
  (if (> n (polynomial-degree poly))
      nil
      (aref (polynomial-coefficients poly) n)))

(defun print-polynomial (poly stream depth)
  (format stream "#<Polynomial ~A>~%" (polynomial-to-string poly)))

(defun polynomial-to-string (poly)
  (let ((sym (polynomial-variable poly))
	(degree (polynomial-degree poly)))
    (labels ((polynomial-to-string-aux (n)
	       (cond ((zerop n)
		      (if (zerop (coefficient-of-degree poly 0))
			  ""
			  (format nil "~:[ + ~; - ~]~A"
				  (minusp (coefficient-of-degree poly 0))
				  (abs-term-to-infix
				   (coefficient-of-degree poly 0) sym 0))))
		     (t (let ((coeff (coefficient-of-degree poly n)))
			  (concatenate 'string
				       (if (zerop coeff)
					   ""
					   (format nil "~:[ + ~; - ~]~A"
						   (minusp coeff)
						   (abs-term-to-infix coeff
								      sym n)))
				       (polynomial-to-string-aux (1- n)))) ))))
      (let ((first-term (format nil "~A"
				(term-to-infix
				 (coefficient-of-degree poly
							degree) sym degree))))
	(if (> degree 0)
	    (concatenate 'string
			 first-term
			 (polynomial-to-string-aux (1- degree)))
	    first-term)))) )


(defun term-to-infix (coeff var pow)
  (cond ((zerop pow)
	 (format nil "~A" coeff))
	((zerop coeff) "")
	((= coeff 1)
	 (if (= pow 1)
	     (string var)
	     (format nil "~A^~A" var pow)))
	((= coeff -1)
	 (if (= pow 1)
	     (format nil "-~A" var)
	     (format nil "-~A^~A" var pow)))
	(t (if (= pow 1)
	       (format nil "~A~A" coeff var)
	       (format nil "~A~A^~A" coeff var pow)))) )

(defun abs-term-to-infix (coeff var pow)
  (term-to-infix (abs coeff) var pow))
    
(defun infix-to-poly (expr)
  (let ((coeff-list (remove-trailing-zeroes (infix-to-internal expr))))
    (make-polynomial :coefficients (make-array (length coeff-list)
					       :initial-contents coeff-list))))

;;;
;;;    See Slade ch. 7 matrix.lisp for similar technique.
;;;    Note: The input expression need not be ordered by degrees of terms.
;;;    
(defun list-of-terms (expr)
  (cond ((null expr) (list ()))
	((eq (car expr) '+) (cons () (list-of-terms (cdr expr))))
	((eq (car expr) '-)
	 (if (numberp (cadr expr))
	     (cons () (list-of-terms (cons (- (cadr expr)) (cddr expr))))
	     (cons () (list-of-terms (cons -1 (cons '* (cdr expr)))) )))
	(t (let ((rest (list-of-terms (cdr expr))))
	     (cons (cons (car expr) (car rest)) (cdr rest)))) ))

; (defun process-term (term)
;   (cond ((numberp (car term))
; 	 (if (cdr term)
; 	     (cons '* (cons (car term) (list (process-term (cddr term)))) )
; 	     (car term)))
; 	((symbolp (car term))
; 	 (if (cdr term)
; 	     (cons '** (cons (car term) (list (third term))))
; 	     (car term)))) )

;;;
;;;    Convert a single term from infix to prefix notation.
;;;    4 possible formats are accepted:
;;;    Constant term/variable w/o coefficient,exponent -- (5) or (x)
;;;    Variable w/ coefficient (implied exponent) -- (2 * x)
;;;    Variable w/ exponent (implied coefficient) -- (x ** 3)
;;;    Variable w/ coefficient and exponent -- (4 * x ** 9)
;;;    
;;;    notice the symmetry here:
;;;    (3 * x ** 2) CADR is operator, CDDR evaluated recursively
;;;    =>(* 3 (** x 2))
;;;    (x ** 3)     CADR is operator, CDDR evaluated recursively
;;;    =>(** x 3)
;;;    
(defun process-term (term)
  (if (single term)
      (car term)
      (list (second term) (first term) (process-term (cddr term)))) )

(defun infix-to-prefix (expr)
  (let ((terms-list (list-of-terms expr)))
    (if (single terms-list)
	(process-term (car terms-list))
	(cons '+ (mapcar #'process-term terms-list)))) )

;;;
;;;    TERM-COEFFICIENT and TERM-EXPONENT assume non-NIL terms.
;;;    
(defun term-coefficient (term)
  (assert (not (null term)) (term) "term-coefficient: Term should not be nil.")
  (cond ((numberp term) term)
	((symbolp term) 1)
	((eq (car term) '*) (cadr term))
	(t 1)))

(defun term-exponent (term)
  (assert (not (null term)) (term) "term-exponent: Term should not be nil.")
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

; (defun poly-nth (poly n)
;   "Find the term of specified degree in a given polynomial."
;   (cond	((null poly) nil)
; 	((listp poly)
; 	 (cond ((eq (car poly) '+) (poly-nth (cdr poly) n))
; 	       ((= (term-exponent (car poly)) n) (car poly))
; 	       (t (poly-nth (cdr poly) n))))
; 	((= (term-exponent poly) n) poly)
; 	(t nil)))

(defun find-coefficient-of-matching-term (pow poly)
  (let ((term (poly-nth poly pow)))
    (if term
	(term-coefficient term)
	0)))

(defun degree-of-polynomial (poly)
  (labels ((degree-aux (poly max)
	     (cond ((null poly) max)
		   (t (degree-aux (cdr poly)
				  (max max (term-exponent (car poly)))) ))))
    (if (and (listp poly) (eq (car poly) '+))
	(degree-aux (cddr poly) (term-exponent (cadr poly)))
	(term-exponent poly))))

(defun external-to-internal (poly)
  (build-internal-form poly 0 (degree-of-polynomial poly)))

(defun build-internal-form (poly start end)
  (if (> start end)
      ()
      (cons (find-coefficient-of-matching-term start poly)
	    (build-internal-form poly (1+ start) end))))

;;;
;;;    Ex. 4.2
;;;    
(defun coefficient-and-exponent-of-term (term)
  (cond ((numberp term) (list term 0))
	((symbolp term) (list 1 1))
	((eq (car term) '*) (list (cadr term)
				  (cadr (coefficient-and-exponent-of-term
					 (third term)))) )
	(t (list 1 (third term)))) )

;;;
;;;    Ex. 4.3
;;;    
(defun external-to-internal-2 (poly)
  (reverse (build-internal-form-2 poly (degree-of-polynomial poly))))

(defun build-internal-form-2 (poly end)
  (cond ((zerop end) (list (find-coefficient-of-matching-term 0 poly)))
	(t (cons (find-coefficient-of-matching-term end poly)
		 (build-internal-form-2 poly (1- end)))) ))

;;;
;;;    Ex. 4.4
;;;
(defun infix-to-internal (infix-poly)
  (external-to-internal (infix-to-prefix infix-poly)))

; (defun infix-to-internal (infix-poly)
;   (labels ((infix-to-prefix-aux (terms-list)
; 	     (cond ((null terms-list) ())
; 		   (t (cons (process-term (car terms-list))
; 			    (infix-to-prefix-aux (cdr terms-list)))) )))
;     (let* ((terms-list (list-of-terms infix-poly))
; 	   (poly (if (cdr terms-list)
; 		     (cons '+ (infix-to-prefix-aux terms-list))
; 		     (process-term (car terms-list)))) )
;       (build-internal-form poly 0 (degree-of-polynomial poly)))) )

(let ((default-variable 'x))
  (defun convert-term (coefficient exponent)
    (cond ((zerop exponent) coefficient)
	  ((zerop coefficient) nil)
	  ((= coefficient exponent 1) default-variable)
	  ((= coefficient 1) (list '** default-variable exponent))
	  (t (list '* coefficient (convert-term 1 exponent)))) ))

;;;
;;;    Convert an 'internal' list of coefficients into a list of 'external'
;;;    prefix terms. Terms with 0 coefficients are removed. If the constant
;;;    term is 0 it is only included if it is the only term.
;;;    
(defun build-list-of-terms (internal-poly)
  (labels ((build-aux (l n result)
	     (cond ((null l) result)
		   (t (let ((term (convert-term (car l) n)))
			(if term
			    (build-aux (cdr l)
				       (1+ n)
				       (cons (convert-term (car l) n) result))
			    (build-aux (cdr l) (1+ n) result)))) )))
    (if (and (zerop (car internal-poly))
	     (not (single internal-poly)))
	(build-aux (cdr internal-poly) 1 ())
	(build-aux internal-poly 0 ()))) )

(defun internal-to-external (list-of-coefficients)
  (let ((terms-list (build-list-of-terms list-of-coefficients)))
    (if (single terms-list)
	(car terms-list)
	(cons '+ terms-list))))

; (defun remove-zero-terms (terms-list)
;   (labels ((remove-zero-terms-aux (terms-list)
; 	     (cond ((null terms-list) ())
; 		   ((zerop (term-coefficient (car terms-list)))
; 		    (remove-zero-terms-aux (cdr terms-list))) ...

(defun polynomial-add (p1 p2)
  (let ((longer p1)
	(shorter p2))
    (when (< (polynomial-degree longer) (polynomial-degree shorter))
      (reversef longer shorter))
    (let ((sum-vec (copy-polynomial longer)))
      (setf (polynomial-coefficients sum-vec)
	    (copy-seq (polynomial-coefficients longer)))
      (do ((i 0 (1+ i))
	   (stop (polynomial-degree shorter)))
	  ((> i stop) (normalize sum-vec))
	(incf (aref (polynomial-coefficients sum-vec) i)
	      (aref (polynomial-coefficients shorter) i)))) ))

(defun normalize (poly)
  (let ((pos (or (position-if (complement #'zerop)
			      (polynomial-coefficients poly)
			      :from-end t)
		 0))
	(result (copy-polynomial poly)))
    (unless (= pos (polynomial-degree result))
      (setf (polynomial-coefficients result)
	    (copy-seq (polynomial-coefficients poly)))
      (setf (polynomial-coefficients result)
	    (subseq (polynomial-coefficients result) 0 (1+ pos))))
    result))
	
(defun poly-add (p1 p2)
  (labels ((add-aux (p1 p2)
	     (cond ((null p1) p2)
		   ((null p2) p1)
		   (t (cons (+ (car p1) (car p2))
			    (poly-add (cdr p1) (cdr p2)))) )))
    (remove-trailing-zeroes (add-aux p1 p2))))

(defun remove-trailing-zeroes (poly)
  (let* ((rev-poly (reverse poly))
	 (pos (position-if-not #'zerop rev-poly)))
    (if pos
	(reverse (subseq rev-poly pos))
	(list 0))))

(defun polynomial-subtract (p1 p2)
  (let ((p3 (copy-polynomial p2)))
    (setf (polynomial-coefficients p3)
	  (map 'vector #'- (polynomial-coefficients p3)))
    (polynomial-add p1 p3)))

(defun poly-subtract (p1 p2)
  (poly-add p1 (mapcar #'- p2)))

(defun poly-derivative (poly)
  (labels ((derive (poly expt)
	     (cond ((null poly) ())
		   (t (cons (* (car poly) expt)
			    (derive (cdr poly) (1+ expt)))) )))
    (if (single poly)
	(list 0)
	(derive (cdr poly) 1))))
		

(defun derive ()
  (internal-to-external (poly-derivative (infix-to-internal (read)))))

;;;
;;;    Ex. 4.9
;;;
(defun zero-degreep (internal-poly)
  (or (single internal-poly)
      (every #'zerop (cdr internal-poly))))

(defun zero-degreep (internal-poly)
  (labels ((all-zero (l)
	     (cond ((null l) t)
		   ((zerop (car l)) (all-zero (cdr l)))
		   (t nil))))
    (all-zero (cdr internal-poly))))

(defun zero-degreep (internal-poly)
  (not (find-if-not #'zerop (cdr internal-poly))))

;;;
;;;    Ex. 4.11
;;;
;;;    Manage which operators are recognized. Each should be represented by a
;;;    list: (<op-symbol> <op-function-name> <arity>)
;;;    
(let ((poly-operators '((+ poly-add 2)
			(- poly-subtract 2)
			(* poly-multiply 2)
			(/ poly-divide 2)
			(df poly-derivative 1)
			(int poly-integral 1))))
  
  (defun operator (expr)
    (cadr (assoc (car expr) poly-operators)))

  (defun unaryp (op)
    (= (third (assoc op poly-operators)) 1))

  (defun add-operator (symbol function arity)
    (if (assoc symbol poly-operators)
	(warn "An entry for ~S already exists!~%" symbol)
	(progn (push (list symbol function arity) poly-operators) t)))

  (defun remove-operator (symbol)
    (if (assoc symbol poly-operators)
	(progn (setf poly-operators (delete symbol poly-operators :key #'car))
	       t)
	nil))

  (defun change-operator (symbol function arity)
    (if (assoc symbol poly-operators)
	(progn (remove-operator symbol)
	       (add-operator symbol function arity))
	(warn "There is no entry for ~S!~%" symbol))) )

(defun first-operand (expr)
  (external-to-internal (cadr expr)))

(defun second-operand (expr)
  (if (caddr expr)
      (external-to-internal (caddr expr))
      nil))

; (defun manipulate (expr)
;   (let ((operator (operator expr))
; 	(op1 (first-operand expr))
; 	(op2 (second-operand expr)))
;     (if (unaryp operator)
; 	(internal-to-external (funcall operator op1))
; 	(internal-to-external (funcall operator op1 op2)))) )

;;;
;;;    Handle multiple values (possible with division)
;;;    
(defun manipulate (expr)
  (multiple-value-bind (primary secondary)
      (apply (operator expr) (make-internal-list (operands expr)))
    (if secondary
	(values (internal-to-external primary)
		(internal-to-external secondary))
	(internal-to-external primary))))

(defun operands (expr)
  (cdr expr))

(defun make-internal-list (external-list)
  (cond ((null external-list) ())
	(t (cons (external-to-internal (car external-list))
		 (make-internal-list (cdr external-list)))) ))

(defun poly-multiply (p1 p2)
  (labels ((multiply-single (k p)
	     (mapcar #'(lambda (x)
			 (* k x))
		     p)))
    (if (single p1)
	(multiply-single (car p1) p2)
	(poly-add (multiply-single (car p1) p2)
		  (poly-multiply (cdr p1) (cons 0 p2)))) ))

;;;
;;;    Compute indefinite integral of polynomial. Assume constant of
;;;    integration is 0.
;;;
(defun poly-integral (poly)
  (labels ((integrate (poly expt)
	     (cond ((null poly) ())
		   (t (cons (/ (car poly) (1+ expt))
			    (integrate (cdr poly) (1+ expt)))) )))
    (cons 0 (integrate poly 0))))

;;;
;;;    We reverse the divisor/dividend to put higher-degree terms at front of
;;;    list. We then perform pseudo-polynomial subtraction and multiplication
;;;    on these reversed 'polynomials'.
;;;
;;;    Example:
;;;    6x**2 - 11x - 10 => (-10 -11 6) => (6 -11 -10)
;;;    3x + 2 => (2 3) => (3 2)
;;;
;;;              2  -5 => (-5 2)
;;;        ___________
;;;    3 2 | 6 -11 -10
;;;        -(6   4)                  (poly-multiply (2) (3 2))
;;;          0 -15 -10 => (-15 -10)  (Take CDR to ignore leading 0)
;;;          -(-15 -10)              (poly-multiply (-5) (3 2))
;;;              0   0 => (0)        (Take CDR)
;;;
;;;    Assumes dividend is evenly divisible by divisor!!
;;;    
; (defun poly-divide (p1 p2)
;   (labels ((divide (p1 p2)
; ; 	     (format t "~A~%" p1)
; ; 	     (read-line)
; 	     (cond ((null p1) ())
; 		   (t (let ((quotient (/ (car p1) (car p2))))
; 			(cons quotient
; 			      (divide (cdr (poly-subtract
; 					    p1
; 					    (poly-multiply
; 					     (list quotient) p2)))
; 				      p2)))) )))
;     (let ((quotient (reverse (divide (reverse p1) (reverse p2)))) )
;       (assert (equal p1 (poly-multiply quotient p2))
; 	      () "Doh! ~S ~S" p1 (poly-multiply quotient p2))
;       quotient)))
;;;
;;;    The above version fails when the dividend (p1) has no constant term.
;;;    (The POLY-SUBTRACT function collapses a list of 0's to a single
;;;    term (0)).
;;;    The version below also handles remainders. It returns the quotient as
;;;    the primary value and the remainder as a secondary value.
;;;    
;;;    [41]> (poly-divide '(0 12 9 0 -8 -2 3) '(0 4 3))
;;;    (3 0 0 -2 1) ;
;;;    (0)
;;;    [42]> (poly-divide '(0 12 9 0 -8 -2 3) '(1 4 3))
;;;    (44/27 10/9 -1/3 -2 1) ;
;;;    (-44/27 118/27)
;;;    
(defun poly-divide (p1 p2)
  (let ((length-p2 (length p2))) ;Invariant
    (labels ((divide (p1 p2 result)
	       (cond ((< (length p1) length-p2)
		      (values result (remove-trailing-zeroes (reverse p1))))
		     (t (let ((quotient (/ (car p1) (car p2))))
			  (divide (cdr (subtract p1
						 (mapcar #'(lambda (x)
							     (* x quotient))
							 p2)))
				  p2 (cons quotient result)))) ))
	     (subtract (p1 p2)
	       (cond ((null p2) p1)
		     (t (cons (- (car p1) (car p2))
			      (subtract (cdr p1) (cdr p2)))) )))
      (divide (reverse p1) (reverse p2) ()))) )

;;;
;;;    Kessler's version only works if divisor is a binomial!
;;;    
(defun poly-division (p1 p2)
  (cdr (poly-division-aux p1 p2)))
(defun poly-division-aux (p1 p2)
  (if (null (cddr p1))
      (compute-remainder-and-factor p1 p2)
      (let ((prev-term (poly-division-aux (cdr p1) p2)))
	(append (compute-remainder-and-factor (list (car p1)
						    (car prev-term))
					      p2)
		(cdr prev-term)))) )
(defun compute-remainder-and-factor (p1 p2)
  (let ((new-term (/ (cadr p1) (cadr p2))))
    (list (- (car p1) (* new-term (car p2)))
	  new-term)))

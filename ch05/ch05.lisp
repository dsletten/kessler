;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch05.lisp
;;;
;;;   STARTED:            Tue Aug  6 18:37:49 2002
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

(defun make-clown-object (name shoe-size expression)
  `((name ,name)
    (shoe-size ,shoe-size)
    (expression ,expression)))

(defun mood (clown)
  `(,(get-name clown) is ,(get-expression clown)))

(defun tickle (clown)
  (cons `(expression smiling) clown))

(defun biggest-feet (clown1 clown2)
  (if (> (get-shoe-size clown1)
	 (get-shoe-size clown2))
      (get-name clown1)
      (get-name clown2)))

;;;
;;;    Ex. 5.1
;;;    
(defun get-name (clown)
  (second (assoc 'name clown)))

(defun get-shoe-size (clown)
  (second (assoc 'shoe-size clown)))

(defun get-expression (clown)
  (second (assoc 'expression clown)))

;;;
;;;    Ex. 5.2
;;;
(defun change-shoes (clown new-size)
  (if (= (get-shoe-size clown) new-size)
      clown
      (cons (list 'shoe-size new-size) (remove 'shoe-size clown :key #'car))))

;;;
;;;    Structure implementation
;;;    
; (defstruct (clown (:print-function print-clown))
;   (name "" :type string)
;   (shoe-size 0 :type number)
;   (expression "smiling" :type string))

; (defun print-clown (clown stream depth)
;   (format stream "#<Clown ~A>" (clown-name clown)))

; (defgeneric show-mood (c)
;   (:method ((c clown))
; 	   (format nil "~A is ~A." (clown-name c) (clown-expression c))))

; (defgeneric tickle-clown (c)
;   (:method ((c clown))
; 	   (setf (clown-expression c) "smiling")))

; (defgeneric biggest-clown-feet (c1 c2)
;   (:method ((c1 clown) (c2 clown))
; 	   (if (> (clown-shoe-size c1)
; 		  (clown-shoe-size c2))
; 	       (clown-name c1)
; 	       (clown-name c2))))

; (defgeneric change-clown-shoes (c new-size)
;   (:method ((c clown) (new-size number))
; 	   (unless (= (clown-shoe-size c) new-size)
; 	     (setf (clown-shoe-size c) new-size))))


(defclass person ()
  ((name :initarg :name :accessor person-name)
   (mother :initarg :mother :accessor person-mother)
   (father :initarg :father :accessor person-father)
   (sex :initarg :sex :accessor person-sex)
   (parent :initarg :parent :accessor person-parent)
   (siblings :initarg :siblings :accessor person-siblings)))


;;;
;;;    5.4
;;;
(defclass dog ()
  ((name :initarg :name :reader dog-name)
   (breed :initarg :breed :initform '? :reader dog-breed)
   (tricks :initarg :tricks :initform () :accessor dog-tricks)
   (owner :initarg :owner :initform '? :accessor dog-owner)))

(defclass cat ()
  ((name :initarg :name :reader cat-name)
   (breed :initarg :breed :initform '? :reader cat-breed)
   (lives-remaining :initform 9 :accessor cat-lives)
   (owner :initarg :owner :initform '? :accessor cat-owner)))

;;;
;;;    5.6
;;;
(defclass clown ()
  ((name :initarg :name :reader clown-name)
   (shoe-size :initarg :shoe-size :accessor clown-shoe-size)
   (expression :initarg :expression :accessor clown-expression)))

(defmethod print-object ((object person) stream)
  (format stream "#<Person ~A>" (person-name object)))

;;;
;;;    5.8
;;;
(defmethod train ((d dog) trick)
  (cond ((member trick (dog-tricks d)) nil)
	(t (push trick (dog-tricks d)))) )

(defmethod trickp ((d dog) trick)
  (and (member trick (dog-tricks d)) t))

(defmethod showtime ((d dog))
  (randomize-list (dog-tricks d)))

(defun randomize-list (l)
  (labels ((randomize-list-aux (l n result)
	     (cond ((null l) result)
		   (t (multiple-value-bind (elt remnant)
			  (transfer l (random n))
			(randomize-list-aux remnant
					    (1- n)
					    (cons elt result)))) )))
    (if (null l)
	nil
	(randomize-list-aux l (length l) ()))) )

(defun transfer (l i)
  "Remove the ith element from a list, returning the element and the new list."
  (labels ((transfer-aux (l i result)
             (cond ((null l) (error "Could not transfer element"))
                   ((zerop i) (values (car l)
                                      (append (nreverse result) (cdr l))))
                   (t (transfer-aux (cdr l)
                                    (1- i)
                                    (cons (car l) result)))) ))
    (transfer-aux l i ())))

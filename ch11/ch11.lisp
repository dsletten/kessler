;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   NAME:               ch11.lisp
;;;;
;;;;   STARTED:            Wed Sep  4 00:35:29 2002
;;;;   MODIFICATIONS:
;;;;
;;;;   PURPOSE:
;;;;
;;;;
;;;;
;;;;   CALLING SEQUENCE:
;;;;
;;;;
;;;;   INPUTS:
;;;;
;;;;   OUTPUTS:
;;;;
;;;;   EXAMPLE:
;;;;
;;;;   NOTES:
;;;;
;;;;

;;;
;;;    Ex. 11.1/2
;;;    
(defstruct (player (:print-function print-player))
  hit-points
  initial-strength
  (weapons ())
  current-room
  (fighting ())
  name)

(defun print-player (player stream depth)
  (format t "[Player ~A]" (player-name player)))

;;;
;;;    Ex. 11.3
;;;
(defmethod number-of-weapons ((p player))
  (length (player-weapons p)))

;;;
;;;    Ex. 11.4
;;;    (See Kessler's version!)
;;;
(defmacro pl-defstruct (name &rest slots)
  (let ((expansion-list ()))
    (dolist (slot slots (cons 'progn (nreverse (cons `',name expansion-list))))
      (push `(progn (defun ,(car slot) (sym)
		      (get sym ',(car slot)))
	            (defun ,(cadr slot) (sym val)
		      (setf (get sym ',(car slot)) val))
	            (defsetf ,(car slot) ,(cadr slot)))
	    expansion-list))))

;;;
;;;    Chapter Exercises
;;;
(defun my-remove-duplicates (l)
  (labels ((remove-duplicates-aux (l result)
             (cond ((null l) result)
                   (t (remove-duplicates-aux (cdr l)
                                             (adjoin (car l) result)))) ))
    (remove-duplicates-aux l ())))

(let ((symbol-list '(ashbel smith pioneer doctor and leader in the development
		     of texas son of moses and phoebe adams smith was born at
		     hartford connecticut on august 13 1805 he has been called
		     quot the father of texas medicine quot and quot the father
		     of the university of texas quot he also made valuable
		     contributions to texas in the areas of politics diplomacy
		     agriculture and ranching warfare finance transportation
		     and immigration after graduating from hartford public
		     school and hartford grammar school smith attended yale
		     college by the time he was nineteen he had earned ab and
		     am degrees from yale where he was made a member of the phi
		     beta kappa honor society after a year and a half of
		     teaching in a private school in salisbury north carolina
		     he returned to yale to study medicine and earned the
		     degree of md in the spring of 1828 he did a subsequent
		     two-year stint of teaching)))
  (defun compare-times (n)
    (do ((i 1 (1+ i))
	 (a-list ())
	 (hash-table (make-hash-table)))
	(nil)
      (format t "~3D: " i)
      (format t "[EM:~D] " (time-form () n))
      (format t "[AS:"))))

;;;
;;;    Without optional SEXPR, simply time empty loop.
;;;    
(defmacro time-form (count &optional sexpr)
  (let ((index (gensym))
	(time0 (gensym)))
    `(let ((,time0 (get-internal-run-time)))
      (dotimes (,index ,count (- (get-internal-run-time) ,time0))
	,@(and sexpr (list sexpr)))) ))

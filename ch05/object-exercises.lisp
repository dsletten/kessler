;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               object-exercises.lisp
;;;
;;;   STARTED:            Thu Aug  8 18:16:37 2002
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

(load "../objects")

(defvar *unknown* '?)

(define-type person
  (:var name)
  (:var mother)
  (:var father)
  (:var sex)
  (:var parent (:init *unknown*))
  (:var sibling (:init *unknown*)))

;;;
;;;    Ex. 5.4/5.5
;;;
(define-type dog
  (:var name)
  (:var breed)
  (:var tricks)
  (:var owner))

(define-type cat
  (:var name)
  (:var breed)
  (:var lives-remaining (:init 9))
  (:var owner))

;;;
;;;    Ex. 5.6
;;;
(define-type clown
  (:var name)
  (:var shoe-size)
  (:var expression))

(define-method (person set-father) (new-dude)
  (setf father new-dude))

(define-method (person get-father) ()
  father)

; #'(LAMBDA (SELF)
;     (LET ((USER-FN (USER-FUNCTION 'GET-FATHER)))
;       (COND ((OBJECTP SELF)
; 	     (LET* ((TYPE-NAME (OBJECT-TYPE SELF))
; 		    (ACTUAL-NAME-AND-SELF
; 		     (GET-A-METHOD 'GET-FATHER TYPE-NAME SELF))
; 		    (TARGET-OBJECT (CDR ACTUAL-NAME-AND-SELF))
; 		    (ACTUAL-NAME (CAR ACTUAL-NAME-AND-SELF)))
; 	       (COND ((AND USER-FN (NOT ACTUAL-NAME))
; 		      (SETF ACTUAL-NAME USER-FN)
; 		      (SETF TARGET-OBJECT SELF))
; 		     ((NOT ACTUAL-NAME)
; 		      (ERROR "~S is not a known method for ~S"
; 			     'GET-FATHER TYPE-NAME)))
; 	       (APPLY ACTUAL-NAME (LIST TARGET-OBJECT))))
; 	    (USER-FN (APPLY USER-FN (LIST SELF)))
; 	    (T (ERROR "~S is not an object, invoked with ~S" SELF
; 		      'GET-FATHER)))))

;#'(LAMBDA (SELF) (LET ((USER-FN (USER-FUNCTION 'GET-FATHER))) (COND ((OBJECTP SELF) (LET* ((TYPE-NAME (OBJECT-TYPE SELF)) (ACTUAL-NAME-AND-SELF (GET-A-METHOD 'GET-FATHER TYPE-NAME SELF)) (TARGET-OBJECT (CDR ACTUAL-NAME-AND-SELF)) (ACTUAL-NAME (CAR ACTUAL-NAME-AND-SELF))) (COND ((AND USER-FN (NOT ACTUAL-NAME)) (SETF ACTUAL-NAME USER-FN) (SETF TARGET-OBJECT SELF)) ((NOT ACTUAL-NAME) (ERROR "~S is not a known method for ~S" 'GET-FATHER TYPE-NAME))) (APPLY ACTUAL-NAME (LIST TARGET-OBJECT)))) (USER-FN (APPLY USER-FN (LIST SELF))) (T (ERROR "~S is not an object, invoked with ~S" SELF 'GET-FATHER)))))


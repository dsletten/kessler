;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   NAME:               ch10.lisp
;;;;
;;;;   STARTED:            Sat Aug 31 10:27:04 2002
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

(defclass monster ()
  ((name :initarg :name :reader monster-name)
   (type :initarg :type :reader monster-type)
   (description :initarg :description :reader monster-description)
   (strength :initarg :strength :accessor monster-strength)
   (damage :initarg :damage :accessor monster-damage)
   (room :initarg :room :accessor monster-room)))

(defmethod print-object ((o monster) stream)
  (format stream "#<Monster ~A (~A)>" (monster-name o) (monster-type o)))

(defclass dragon (monster)
  ((number-of-heads :initarg :number-of-heads :reader dragon-heads)
   (fire-breath-temperature :initarg :fire-breath-temperature :accessor
			    dragon-breath-temperature)))

(defmethod print-object ((o dragon) stream)
  (format stream "#<Dragon ~A (~D head~:P)>" (monster-name o)
	  (dragon-heads o)))

;;;
;;;    Ex. 10.1
;;;
(let ((monster-options '("prepares to strike" "ignores your presence"
			 "twiddles its thumbs")))
  (defun monster-enhancer ()
    (format nil "The monster ~A." (nth (random (length monster-options))
				       monster-options))))

;;;
;;;    Ex. 10.2
;;;
(defclass evil-professor (monster)
  ((office-number :initarg :office-number :accessor professor-office)))

(defmethod print-presence ((m monster))
  (format t "~A is present.~%" (monster-description m)))

(defmethod print-presence ((d dragon))
  (format t "~A ~A is present.~%" (if (= (random 2) 0)
				      "Danger, Danger, Danger"
				      "Be careful")
	  (monster-description d)))

;;;
;;;    User command functions
;;;
(defun move (direction)
  (format t "Moving ~A~%" direction))

(defun take (command obj)
  (format t "Taking ~A~%" obj))

(defun drop (command obj)
  (format t "Dropping ~A~%" obj))

(defun fight (command monster)
  (format t "Fighting ~A -- Good luck.~%" monster))

(let ((zero-arg-commands '((north move)
			   (south move)
			   (east move)
			   (west move)))
      (one-arg-commands '((take take)
			  (drop drop)
			  (fight fight)))
      (quit-commands '(quit stop halt end)))

  (defun do-commands ()
    (do ((command (read) (read)))
	((member command quit-commands)
	 (format t "Leaving the game~%"))
      ;;  Fix this!
      (or ((lambda (command-function)
	     (when command-function
	       (funcall (second command-function) command)
	       t))
	   (assoc command zero-arg-commands))
	  ((lambda (command-function)
	     (when command-function
	       (funcall (second command-function) command (read))
	       t))
	   (assoc command one-arg-commands))
	  (format t "~A is an illegal command~%" command)))) )

(let ((zero-arg-commands '((#\n move)
			   (#\s move)
			   (#\e move)
			   (#\w move)))
      (one-arg-commands '((#\t take)
			  (#\d drop)
			  (#\f fight)))
      (quit-commands '(#\q #\h))
      (ignore-commands '(#\newline #\space)))

  (defun do-single-char-commands ()
    (do ((command (read-char) (read-char)))
	((member command quit-commands)
	 (format t "Leaving the game~%"))
      ;;  Fix this!
      (or (member command ignore-commands)
	  ((lambda (command-function)
	     (when command-function
	       (funcall (second command-function) command)
	       t))
	   (assoc command zero-arg-commands))
	  ((lambda (command-function)
	     (when command-function
	       (funcall (second command-function) command
			(read-from-string (read-line)))
	       t))
	   (assoc command one-arg-commands))
	  (format t "~A is an illegal command~%" command)))) )


;;;
;;;    Ex. 10.6
;;;
(defun strict-palindrome-p (phrase)
  (let* ((clean-phrase (string-right-trim ".!?" phrase))
	 (lower-case (string-downcase clean-phrase)))
    (equal lower-case (reverse lower-case))))

(defun palindromep (phrase)
  (strict-palindrome-p (remove-if-not #'alpha-char-p phrase)))

(defun strict-palindrome ()
  (strict-palindrome-p (read-line)))

(defun palindrome ()
  (palindromep (read-line)))

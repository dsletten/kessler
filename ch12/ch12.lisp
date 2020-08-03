;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   NAME:               ch12.lisp
;;;;
;;;;   STARTED:            Thu Sep  5 16:16:39 2002
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
   (description :initarg :description :reader monster-description)
   (initial-strength :initarg :initial-strength
		     :initform 0
		     :accessor monster-initial-strength)
   (initial-damage :initarg :initial-damage
		   :initform 0
		   :accessor monster-initial-damage)
   (hit-points :initarg :hit-points :accessor monster-hit-points)))

(defmethod print-object ((m monster) stream)
  (format stream "#<Monster ~A: ~A>" (monster-name m) (monster-description m)))

(defclass dragon (monster)
  ((number-of-heads :initarg :number-of-heads :reader dragon-number-of-heads)))

(defclass swarm (monster)
  ((count-of-other-swarm-in-room :initarg :count-of-other-swarm-in-room
				 :accessor swarm-swarm-count)
   (count-of-non-swarm-in-room :initarg :count-of-non-swarm-in-room
			       :accessor swarm-other-count)))

(defclass zombie (monster)
  ((years-since-death :initarg :years-since-death
		      :initform 0
		      :accessor zombie-years-since-death)))

(defmethod print-object ((z zombie) stream)
  (format stream "#<Zombie ~A: ~A (Dead for ~D year~:P)>"
	  (monster-name z) (monster-description z)
	  (zombie-years-since-death z)))

(defun init-monster (type name description strength damage)
  (make-instance type
		 :name name
		 :description description
		 :initial-strength strength
		 :initial-damage damage
		 :hit-points strength))

(defclass weapon ()
  ((name :initarg :name :reader weapon-name)
   (description :initarg :description :reader weapon-description)
   (damage :initarg :damage :reader weapon-damage)
   (lifetime :initarg :lifetime :accessor weapon-lifetime)))

(defmethod print-object ((w weapon) stream)
  (format stream "#<Weapon ~A: ~A" (weapon-name w) (weapon-description w))
  (when (numberp (weapon-lifetime w))
    (format stream " (~D use~:P left)" (weapon-lifetime w)))
  (format stream ">"))

(defclass special (weapon)
  ())

(defclass wand (weapon)
  ((incantation :initarg :incantation :reader wand-incantation)))

(defclass club (weapon)
  ())

(defclass cannon (weapon)
  ())

;;;
;;;    Ex. 12.1 (Accuracy 50% ...?)
;;;
(defclass dagger (weapon)
  ((length :initarg :length :reader dagger-length)))

(let ((weapon-initializations '((special 5 6)
				(wand 10 nil)
				(club 3 nil)
				(cannon 100 1)
				(dagger 5 nil))))
  (defun init-weapon (type name description)
    (let ((damage-and-lifetime (assoc type weapon-initializations)))
      (make-instance type
		     :name name
		     :description description
		     :damage (second damage-and-lifetime)
		     :lifetime (third damage-and-lifetime)))) )

(defclass room ()
  ((name :initarg :name :reader room-name)
   (description :initarg :description :reader room-description)
   (connections :initarg :connections :reader room-connections)
   (weapons :initarg :weapons :accessor room-weapons)
   (monsters :initarg :monsters :accessor room-monsters)))

(defstruct (player (:print-function print-player))
  name
  hit-points
  (weapons ())
  current-room
  (fighting ())
  initial-strength)

(defun print-player (player stream depth)
  (format t "[Player ~A]" (player-name player)))

;;;
;;;    Ex. 12.2
;;;
(defclass loner (monster)
  ((count-of-swarm-in-room :initarg :count-of-swarm-in-room
			   :accessor loner-swarm-count)
   (count-of-non-swarm-in-room :initarg :count-of-non-swarm-in-room
			       :accessor loner-other-count)))

(defclass blissterror (monster)
  ())

(defclass empath (monster)
  ())

(let ((dungeon (make-hash-table)))
  (defun add-object (name object)
    (setf (gethash name dungeon) object))

  (defun get-object (name)
    (gethash name dungeon))

  (defun initialize-dungeon ()
    (clrhash dungeon))

  (defun show-dungeon ()
    (maphash #'(lambda (name object)
		 (format t "~A: ~A~%" name object))
	     dungeon))
;;;
;;;    Ex. 12.3
;;;
  (defun consistency-check ()
    (maphash #'(lambda (key val)
		 (when (typep val 'room)
		   (dolist (connection (room-connections val))
		     (unless (gethash (second connection) dungeon)
		       (format t "Room ~A does not exist but room ~A connects to it."
			       (second connection) (room-name val))))
		   (dolist (weapon (room-weapons val))
		     (unless (gethash weapon dungeon)
		       (format t "Weapon ~A does not exist but room ~A contains it."
			       weapon (room-name val))))
		   (dolist (monster (room-monsters val))
		     (unless (gethash monster dungeon)
		       (format t "Monster ~A does not exist but room ~A contains it."
			       monster (room-name val)))) ))
	     dungeon))

;;;
;;;    Ex. 12.4
;;;    
  (defun cheat-sheet ()
    (maphash #'(lambda (key val)
		 (when (typep val 'room)
		   (let ((indent (length (symbol-name key))))
		     (format t "~&~A -> " key)
		     (dolist (connection (room-connections val))
		       (format t "~V,0T~A is to the ~A~%" (+ indent 4)
			       (second connection) (first connection)))) ))
	     dungeon))
  
;;;
;;;    Ex. 12.5
;;;    BACKTRACK-LIST records rooms we've already checked to avoid infinite
;;;    loops between interconnected rooms.
;;;

;;;
;;;    Find a chain of rooms from START to DEST.
;;;    
  (defun help-path1 (start dest)
    (labels ((find-path (room connection-list backtrack-list)
	       "Find a path from a ROOM connected to other rooms via a given CONNECTION-LIST while avoiding rooms already visited in BACKTRACK-LIST."
	       (let ((first-room (second (first connection-list))))
		 (cond ((null connection-list) nil)
		       ((eq first-room dest)
			(cons room (list first-room)))
		       ((member first-room backtrack-list)
			(find-path room (rest connection-list) backtrack-list))
		       (t (let ((path (find-path first-room
						 (room-connections
						  (get-object first-room))
						 (cons room backtrack-list))))
			    (if path
				(cons room path)
				(find-path room (rest connection-list)
					   backtrack-list)))) ))))
      (if (eq start dest)
	  t
	  (find-path start (room-connections (get-object start)) ()))) )
;;;
;;;    Find a chain of connections which lead from START to DEST.
;;;    (Return T if START and DEST are same.)
;;;    
  (defun help-path (start dest)
    (labels ((find-best-path (room connection-list backtrack-list)
	       (let ((next-room (second (first connection-list))))
		 (cond ((null connection-list) nil)
		       ((member next-room backtrack-list)
			(find-best-path room (rest connection-list)
					backtrack-list))
		       (t (let* ((path1 (find-path room (first connection-list)
						  backtrack-list))
				(path2
				 (or (when (eq room start) (format t "~A~%" path1))

					   (find-best-path room
						       (rest connection-list)
						       backtrack-list))))
			    (cond ((null path1) path2)
				  ((null path2) path1)
				  ((< (length path1) (length path2)) path1)
				  (t path2)))) )))
	     (find-path (room connection backtrack-list)
	       (let ((next-room (second connection)))
		 (cond ((eq next-room dest)
			(list connection))
		       (t (let ((path (find-best-path next-room
						      (room-connections
						       (get-object next-room))
						      (cons room
							    backtrack-list)))) 
			    (if path
				(cons connection path)
				nil)))) )))
      (if (eq start dest)
	  t
	  (find-best-path start (room-connections (get-object start)) ()))) )

;;;
;;;    Inadequate for dungeons with circularities (Will find path but not
;;;    necessarily shortest one.)
;;;    
;   (defun help-path (start dest)
;     (labels ((find-path (room connection-list backtrack-list)
; 	       (let ((first-room (second (first connection-list))))
; 		 (cond ((null connection-list) nil)
; 		       ((eq first-room dest)
; 			(list (first connection-list)))
; 		       ((member first-room backtrack-list)
; 			(find-path room (rest connection-list) backtrack-list))
; 		       (t (let ((path (find-path first-room
; 						 (room-connections
; 						  (get-object first-room))
; 						 (cons room
; 						       backtrack-list))))
; 			    (if path
; 				(cons (first connection-list) path)
; 				(find-path room (rest connection-list)
; 					   backtrack-list)))) ))))
;       (if (eq start dest)
; 	  t
; 	  (find-path start (room-connections (get-object start)) ()))) )
   )

(defmacro add-room (name description monsters weapons connections)
  `(add-object ',name (make-instance 'room
		                      :name ',name
		                      :description ,description
		                      :connections ',connections
		                      :weapons ',weapons
		                      :monsters ',monsters)))
    
(defmacro add-monster (name type description strength damage)
  `(add-object ',name
               (init-monster ',type ',name ,description ,strength ,damage)))

(defmacro add-weapon (name type description)
  `(add-object ',name (init-weapon ',type ',name ,description)))

(let (player)
  (defun start-game-aux (player-name strength initial-room)
    (setf player (make-player :name player-name
			      :hit-points strength
			      :initial-strength strength))
    (move-to-initial-room))) ;?????????????????????????????????????????

(defmacro start-game (player-name strength initial-room)
  `(start-game-aux ',player-name ,strength ',initial-room))

;;;
;;;    Test the dungeon code.
;;;    
(load "dungeon2")
(load "~/lisp/programs/utils")

(test 'help-path1 '(((i n) (I H F E D C B J K L N))
		    ((m g) (M L K J B C D E F G))
		    ((g m) (G F E D C B J K L M))
		    ((a n) (A C B J K L N))))

(test 'help-path '(((a n)
		    ((SOUTH C) (WEST B) (SOUTH J) (WEST K) (WEST L) (SOUTH N)))
		   ((i n)
		    ((NORTH H) (WEST F) (NORTH E) (NORTH D) (WEST D1) (WEST C)
		     (WEST B) (SOUTH J) (WEST K) (WEST L) (SOUTH N)))
		   ((m g)
		    ((SOUTH L) (EAST K) (EAST J) (NORTH B) (EAST C) (EAST D1)
		     (EAST D) (SOUTH E) (SOUTH F) (WEST G)))
		   ((g m)
		    ((EAST F) (NORTH E) (NORTH D) (WEST D1) (WEST C) (WEST B)
		     (SOUTH J) (WEST K) (WEST L) (NORTH M)))) )



;#!/usr/local/bin/clisp
;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   NAME:               ch08.lisp
;;;;
;;;;   STARTED:            Fri Aug 30 20:52:27 2002
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
;;;    Ex. 8.7
;;;
(defmacro my-dotimes ((var limit &optional result) &body body)
  (let ((lim (gensym)))
    `(do ((,var 0 (1+ ,var))
	  (,lim ,limit))
         ((= ,var ,lim) ,result)
       ,@body)))

; [15]> (macroexpand-1 '(my-dotimes (i 10 'foo) (format t "~A~%" i)))
; (DO ((I 0 (1+ I)) (#:G1079 10)) ((= I #:G1079) 'FOO) (FORMAT T "~A~%" I)) ;
; T
; [16]> (macroexpand-1 '(dotimes (i 10 'foo) (format t "~A~%" i)))
; (DO ((I 0 (1+ I))) ((>= I 10) 'FOO) (FORMAT T "~A~%" I)) ;
; T
; [17]> (macroexpand-1 '(dotimes (i (* 10 2) 'foo) (format t "~A~%" i)))
; (DO ((I 0 (1+ I)) (#:G1080 (* 10 2))) ((>= I #:G1080) 'FOO)
;  (FORMAT T "~A~%" I)) ;
; T
; [18]> (macroexpand-1 '(my-dotimes (i (* 10 2) 'foo) (format t "~A~%" i)))
; (DO ((I 0 (1+ I)) (#:G1081 (* 10 2))) ((= I #:G1081) 'FOO) (FORMAT T "~A~%" I)) ;
; T

;;;
;;;    Ex. 8.8/8.9
;;;
(defmacro my-dolist ((var l &optional result) &body body)
  (let ((list-var (gensym)))
    `(do* ((,list-var ,l (cdr ,list-var))
	   (,var (car ,list-var) (car ,list-var)))
          ((null ,list-var) ,result)
       ,@body)))

; [12]> (macroexpand-1 '(dolist (i l 'foo) (list i)))
; (DO* ((#:G1077 L (CDR #:G1077)) (I NIL)) ((ENDP #:G1077) 'FOO)
;  (DECLARE (LIST #:G1077)) (SETQ I (CAR #:G1077)) (LIST I)) ;
; T
; [13]> (macroexpand-1 '(my-dolist (i l 'foo) (list i)))
; (DO* ((#:G1078 L (CDR #:G1078)) (I (CAR #:G1078) (CAR #:G1078)))
;  ((NULL #:G1078) 'FOO) (LIST I)) ;
; T

;;;
;;;    This fails where special operators are involved, e.g., IF
;;;    (our-eval '(and (oddp 8) 12))
;;;    
(defun our-eval (expr)
  (cond ((symbolp expr) (symbol-value expr))
	((atom expr) expr)
	((macro-function (car expr))
	 (our-eval (macroexpand expr)))
	(t (apply (car expr) (mapcar #'our-eval (cdr expr)))) ))
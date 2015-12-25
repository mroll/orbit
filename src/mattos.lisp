(defpackage :mattos
  (:use :cl)
  (:export :newclass))

(in-package :mattos)


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro str (symb)
  (format nil "~A" symb))

(defmacro make-struct (preamble slots)
  `(defstruct ,preamble ,@slots))

(defmacro class-mac (classname)
  `(defmacro ,classname (varname)
     `(progn (defparameter ,varname (funcall #',(symb "MAKE-" (format nil "~A" ',classname))))
             (instance-mac ,,'varname ,',classname))))

(defmacro instance-mac (instance-name classname)
  `(defmacro ,instance-name (field &rest args)
     (let ((accessor (symb (str ,classname) "-" (format nil "~A" field)))
           (slot-var (gensym))
           (args-var (if args args '(nil))))
       `(let ((,slot-var (funcall #',accessor ,',instance-name)))
          (cond ((functionp ,slot-var) (if ,@args-var
                                         (funcall ,slot-var ,',instance-name ,@args-var)
                                         (funcall ,slot-var ,',instance-name))) 
                ((null ,@args-var) ,slot-var)
                (t (setf (,accessor ,',instance-name) ,@args-var)))))))

(defmacro newclass (preamble &rest slots)
  (let ((classname (if (listp preamble)
                     (car preamble)
                     preamble)))
    `(progn (make-struct ,preamble ,slots)
            (class-mac   ,classname))))

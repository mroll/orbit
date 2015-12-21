#!/usr/bin/env sbcl --script

;;;; build-image.lisp
;;;; build a customised sbcl image
;;;;
;;;; you can modify the package list by changing the *base-packages*
;;;; global variable. this is useful for building a custom binary with
;;;; your commonly used libraries already present (at the expense of
;;;; some memory) but will cut down on load times.

;; list of packages to install - it's at the top so it's easier to change
(defparameter *base-packages* '(cl-ncurses cl-utilities cffi trivial-gray-streams))

;;; load quicklisp and quickload the libraries to be baked in
;;; taken from quicklisp's addition to my .sbclrc
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun load-packages (package-list)
  (dolist (package (mapcar #'string package-list))
    (ql:quickload package)))

;; build the image
(defun build-image (image-name)
  (if (stringp image-name)
      (sb-ext:save-lisp-and-die image-name :executable t)
      (format t "invalid image name!~%")))

(defun validate-arg (arg)
  (format t "validating ~A~%" arg)
  (if (stringp arg)
      arg
      (progn
    (format t "invalid argument: ~A!~%" arg)
    (quit))))

;; grab the image name from the command line args
(defvar *image-name* (validate-arg (second sb-ext:*posix-argv*)))

(format t "building sbcl image ~A with ~A built in...~%"
    *base-packages* *image-name*)
(load-packages *base-packages*)
(format t "saving image...~%")
(build-image *image-name*)
(quit)          ; shouldn't get this far but just in case, die

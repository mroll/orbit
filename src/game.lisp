#!/usr/bin/env sbcl --noinform --core qlsblc --script

(load "curses.lisp")

(defpackage :space-age
  (:use :cl :curses :cl-utilities))

(in-package :space-age)

(defvar *main-menu* (make-hash-table))
(defvar *bodies*  (make-hash-table :test 'equal))
(defvar *actions* (make-hash-table :test 'equal))

(defparameter iw nil)
(defparameter pw nil)

(defmacro action (name fn)
  `(setf (gethash ,name *actions*) ,fn))

(defmacro body (name info)
  `(setf (gethash ,name *bodies*) ,info))

(defun de-ascii (ch)
  (- ch 48))

;; Graham's aif
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro while (condition &body body)
  `(loop while ,condition
      do (progn ,@body)))

(defun do-menu (menu)
  (printw (gethash 'text menu))
  (let ((choice (de-ascii (getch))))
    (while (not (gethash choice menu))
      (setf choice (de-ascii (getch))))
    (funcall (gethash choice menu))))

(defun start-screen (title main-menu)
  (connect-console)
  (printw title)
  (refresh)
  (do-menu main-menu)
  (close-console))

(defun create-win (h w starty startx)
  (refresh)
  (let ((localwin (newwin h w starty startx)))
    (box localwin 0 0)
    (wrefresh localwin)
    localwin))

(defun win-reset (w)
  (werase w)
  (box w 0 0)
  (wrefresh w))

(defvar *promptx* 2)
(defvar *prompty* 32)

(defvar *infoy* 2)
(defvar *infox* 2)

(defun prompt ()
  (win-reset pw)
  (mvwprintw pw 1 1 "> ")
  (wrefresh pw)
  (refresh)
  (mvwgetstr pw 1 3))

(defun handle (action)
  (let* ((fields (split-sequence #\Space action))
         (funcname (car fields))
         (args (cdr fields)))
    (aif (gethash funcname *actions*)
         (apply it args))))

(defun info-body (body-name)
  (gethash body-name *bodies*))

(defun play ()
  (setf iw (create-win 30 200 0 0))
  (setf pw (create-win 3  200 31 0))
  (mvplines *infoy* 2 "You are orbiting the star Aegis. You're currently at perihelion,
at a distance of close to 150,000,000,000 meters to the star.")
  (mvplines (+ *infoy* 2) 2 (info-body "Aegis"))
  (wrefresh iw)
  (refresh)
  (do ((action (prompt) (prompt)))
    ((equal action "quit"))
    (win-reset iw)
    (handle action)))

(setf (gethash 'text *main-menu*) "
Please choose an option:
    1) New Game
    2) Quit")

(setf (gethash 1 *main-menu*) #'play)
(setf (gethash 2 *main-menu*) #'close-console)

(setf (gethash "Aegis" *bodies*) "Aegis is a main-sequence star of
approximately 1.2 solar masses that has been burning hydrogen for
close to 7 billion years.  The Aegis system consists of four planets:
Enid, Opacus, Tempus, and Celerity.

(See individual info pages for details on each planet.)")

(action "info" #'(lambda (x) (aif (info-body x)
                                  (mvplines *infoy* *infox* it))))

(body "Enid" "Enid (planet)
-------------
Star: Aegis
Type: Gas Giant
Distance at perihelion: 300 billion meters
Satellites: 2
    - Moon: Steele
    - Moon: Bosphor")

(body "Tempus" "Tempus (planet)
---------------
Star: Aegis
Type: Habitable
Distance at perihelion: 170 billion meters
Satellites: None")


(start-screen "Welcome to Space Age" *main-menu*)

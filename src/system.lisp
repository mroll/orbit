#!/usr/bin/env sbcl --noinform --core qlsblc --script

; (load "lib/twobody.lisp")
(load "lib/mattos/mattos.lisp")
(load "lib/curses.lisp")
(load "lib/twobody.lisp")

(defpackage :space-age
  (:use :cl :curses :cl-utilities :mattos
        :twobody))

(in-package :space-age)

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))



; A body in motion will stay in motion at the same speed and with
; the same direction unless acted upon by an outside force.

(defparameter *timestep* 0.2)

(defobject body (name pos mass))

(defobject planet (orbit) :inherits body)

(defmeth planet update-pos (time)
         (setf pos (mapcar #'(lambda (c)
                               (round (/ c 10000000)))
                           (get-2d-coords orbit time))))

(body aegis1)
(aegis1 'pos '(0 0 0))
(aegis1 'mass (ee 2.047 30))
(aegis1 'name "aegis1")

(planet enid)
(enid 'mass (ee 3.3387 24))
(let ((enid-aegis1-mu (solve-mu (enid 'mass) (aegis1 'mass))))
  (enid 'orbit (list 0.0 0.2 102.94719 11.26064 122009599 enid-aegis1-mu 0)))
(enid 'name "enid")

(planet earth)
(earth 'mass (ee 5.976 24))
(let ((earth-aegis1-mu (solve-mu (earth 'mass) (aegis1 'mass))))
  (earth 'orbit (list 0.0 0.0167 102.94719 -11.26064 149597000 earth-aegis1-mu 0)))
(earth 'name "earth")

; i ecc w bigw a mu bigt

;;; Window Class
;;; -----------------------------------------------------------------

(defobject window (height width starty startx (box #\t) border winptr))

(defmeth window create ()
         (refresh)
         (setf winptr (newwin height width starty startx))
         (if box (box winptr 0 0))
         (wrefresh winptr))

(defmeth window reset ()
         (werase winptr)
         (if box (box winptr 0 0))
         (wrefresh winptr)
         (refresh))

(defmeth window display (text &optional x y)
         (if x
           (setf x (+ x startx))
           (setf x startx))
         (if y 
           (setf y (+ y starty))
           (setf y starty))
         (mvplines (+ border y) (+ border x) text)
         (wrefresh winptr)
         (refresh))

(defmeth window prompt ()
         (this 'reset)
         (mvwprintw winptr 1 1 "> ")
         (wrefresh winptr)
         (refresh)
         (mvwgetstr winptr 1 3))

;;; -----------------------------------------------------------------

(window info-window)
(info-window 'height 64)
(info-window 'width  140)
(info-window 'starty 0)
(info-window 'startx 0)
(info-window 'border 2)


(window prompt-window)
(prompt-window 'height 3)
(prompt-window 'width  140)
(prompt-window 'starty 65)
(prompt-window 'startx 0)
(prompt-window 'border 1)

;;; Grid Class
;;; -----------------------------------------------------------------

(defobject grid (x-axis y-axis) :inherits window)

(defmeth grid draw-axes ()
         (let ((midx (+ startx (/ width  2)))
               (midy (+ starty (/ height 2))))
           (move midy startx)
           (hline 0 width)
           (move starty midx)
           (vline 0 height)
           (mvaddspch midy midx *ACS_PLUS*)
           (wrefresh winptr)
           (refresh)))

(defmeth grid normal-x (x)
         (+ (+ startx (/ width  2)) x))

(defmeth grid normal-y (y)
         (- (+ starty (/ height 2)) y))

(defmeth grid plot (p)
         (let ((normal-x (this 'normal-x (car p)))
               (normal-y (this 'normal-y (cadr p))))
           (mvaddspch normal-y normal-x *ACS_BULLET*))
         (refresh))

(defmeth grid display (text &optional x y)
         (if x
           (setf x (this 'normal-x x))
           (setf x (this 'normal-x startx)))
         (if y
           (setf y (this 'normal-y y))
           (setf y (this 'normal-y starty)))
         (mvplines (+ border y) (+ border x) text)
         (refresh))

(defmeth grid plot-points (points)
         (dolist (p points)
           (this 'plot p))
         (wrefresh winptr)
         (refresh))

(defmeth grid update (points)
         (werase winptr)
         (this 'draw-axes)
         (this 'plot-points points)
         (move 0 0)
         (refresh))

(defmeth grid update-with-tags (tagged-points)
         (werase winptr)
         (this 'draw-axes)
         (this 'plot-tagged-points tagged-points)
         (move 0 0)
         (refresh))

(defmeth grid plot-tagged-point (tagged-p)
         (destructuring-bind (p tag) tagged-p
           (this 'plot p)
           (this 'display tag (1+ (car p)) (1+ (cadr p))))
         (refresh))

(defmeth grid plot-tagged-points (tagged-points)
         (dolist (p tagged-points)
           (this 'plot-tagged-point p))
         (wrefresh winptr)
         (refresh))


;;; -----------------------------------------------------------------

(grid system-grid)
(system-grid 'height 40)
(system-grid 'width  60)
(system-grid 'starty 0)
(system-grid 'startx 142)
(system-grid 'box nil)
(system-grid 'border 0)


(defparameter *bodies* (list #'earth #'enid))

(defun handle (input)
  (if (equal input "quit")
    #\t))

(defmacro input-loop (handler pw &body body)
  (let ((curr-char (gensym))
        (input (gensym))
        (chars-in-prompt (gensym))
        (quit (gensym)))
    `(progn (nodelay *stdscr* #\t)
            (cbreak)
            (do ((,input "")
                 (,curr-char nil)
                 (time 1 (+ time *timestep*))
                 (,chars-in-prompt 0)
                 (,quit nil))
              (,quit)
              (mvwprintw (,pw 'winptr) 1 2 "> ")
              (wrefresh (,pw 'winptr))
              (refresh)
              (setf ,curr-char (getch))
              (if (and (< ,curr-char 256) (> ,curr-char 0))
                (if (eql #\return (code-char ,curr-char))
                  (progn (setf ,quit (,handler ,input))
                         (,pw 'reset)
                         (setf ,input "")
                         (setf ,chars-in-prompt 0))
                  (progn (mvwaddch (,pw 'winptr) 1 (+ 4 ,chars-in-prompt) (code-char ,curr-char))
                         (wrefresh (,pw 'winptr))
                         (setf ,chars-in-prompt (1+ ,chars-in-prompt))
                         (setf ,input (concatenate 'string ,input (format nil "~a" (code-char ,curr-char)))))))
              (move 0 0)
              ,@body
              (sleep 0.1))
            (nodelay *stdscr* #\f)
            (cbreak))))

(defobject system-controller (bodies))

(defmeth system-controller update-positions (time)
         (let ((new-coords nil))
           (dolist (b bodies)
             (push (list (funcall b 'update-pos time)
                         (funcall b 'name))
                   new-coords))
           new-coords))

(system-controller sys-control)
(sys-control 'bodies *bodies*)

(defun play ()
  (input-loop handle prompt-window
              (system-grid 'update-with-tags (sys-control 'update-positions time))))

(defun main ()
  (connect-console)
  (info-window 'create)
  (prompt-window 'create)
  (system-grid 'create)
  (info-window 'display "Hello there. This is a test.")
  (play)
  (close-console))

(main)

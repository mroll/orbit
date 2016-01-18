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

;; Graham's aif
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun str->symb (string)
  (intern (string-upcase string)))

(defmacro change-system-view (new-sys)
  `(sys-control 'current-system (str->symb ,new-sys)))


; A body in motion will stay in motion at the same speed and with
; the same direction unless acted upon by an outside force.

(defparameter *timestep* 0.5)

(defobject body (name pos mass))

(defobject planet (orbit) :inherits body)

(defmeth planet update-pos (time)
         (setf pos (mapcar #'(lambda (c)
                               (round (/ c 10000000)))
                           (get-2d-coords orbit time))))

(body sol)
(sol 'pos '(0 0 0))
(sol 'mass (ee 2.047 30))
(sol 'name "sol")

(planet mercury)
(mercury 'mass (ee 0.330 24))
(let ((mercury-mu (solve-mu (mercury 'mass) (sol 'mass))))
  (mercury 'orbit (list 7.0 0.205 77.45645 48.33167 (ee 57.91 6) mercury-mu 0)))
(mercury 'name "mercury")

(planet venus)
(venus 'mass (ee 4.87 24))
(let ((venus-mu (solve-mu (venus 'mass) (sol 'mass))))
  (venus 'orbit (list 3.39 0.0067 131.53298 76.68069 (ee 108.21 6) venus-mu 0)))
(venus 'name "venus")

(planet earth)
(earth 'mass (ee 5.976 24))
(let ((earth-mu (solve-mu (earth 'mass) (sol 'mass))))
  (earth 'orbit (list 0.0 0.0167 102.94719 -11.26064 (ee 149.6 6) earth-mu 0)))
(earth 'name "earth")

(planet mars)
(mars 'mass (ee 0.64174 24))
(let ((mars-mu (solve-mu (mars 'mass) (sol 'mass))))
  (mars 'orbit (list 1.850 0.0935 336.04084 49.57854 (ee 227.92 6) mars-mu 0)))
(mars 'name "mars")

(defobject moon () :inherits planet)

(defmeth moon update-pos (time)
         (setf pos (mapcar #'(lambda (c)
                               (round (/ c 20000)))
                           (get-2d-coords orbit time))))

(moon luna)
(luna 'mass (ee 0.07342 24))
(let ((luna-mu (solve-mu (luna 'mass) (earth 'mass))))
  (luna 'orbit (list 5.145 0.0549 100.7 40.94852 (ee 384.748 3) luna-mu 0)))
(luna 'name "luna")

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
(info-window 'width  100)
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
         (destructuring-bind ((x y) tag) tagged-p
           (this 'plot (list x y))
           (this 'display tag (1+ x) (1+ y)))
         (refresh))

(defmeth grid plot-tagged-points (tagged-points)
         (dolist (p tagged-points)
           (this 'plot-tagged-point p))
         (wrefresh winptr)
         (refresh))


;;; -----------------------------------------------------------------

(grid system-grid)
(system-grid 'height 60)
(system-grid 'width  100)
(system-grid 'starty 0)
(system-grid 'startx 102)
(system-grid 'box nil)
(system-grid 'border 0)


(defparameter *planets* (list #'mercury #'venus #'earth #'mars))
(defparameter *earth-system* (list #'luna))
(defparameter *actions* (make-hash-table))

(defmacro action (name func)
  `(setf (gethash ',name *actions*) ,func))

(action quit #'(lambda () 'quit))
(action zoom #'(lambda (body)
                 (change-system-view body)))

(defun handle (action)
  (let* ((fields (split-sequence #\Space action))
         (funcname (str->symb (car fields)))
         (args (cdr fields)))
    (aif (gethash funcname *actions*)
         (if args
            (apply it args)
            (funcall it)))))

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
                  (progn (setf ,quit (eq 'quit (,handler ,input)))
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

(defobject system-controller (systems current-system))

(defmeth system-controller update-positions (time syskey)
         (let ((new-coords nil))
           (dolist (b (gethash syskey (this 'systems)))
             (push (list (funcall b 'update-pos time)
                         (funcall b 'name))
                   new-coords))
           new-coords))

(system-controller sys-control)
(sys-control 'systems (make-hash-table))
(setf (gethash 'sol-system (sys-control 'systems)) *planets*)
(setf (gethash 'earth-system (sys-control 'systems)) *earth-system*)
(sys-control 'current-system 'sol-system)

(defun play ()
  (input-loop handle prompt-window
              (system-grid 'update-with-tags (sys-control 'update-positions
                                                          time
                                                          (sys-control 'current-system)))))

(defun main ()
  (connect-console)
  (info-window 'create)
  (prompt-window 'create)
  (system-grid 'create)
  (info-window 'display "Hello there. This is a test.")
  (play)
  (close-console))

; (format t "~a~%" (handle "quit"))

(main)

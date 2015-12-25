; #!/usr/bin/env sbcl --noinform --core qlsblc --script

(load "curses.lisp")

(defpackage :grid
  (:use :cl :curses :cl-utilities)
  (:export
    :grid :draw-axes :animate))

(in-package :grid)

(defvar *A_ALTCHARSET* 4194304)
(defvar *ACS_PLUS* 110)
(defvar *ACS_DIAMOND* 96)
(defvar *ACS_DEGREE* 176)
(defvar *ACS_BULLET* 102)


(defun grid (h w starty startx)
  (list h w starty startx))

(defun height (g) (nth 0 g)) 
(defun width  (g) (nth 1 g)) 
(defun starty (g) (nth 2 g)) 
(defun startx (g) (nth 3 g))

(defun x-axis (y x w)
  (move y x)
  (hline 0 w))

(defun y-axis (y x h)
  (move y x)
  (vline 0 h))

(defun mvaddspch (y x ch)
  (move y x)
  (attron *A_ALTCHARSET*)
  (c-addch ch)
  (attroff *A_ALTCHARSET*))

(defun draw-axes (g)
  (let ((midx (+ (startx g) (/ (width g) 2)))
        (midy (+ (starty g) (/ (height g) 2))))
    (x-axis midy (startx g) (width g))
    (y-axis (starty g) midx (height g))
    (mvaddspch midy midx *ACS_PLUS*)
    (refresh)))

(defun plot-point (g p)
  (let ((normal-x (+ (+ (startx g)
                        (/ (width g) 2)) (car p)))
        (normal-y (- (+ (starty g)
                        (/ (height g) 2)) (cadr p))))
    (mvaddspch normal-y normal-x *ACS_BULLET*))
  (refresh))

(defun plot-points (g pts)
  (when (not (null pts))
    (plot-point g (car pts))
    (plot-points g (cdr pts))))

(defun legend (g label val)
  (mvprintw (+ (height g) (starty g)) (startx g) label)
  (mvprintw (+ (height g) (starty g)) (+ (startx g) (length label) 1) val))

(defun animate (grid funcs tmax step-size)
  (loop for time from 0 to tmax by step-size do
        ; (erase)
        (draw-axes grid)
        (plot-points grid (map 'list #'(lambda (f)
                                         (funcall f time)) funcs))
        (legend grid "time: " (format nil "~,4F" time))
        (sleep 0.10)))

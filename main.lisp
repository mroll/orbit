#!/usr/bin/env sbcl --noinform --core qlsblc --script

(load "twobody.lisp")
(load "grid.lisp")

(defun ee (scalar pow)
  (* scalar (expt 10 pow)))

; newbody args: mass initial-pos


; mass is in KILOGRAMS
; distance is in KILOMETERS
(newbody earth (ee 5.976  24) '(147999999 0 0))
(newbody sol   (ee 1.9891 30) '(0 0 0))

; orbit args: i ecc a w bigw bigt
(make-orbit earth-orbit earth 0.00005 0.0167 149597000 102.94719 -11.26064 0)

(defun main ()
  (curses:connect-console)
  (let ((g (grid:grid 30 100 10 75))
        (f #'(lambda (time)
               (multiple-value-bind (x y) (get-2d-coords earth-orbit time)
                 (list (round (/ x 10000000)) (round (/ y 10000000)))))))
    (grid:draw-axes g)
    (grid:animate g (list f) 1000 1))
  (curses:getch)
  (curses:close-console))

(main)
; (write-2d-orbit earth-orbit "earth.orbit" 1000 1)

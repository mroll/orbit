#!/usr/bin/env sbcl --noinform --core qlsblc --script

(load "twobody.lisp")
(load "grid.lisp")

(defun ee (scalar pow)
  (* scalar (expt 10 pow)))

; newbody args: mass initial-pos


; mass is in KILOGRAMS
; distance is in KILOMETERS
(newbody mercury (ee 3.3346 23))
(newbody venus   (ee 4.8676 24))
(newbody earth   (ee 5.976  24))
(newbody sol     (ee 1.9891 30))

; orbit args: i ecc a w bigw bigt
; unit time: DAYS
(make-orbit earth-orbit   earth   0.0 0.0167 149597000 102.94719 -11.26064 0)
(make-orbit mercury-orbit mercury 7.0 0.2056 57910000  77.45645  48.33167  0)
(make-orbit venus-orbit   venus   3.4 0.0067 108210000 131.53298 76.68069  0)

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

; (main)
(write-3d-orbit earth-orbit   "earth.orbit"   1000 1)
(write-3d-orbit mercury-orbit "mercury.orbit" 1000 1)
(write-3d-orbit venus-orbit   "venus.orbit"   1000 1)

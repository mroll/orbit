; Fourth order Runge-Kutta method for approximating
; differential equations over intervals
;

(defun rk4 (f tmin tmax y0 h)
  (do* ((hh (/ h 2))
        (h6 (/ h 6))
        (xn tmin (+ xn h))
        (yn y0 (+ yn (/ k1 6) (/ k2 3) (/ k3 3) (/ k4 6)))
        (k1 (* h (funcall f xn yn)))
        (k2 (* h (funcall f (+ xn hh) (+ yn (/ k1 2)))))
        (k3 (* h (funcall f (+ xn hh) (+ yn (/ k2 2)))))
        (k4 (* h (funcall f (+ xn h) (+ yn k3))))
        (yout nil (push yn yout)))
    ((> xn tmax) yout)))

(load "twobody.lisp")

(newbody deathstar 30 '(-7 5 -5))
(newbody sol 100000 '(0 0 0))

; orbit args: i ecc w bigw bigt
(make-orbit deathstar-orbit deathstar 30 0.9 4 2.4 1)

(loop for i from 0 to 1000 by pi do
      (multiple-value-bind (x y) (get-2d-coords deathstar-orbit i)
        (format t "~,3F ~,3F~%" x y)))

(defun get-x (pt)
  (car pt))

(defun get-y (pt)
  (cadr pt))

(defun sqr (n)
  (* n n))

(defun distance (p1 p2)
  (let ((delta_x (- (get-x p2) (x p1)))
        (delta_y (- (get-y p2) (y p1))))
    (sqrt (+ (sqr delta_x) (sqr delta_y)))))

(defun vlen (v)
  (distance v '(0 0)))

(defun er (theta)
  (list (cos theta) (sin theta)))

(defun et (theta)
  (list (- 0 (sin theta)) (cos theta)))

(defun dotproduct (x v)
  (loop for y in v
        collect (* x y)))

(defun r (radius theta)
  (dotproduct radius (er theta)))

;(loop for i from 1 to 20 by 0.1 do
;      (let ((pt (r 1 i)))
;        (format t "~A ~A~%" (get-x pt) (get-y pt))))

; for a planet with position vector _r_,
; the its equation of motion about a single
; center of gravity, M, is given by:
;
;       d''r    -GM 
;       ---- = ----- * _r_
;       dt^2    r^3
;
;  where r is the length of _r_, G is the
;  gravitational constant, and M is the
;  solar mass.
;
;  Polar Coordinates
;  -----------------
;
;  We can determine the instantaneous position
;  of our planet in the x,y plane in terms of
;  standard Cartesian coordinates, (x, y), or
;  polar coordinates, (r, theta). Here,
;  r = sqrt(x^2 + y^2) and theta = tan^-1(y/x).
;  It is helpful to define two unit vectors,
;  er = _r_/r and et = ez x er, at the instant-
;  aneous position of the planet. The first
;  always points radially away from the origin,
;  whereas the second is normal to the first, in
;  the direction of increasing theta. The
;  Cartesian components of er and et are
;
;       er = (cos(theta), sin(theta))       (218)
;       et = (-sin(theta), cos(theta))      (219)
;
;  respectively.
;
;  We can write the position vector of our
;  planet as
;
;       _r_ = r * er.                       (220)
;
;  Thus, the planet's velocity becomes
;
;             d_r_
;       _v_ = ---- = (r' * er) + (r * er'). (221)
;              dt
;
;  Note that er has a non-zero time-derivative
;  because its direction _changes_ as the planet
;  moves around. As is easily demonstrated, from
;  differentiating (218) with respect to time,
;
;       er' = theta'(-sin(theta), cos(theta))
;           = theta' * et.
;
;  Thus,
;
;       _v_ = (r' * er) + (r * theta' * et).
;
;  Now, the planet's acceleration is written
;
;           d_v_   d^2_r_
;       a = ---- = ------ 
;            dt     dt^2
;
;         = (r''*er) + (r'*er') + (r'theta' + r*theta'')*et + (r*theta'et').
;
;  Again, et has a non-zero time-derivative
;  because its direction _changes_ as the planet
;  moves around.
;
;  Differentiation of Equation (219) with respect
;  to time yields
;
;       et' = theta'*(-cos(theta), -sin(theta))
;           = -theta'*er.
;
;  Hence,
;
;       _a_ = (r'' - r*theta'^2)*er + (r*theta'' + 2*r'*theta')*et.
;
;  It follows that the equation of motion of
;  our planet, (212), can be written
;
;       _a_ = (r'' - r*theta'^2)*er + (r*theta'' + 2*r'*theta')*et
;             -GM
;           = --- * er.
;             r^2
;
;  Since er and et are mutually orthogonal, we
;  can separately equate the coefficients of both,
;  in the above equation, to give a
;  _radial equation of motion_,
;
;       r'' - r*r*theta'^2
;         -GM
;       = --- * er.
;         r^2
;
;  And a _tangential equation of motion_,
;
;       r*theta'' + r*r'*theta' = 0.
;
;

(defun ship (m a ecc _t)
  (list m a ecc _t))

(defun orbit (i ecc w bigw a mu bigt)
  (list i ecc w bigw a mu bigt))

(defun get-i (orbit) (car orbit))
(defun get-ecc (orbit) (cadr orbit))
(defun get-w (orbit) (caddr orbit))
(defun get-bigw (orbit) (cadddr orbit))
(defun get-a (orbit) (nth 4 orbit))
(defun get-mu (orbit) (nth 5 orbit))
(defun get-bigt (orbit) (nth 6 orbit))

(defun solve-n (mu a)
  (* (sqrt mu) (expt a (/ -3 2))))

(defun solve-M (n time bigt)
  (* n (- time bigt)))

(defun solve-V (mu r a)
  (sqrt (* mu (- (/ 2 r) (/ 1 a)))))

(defun solve-r (a ecc E)
  (* a (- 1 (* ecc (cos E)))))

(defun Kepler (M ecc)
  (do ((i 0 (1+ i))
       (ecc-rads (/ (* 180 ecc) pi) ecc-rads)
       (E M (+ M (* ecc-rads (sin E)))))
    ((> i 5) E)))

(defun a (ship)
  (cadr ship))

(defun ecc (ship)
  (caddr ship))

(defun bigt (ship)
  (cadddr ship))

(defun to-coord (decimal)
  (multiple-value-bind (deg tmp) (floor decimal)
    (multiple-value-bind (min frac) (floor (* 60 tmp))
      (values deg min (truncate (* 60 frac))))))

(defun solve-x (r w bigw f i)
  (* r (- (* (cos W) (cos (+ w f)))
          (* (sin W) (sin (+ w f)) (cos i)))))

(defun solve-y (r w bigw f i)
  (* r (+ (* (cos W) (cos (+ w f)))
          (* (sin W) (sin (+ w f)) (cos i)))))

(defun solve-z (r w f i)
  (* r (sin (+ w f)) (sin i)))

(defun solve-f (E ecc)
  (* 2 (atan (* (sqrt (- 1 ecc)) (cos (/ E 2)))
             (* (sqrt (+ 1 ecc)) (sin (/ E 2))))))

(defun solve-r (orbit f)
  (let* ((e (get-ecc orbit))
         (a (get-a orbit))
         (p (* a (- (sqr e) 1))))
    (/ p (+ 1 (* e (cos f))))))

(defun get-3d-coords (r w bigw f i)
  (let ((x (solve-x r w bigw f i))
        (y (solve-y r w bigw f i))
        (z (solve-z r w f i)))
    (list x y z)))

(defun get-true-anomaly (orbit time)
  (let ((E (Kepler (solve-M (solve-n (get-mu orbit)
                                     (get-a orbit))
                            time
                            (get-bigt orbit))
                   (get-ecc orbit))))
    (solve-f E (get-ecc orbit))))

(defun spatial-orientation (orbit time)
  (let ((f (get-true-anomaly orbit time))
        (w (get-w orbit))
        (bigw (get-bigw orbit))
        (i (get-i orbit)))
    (values-list (get-3d-coords (solve-r orbit f) w bigw f i))))

(defun getpos (ship time mu)
  (let* ((ecc (ecc ship))
         (a (a ship))
         (n (solve_n mu (a ship)))
         (M (solve_M n time (bigt ship)))
         (E (Kepler M ecc))
         (r (solve_r a ecc E))
         (V (solve_V mu r a)))
    (values r V n M E)))

; orbit args: i ecc w bigw a mu bigt
(defvar o1 (orbit 30 0.9 4 10 2.4 20 1))
(defvar o2 (orbit 10 0.75 2.4 40 4 60 4))

; (loop for x from 0 upto 100 by 0.1 do
;       (multiple-value-bind (x y z) (spatial-orientation o1 x)
;         (format t "~,3F ~,3F ~,3F~%" x y z)))

(loop for x from 0 upto 100 by 0.1 do
      (multiple-value-bind (x y z) (spatial-orientation o2 x)
        (format t "~,3F ~,3F ~,3F~%" x y z)))

(defpackage :twobody
  (:use cl)
  (:export :ee :get-2d-coords :solve-mu :orbit
           :solve-E :solve-n :solve-M))

(in-package :twobody)

; LIST OF SPECIAL TERMS:
; ----------------------
; 
; i    -> inclination
; ecc  -> eccentricity
; w    -> argument of periapsis (angle of body's ascending node
;                                to its periapsis).
; bigw -> longitude of the ascending node (angle from reference
;                                          direction to the
;                                          ascending node.
; a    -> semimajor axis
; mu   -> G(m1 + m2)... (m1 is the mass on the orbit and m2 is
;                        the mass it orbits around.)
; bigt -> time of perihelion passage.

; (defmacro newbody (name mass)
;   `(defparameter ,name (body ,mass)))

; the semi-major axis, a, will be defined by the distance
; from the star to the initial position of the body.
(defmacro make-orbit (body star i ecc a w bigw bigt)
  (let ((muvar (gensym)))
    `(let ((,muvar (solve-mu (funcall ,body 'mass)
                             (funcall ,star 'mass))))
       (orbit ,i ,ecc ,w ,bigw ,a ,muvar ,bigt))))

(defun newtons-method (f fp p0 tol max-iter)                                                                                     
  (flet ((nextp (p_n-1) (- p_n-1 (/ (funcall f p_n-1)                                                                            
                                    (funcall fp p_n-1)))))                                                                       
    (do ((i 0 (1+ i))                                                                                                            
         (old-p p0 p)                                                                                                            
         (p (nextp p0) (nextp p)))                                                                                               
      ((or (> i max-iter) (< (abs (- p old-p)) tol)) p))))  

(defun ee (scalar pow)
  (* scalar (expt 10 pow)))

(defun get-x (pt)
  (car pt))

(defun get-y (pt)
  (cadr pt))

(defun get-z (pt)
  (caddr pt))

(defun sqr (n)
  (* n n))

(defun distance (p1 p2)
  (let ((delta-x (- (get-x p2) (get-x p1)))
        (delta-y (- (get-y p2) (get-y p1))))
    (sqrt (+ (sqr delta-x) (sqr delta-y)))))

(defun distance3d (p1 p2)
  (let ((delta-x (- (get-x p2) (get-x p1)))
        (delta-y (- (get-y p2) (get-y p1)))
        (delta-z (- (get-z p2) (get-z p1))))
    (sqrt (+ (sqr delta-x) (sqr delta-y) (sqr delta-z)))))

; m^3 * kg^-1 * s^-2
(defvar *G* (* 6.674 (expt 10 -11)))

; m: KILOGRAMS
(defun body (m)
  (list m))

(defun get-mass (body)
  (car body))

(defun get-initial-pos (body)
  (cadr body))

(defun orbit (i ecc w bigw a mu bigt)
  (list i ecc w bigw a mu bigt))

(defun get-i (orbit) (car orbit))
(defun get-ecc (orbit) (cadr orbit))
(defun get-w (orbit) (caddr orbit))
(defun get-bigw (orbit) (cadddr orbit))
(defun get-a (orbit) (nth 4 orbit))
(defun get-mu (orbit) (nth 5 orbit))
(defun get-bigt (orbit) (nth 6 orbit))

; m1, m2: KILOGRAMS
(defun solve-mu (m1 m2)
  (* *G* (+ m1 m2)))

; a: METERS
;
; returns P: DAYS
(defun orbital-period (a mu)
  (/ (* 2 pi (sqrt (/ (expt a 3) mu)))
     60 60 24))

; this implementation give n in terms of PER-UNIT MASS.
; this is probably not what we want for this problem.
; (defun solve-n (mu a)
;   (* (sqrt mu) (expt a (/ -3 2))))


; this implementation gives in in terms of RADIANS PER
; UNIT TIME. I'd say these units fit our purposes better.

; a: METERS
;
; returns n: RADIANS / DAY
(defun solve-n (mu a)
  (let ((P (orbital-period a mu)))
    (/ (* 2 pi) P)))

; n:    RADIANS / DAY
; time: DAYS
; bigt: should be zero for testing.
;       later there will likely be a universal start date,
;       at which times will go from.
(defun solve-M (n time bigt)
  (* n (- time bigt)))

(defun solve-V (mu r a)
  (sqrt (* mu (- (/ 2 r) (/ 1 a)))))

(defun to-deg (rads) (* (/ 180 pi) rads))
(defun to-rad (degs) (* (/ pi 180) degs))

; M:   RADIANS
; ecc: scalar
;
; returns E: RADIANS
(defun solve-E (M ecc)
  (let ((E M)
        (max-iter 30)
        (tol 0.00000000010))
    (flet ((f  (x) (- x (* ecc (sin x)) M))
           (fp (x) (- 1 (* ecc (cos x)))))
      (newtons-method #'f #'fp E tol max-iter))))

(defun solve-x (r w bigw f i)
  (* r (- (* (cos bigw) (cos (+ w f)))
          (* (sin bigw) (sin (+ w f)) (cos i)))))

(defun solve-y (r w bigw f i)
  (* r (+ (* (cos bigw) (cos (+ w f)))
          (* (sin bigw) (sin (+ w f)) (cos i)))))

(defun solve-z (r w f i)
  (* r (sin (+ w f)) (sin i)))

; E:   RADIANS
; ecc: scalar
;
; returns f: RADIANS
(defun solve-f (E ecc)
  (* 2 (atan (* (sqrt (+ 1 ecc)) (sin (/ E 2)))
             (* (sqrt (- 1 ecc)) (cos (/ E 2))))))

; f: RADIANS
;
; returns r: KILOMETERS
(defun solve-r (orbit f)
  (let ((a (get-a orbit))
        (ecc (get-ecc orbit)))
    (/ (* a (- (sqr ecc) 1))
       (+ 1 (* ecc (cos f))))))

(defun get-3d-coords (orbit time)
  (let* ((w (get-w orbit))
        (mu (get-mu orbit))
        (a (get-a orbit))
        (bigw (get-bigw orbit))
        (bigt (get-bigt orbit))
        (i (get-i orbit))
        (ecc (get-ecc orbit))
        (n (solve-n mu a))
        (M (solve-M n time bigt))
        (E (solve-E M ecc))
        (f (to-deg (solve-f (to-rad E) ecc)))
        (r (solve-r orbit (to-rad f))))
    (values (solve-x r w bigw f i)
            (solve-y r w bigw f i)
            (solve-z r w f i))))

(defun km->m (km)
  (* 1000 km))

; time: DAYS
(defun get-2d-coords (orbit time)
  (let* ((w (get-w orbit))
         (mu (get-mu orbit))
         (a (get-a orbit))
         (bigw (get-bigw orbit))
         (bigt (get-bigt orbit))
         (i (get-i orbit))
         (ecc (get-ecc orbit))
         (n (solve-n mu (km->m a)))
         (M (solve-M n time bigt))
         (E (solve-E M ecc))
         (f (solve-f E ecc))
         (r (solve-r orbit f)))
    (list (* r (cos f))
          (* r (sin f)))))

(defun write-3d-orbit (orbit fname steps step-size)
  (with-open-file (fp fname
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create)
    (loop for i from 0 upto steps by step-size do
          (multiple-value-bind (x y z) (get-3d-coords orbit i)
            (format fp "~,3F ~,3F ~,3F~%" x y z)))))

(defun write-2d-orbit (orbit fname steps step-size)
  (with-open-file (fp fname
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create)
    (loop for i from 0 upto steps by step-size do
          (multiple-value-bind (x y) (get-2d-coords orbit i)
            (format fp "~,3F ~,3F~%" (/ x 1000000) (/ y 1000000))))))

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

(defmacro newbody (name mass initial-position)
  `(defparameter ,name (body ,mass ,initial-position)))

; the semi-major axis, a, will be defined by the distance
; from the star to the initial position of the body.
(defmacro make-orbit (name body i ecc w bigw bigt)
  (let ((muvar (gensym))
        (avar  (gensym)))
    `(let ((,muvar (solve-mu (get-mass ,body)
                             (get-mass sol)))
           (,avar  (solve-a ,body sol)))
       (defparameter ,name (orbit ,i ,ecc ,w ,bigw ,avar ,muvar ,bigt)))))

(defmacro make-orbit2 (name body1 body2 i ecc w bigw bigt)
  (let ((muvar (gensym))
        (avar  (gensym)))
    `(let ((,muvar (solve-mu (get-mass ,body1)
                             (get-mass ,body2)))
           (,avar  (solve-a ,body1 ,body2)))
       (defparameter ,name (orbit ,i ,ecc ,w ,bigw ,avar ,muvar ,bigt)))))

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

(defvar *G* (* 6.674 (expt 10 -11)))

(defun body (m initial-pos)
  (list m initial-pos))

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

(defun solve-mu (m1 m2)
  (* *G* (+ m1 m2)))

(defun solve-n (mu a)
  (* (sqrt mu) (expt a (/ -3 2))))

(defun solve-M (n time bigt)
  (* n (- time bigt)))

(defun solve-V (mu r a)
  (sqrt (* mu (- (/ 2 r) (/ 1 a)))))

(defun solve-a (body1 body2)
  (distance3d (get-initial-pos body1)
              (get-initial-pos body2)))

(defun solve-E (M ecc)
  (do ((i 0 (1+ i))
       (ecc-rads (/ (* 180 ecc) pi) ecc-rads)
       (E M (+ M (* ecc-rads (sin E)))))
    ((> i 5) E)))

(defun solve-x (r w bigw f i)
  (* r (- (* (cos bigw) (cos (+ w f)))
          (* (sin bigw) (sin (+ w f)) (cos i)))))

(defun solve-y (r w bigw f i)
  (* r (+ (* (cos bigw) (cos (+ w f)))
          (* (sin bigw) (sin (+ w f)) (cos i)))))

(defun solve-z (r w f i)
  (* r (sin (+ w f)) (sin i)))

(defun solve-f (E ecc)
  (* 2 (atan (* (sqrt (- 1 ecc)) (cos (/ E 2)))
             (* (sqrt (+ 1 ecc)) (sin (/ E 2))))))

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
        (f (solve-f E ecc))
        (r (solve-r orbit f)))
    (values (solve-x r w bigw f i)
            (solve-y r w bigw f i)
            (solve-z r w f i))))

(defun get-2d-coords (orbit time)
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
        (f (solve-f E ecc))
        (r (solve-r orbit f)))
    (values (* r (cos f))
            (* r (sin f)))))

(defun write-orbit (orbit fname steps step-size)
  (with-open-file (fp fname
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create)
    (loop for i from 0 upto steps by step-size do
          (multiple-value-bind (x y z) (get-3d-coords orbit i)
            (format fp "~,3F ~,3F ~,3F~%" x y z)))))

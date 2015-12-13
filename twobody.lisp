(defun get-x (pt)
  (car pt))

(defun get-y (pt)
  (cadr pt))

(defun get-z (pt)
  (caddr pt))

(defun sqr (n)
  (* n n))

(defun distance (p1 p2)
  (let ((delta_x (- (get-x p2) (x p1)))
        (delta_y (- (get-y p2) (y p1))))
    (sqrt (+ (sqr delta_x) (sqr delta_y)))))

(defun distance3d (p1 p2)
  (let ((delta-x (- (get-x p2) (get-x p1)))
        (delta-y (- (get-y p2) (get-y p1)))
        (delta-z (- (get-z p2) (get-z p1))))
    (sqrt (+ (sqr delta-x) (sqr delta-y) (sqr delta-z)))))

(defvar G (* 6.674 (expt 10 -11)))

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
  (* G (+ m1 m2)))

(defun solve-n (mu a)
  (* (sqrt mu) (expt a (/ -3 2))))

(defun solve-M (n time bigt)
  (* n (- time bigt)))

(defun solve-V (mu r a)
  (sqrt (* mu (- (/ 2 r) (/ 1 a)))))

(defun solve-r (a ecc E)
  (* a (- 1 (* ecc (cos E)))))

(defun solve-a (body center)
  (distance3d (get-initial-pos body)
              (get-initial-pos center)))

(defun Kepler (M ecc)
  (do ((i 0 (1+ i))
       (ecc-rads (/ (* 180 ecc) pi) ecc-rads)
       (E M (+ M (* ecc-rads (sin E)))))
    ((> i 5) E)))

(defun a (body)
  (cadr body))

(defun ecc (body)
  (caddr body))

(defun bigt (body)
  (cadddr body))

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

(defun getpos (body time mu)
  (let* ((ecc (ecc body))
         (a (a body))
         (n (solve_n mu (a body)))
         (M (solve_M n time (bigt body)))
         (E (Kepler M ecc))
         (r (solve_r a ecc E))
         (V (solve_V mu r a)))
    (values r V n M E)))

(defmacro newbody (name mass initial-position)
  `(defvar ,name (body ,mass ,initial-position)))

; the semi-major axis, a, will be defined by the distance
; from the star to the initial position of the body.
(defmacro make-orbit (name body i ecc w bigw bigt)
  (let ((muvar (gensym))
        (avar  (gensym)))
    `(let ((,muvar (solve-mu (get-mass ,body)
                             (get-mass sol)))
           (,avar  (solve-a ,body sol)))
       (defvar ,name (orbit ,i ,ecc ,w ,bigw ,avar ,muvar ,bigt)))))

(defun write-orbit (orbit fname steps step-size)
  (with-open-file (fp fname
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create)
    (loop for i from 0 upto steps by step-size do
          (multiple-value-bind (x y z) (spatial-orientation orbit i)
            (format fp "~,3F ~,3F ~,3F~%" x y z)))))

; newbody args: mass initial-pos
(newbody enterprise 10 '(20 20 20))
(newbody serenity 4 '(20 10 15))
(newbody deathstar 30 '(7 5 5))
(newbody sol 10000 '(0 0 0))

; orbit args: i ecc w bigw bigt
(make-orbit enterprise-orbit enterprise 30 0.9 4 2.4 1)
(make-orbit serenity-orbit serenity 10 0.75 10 3 3)
(make-orbit deathstar-orbit deathstar 0 0.2 90 0 0)


(write-orbit enterprise-orbit "enterprise.orbit" 100 0.1)
(write-orbit serenity-orbit "serenity.orbit" 100 0.1)
(write-orbit deathstar-orbit "deathstar.orbit" 100 0.1)

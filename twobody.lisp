; for variables that require solving, as opposed to just
; grabbing them from a getter, we need access to the
; expressions that solve them. We also need access to a
; precedence hierarchy that says which terms need to be
; solved before another.
;
; Example: In order to solve r through
;
;               p
;       r = ---------,
;           1 + ecosf
;
; we need to solve f, which can be done so given the orbit
; and the time.
;
; This sounds like it could be done recursively. Like for
; each term that needs solving, store the expression,
; along with a pointer to any other terms that need
; solving before it.

; Let's say we encounter the symbol f used in a function body.
; This means we need to make a let block that binds the correct
; value to f. How do we solve for f? We call the function
; 'get-true-anomaly', passing in the orbit and the time, both
; of which are readily available. Assuming we are using a hash
; table to go from symbol to expression, we might store 'f' in
; this way
;
;       (defvar *expr-db* (make-hash-table))
;       (setf (gethash 'f *expr-db*) '(get-true-anomaly orbit time))
;
; so that a call to (gethash 'f *expr-db*) will return
; '(get-true-anomaly orbit time). This should work in situations
; like that of (spatial-orientation), where if we change our
; body from

;       (defun spatial-orientation (orbit time)
;         (let ((f (get-true-anomaly orbit time)))
;           (values-list (get-3d-coords orbit (solve-r orbit f) f))))
;
; to
;
;       (orbitfn spatial-orientation (orbit time)
;         (values-list (get-3d-coords orbit (solve-r orbit f) f))),
;
; macro-expansion will result in the following definition:
;
;       (defun spatial-orientation (orbit time)
;         (let ((f (get-true-anomaly orbit time)))
;           (values-list (get-3d-coords (solve-r orbit f) f)))).
;
; This expansion correctly binds the symbol f, which we know
; because the expansion is identical to our previous definition.
;
; What we really want (spatial-orientation) to look like, however,
; is this
;
;       (orbitfn spatial-orientation (orbit time)
;         (values-list (get-3d-coords r f))).
;
; This last step requires the correct binding of the symbol r to
; the radius of the given orbit at the given time. This value can
; be easily solved given the orbit and the true anomaly, f, so we
; might store 'r in our *expr-db* like so
;
;       (setf (gethash 'r *expr-db*) '(solve-r orbit f)).
;
; We have to be careful, though, about the order in which these
; symbols are bound in the macro-expansion. Since f can be solved
; directly from the orbit and the time, we bind that first. Then
; we have everything we need to bind r to the correct radius. So, we
; want our final macroexpansion to look like this
;
;       (defun spatial-orientation (orbit time)
;         (let* ((f (get-true-anomaly orbit time))
;                (r (solve-r orbit f)))
;               (values-list (get-3d-coords r f)))).
;
; Notice the 'let' in the previous expansions has been changed
; to 'let*.' This is because we need the value of f available
; to calculate the value of r.
;
; How are we going to ensure our variables in the let block are
; bound in the right order? Well, we can look at the definition
; of orbitfn to see that the bindings are just added to a list
; in the order in which they appear in the result of (symbols-used).
; If we can control the order of the list that gets used in the
; loop, we can control which variables get bound when.
;
; We've reduced our problem to a sorting problem: sort the results
; of (symbols-used) by a predefined precedence. All that's left
; to do is define the precedence. Since we already have a hash
; table that stores variables as keys, we can extend the
; corresponding values to store information about the variable's
; precedence. If we say that lower numbers give higher precedence,
; we might change the way we store 'f to this
;
;       (setf (gethash 'f *expr-db*) (list '(get-true-anomaly orbit time) 1)),
;
; and the way we store 'r to this
;
;       (setf (gethash 'r *expr-db*) (list '(solve-r orbit f) 2)),
;
; since we need to ensure 'r comes after, and thus is bound after, 'f.
; The built-in (sort) is able to take an arbitrary predicate
; function of two arguments that returns true iff the first argument
; is "less than" the second. In our case, "less than" means
; "has a higher precedence." So we can define a predicate function
; that will do what we want like this
;
;       (defun binding-pred (x y)
;         (< (cadr (gethash x *expr-db*))
;            (cadr (gethash y *expr-db*)))).
;
; This code brings up a tangential issue: that we should write
; getters for the values stored in the *expr-db* hash table. So
; we will, and they are as follows
;
;       (defun get-precedence (x)
;         (cadr (gethash x *expr-db*)))
;
;       (defun get-expr (x)
;         (car (gethash x *expr-db*))).
;
; Now we can write our predicate function like this
;
;       (defun binding-pred (x y)
;         (< (get-precedence x)
;            (get-precedence y))),
;
; which in my opinion is nicer.
;
; This method of going from symbol to expression can be generalized
; to variables that don't need solving. We can store the 'ecc in
; our *expr-db* like this
;
;       (setf (gethash 'ecc *expr-db*) '(get-ecc orbit))
;
; and it will expand to the same binding as it was before. This also
; removes the need for that bad interning hack I was using.
;
; Our new orbitfn macro can be written like this
;
;       (defmacro orbitfn (name args body)
;         (let ((symbs (sort #'binding-pred
;                            (symbols-used *orbitsymbs* body))))
;           `(defun ,name ,args
;              (let (,@(loop for s in symbs
;                            collect (list s (get-expr s))))
;                ,body)))).
;

; recurse through a tree and return the list of symbols
; in the tree that are in the 'symbs' list.
(defun symbols-used (symbs tree)
  (remove-duplicates (cond ((null tree) nil)
                           ((atom (car tree)) (if (member (car tree) symbs)
                                                (append (list (car tree)) (symbols-used symbs (cdr tree)))
                                                (symbols-used symbs (cdr tree) )))
                           (t (append (symbols-used symbs (car tree))
                                      (symbols-used symbs (cdr tree)))))))

(defun flatten (list)
  (cond ((null list) nil)
        ((atom list) (list list))
        (t (append (flatten (car list))
                   (flatten (cdr list))))))

(defun add-dependencies (symbs)
  (union symbs
         (flatten (loop for s in symbs
                        collect (add-dependencies (get-depends s))))))

; common variables used as special terms in orbital equations.
(defvar *orbitsymbs* (list 'E 'M 'n 'f 'r 'i 'ecc 'w 'bigw 'bigt 'a 'mu))

(defvar *expr-db* (make-hash-table))

(defun get-precedence (x)
  (cadr (gethash x *expr-db*)))

(defun get-expr (x)
  (car (gethash x *expr-db*)))

(defun get-depends (x)
  (caddr (gethash x *expr-db*)))

(defun binding-pred (x y)
  (< (get-precedence x)
     (get-precedence y)))

(defmacro orbitsymbol (symb &key expr precedence (depends nil))
  `(setf (gethash ,symb *expr-db*) (list ,expr ,precedence ,depends)))

; simple getters
(orbitsymbol 'i    :expr '(get-i orbit)    :precedence 1)
(orbitsymbol 'ecc  :expr '(get-ecc orbit)  :precedence 1)
(orbitsymbol 'w    :expr '(get-w orbit)    :precedence 1)
(orbitsymbol 'bigw :expr '(get-bigw orbit) :precedence 1)
(orbitsymbol 'a    :expr '(get-a orbit)    :precedence 1)
(orbitsymbol 'mu   :expr '(get-mu orbit)   :precedence 1)
(orbitsymbol 'bigt :expr '(get-bigt orbit) :precedence 1)

; hierarchical expressions
;(orbitsymbol 'f :expr '(get-true-anomaly orbit time) :precedence 1)
(orbitsymbol 'f :expr '(solve-f E ecc) :precedence 5 :depends '(E ecc))
(orbitsymbol 'r :expr '(solve-r orbit f) :precedence 6)
(orbitsymbol 'n :expr '(solve-n mu a) :precedence 2 :depends '(mu a))
(orbitsymbol 'M :expr '(solve-M n time bigt) :precedence 3 :depends '(n bigt))
(orbitsymbol 'E :expr '(solve-E M ecc) :precedence 4 :depends '(M ecc))

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

(defun necessary-symbs (args body)
  (sort (add-dependencies (set-difference (symbols-used *orbitsymbs* body)
                                          args))
        #'binding-pred))

; enclose the given function body in a let block that binds any
; orbital-mechanics variables to their values in the given orbit.
; NOTE: functions defined as an 'orbitfn' MUST be passed an
; orbit called 'orbit' as an argument.
(defmacro orbitfn (name args body)
  `(defun ,name ,args
     (let* (,@(loop for s in (necessary-symbs args body)
                    collect (list s (get-expr s))))
       ,body)))

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

(defun solve-r (a ecc E)
  (* a (- 1 (* ecc (cos E)))))

(defun solve-a (body center)
  (distance3d (get-initial-pos body)
              (get-initial-pos center)))

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

(orbitfn solve-r (orbit f)
  (let ((p (* a (- (sqr ecc) 1))))
    (/ p (+ 1 (* ecc (cos f))))))

(orbitfn get-3d-coords (orbit time)
  (let ((x (solve-x r w bigw f i))
        (y (solve-y r w bigw f i))
        (z (solve-z r w f i)))
    (list x y z)))

(defun write-orbit (orbit fname steps step-size)
  (with-open-file (fp fname
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create)
    (loop for i from 0 upto steps by step-size do
          (multiple-value-bind (x y z) (get-3d-coords orbit i)
            (format fp "~,3F ~,3F ~,3F~%" x y z)))))

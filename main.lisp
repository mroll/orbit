(load "twobody.lisp")


; newbody args: mass initial-pos
(newbody enterprise 10 '(20 20 20))
(newbody serenity 5000 '(20 10 15))
(newbody deathstar 30 '(-7 5 -5))
(newbody sol 10000 '(0 0 0))

; orbit args: i ecc w bigw bigt
(make-orbit enterprise-orbit enterprise 30 0.9 4 2.4 1)
(make-orbit serenity-orbit serenity 10 0.75 10 3 3)
(make-orbit deathstar-orbit deathstar 0 0.2 90 5 0)


(write-orbit enterprise-orbit "enterprise.orbit" 1000 0.1)
(write-orbit serenity-orbit "serenity.orbit" 1000 0.1)
(write-orbit deathstar-orbit "deathstar.orbit" 1000 0.1)

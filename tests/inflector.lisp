(defpackage :vana-inflector-test
  (:use :cl
        :vana-inflector
        :lisp-unit))

(in-package :vana-inflector-test)

(define-test test-plural-of-regular
  (assert-equal "quizzes" (plural-of "quiz"))
  (assert-equal "oxen" (plural-of "ox"))
  (assert-equal "matrices" (plural-of "matrix"))
  (assert-equal "vertices" (plural-of "vertex"))
  (assert-equal "indices" (plural-of "index"))
  (assert-equal "hives" (plural-of "hive"))
  (assert-equal "tomatoes" (plural-of "tomato"))
  (assert-equal "crises" (plural-of "crisis")))

(define-test test-plural-of-irregular
  (assert-equal "people" (plural-of "person"))
  (assert-equal "men" (plural-of "man"))
  (assert-equal "sexes" (plural-of "sex"))
  (assert-equal "kine" (plural-of "cow")))

(define-test test-add-irregular
  (assert-equal "womans" (plural-of "woman"))
  (irregular "woman" "women")
  (assert-equal "women" (plural-of "woman")))

(define-test test-uncountable
  (assert-equal "fish" (plural-of "fish"))
  (assert-equal "fish" (singular-of "fish"))
  (assert-equal "sheep" (plural-of "sheep"))
  (assert-equal "sheep" (singular-of "sheep")))

(define-test test-add-uncountable
  (assert-equal "cackles" (plural-of "cackle"))
  (uncountable "cackle")
  (assert-equal "cackle" (plural-of "cackle")))

(define-test test-singular-of
  (assert-equal "cup" (singular-of "cups"))
  (assert-equal "ox" (singular-of "oxen"))
  (assert-equal "life" (singular-of "lives")))

;;(run-tests)

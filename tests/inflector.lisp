(defpackage :vana-inflector-test
  (:use :cl
        :vana-inflector
        :lisp-unit))

(in-package :vana-inflector-test)

(define-test test-pluralize-regular
  (assert-equal "quizzes" (pluralize "quiz"))
  (assert-equal "oxen" (pluralize "ox"))
  (assert-equal "matrices" (pluralize "matrix"))
  (assert-equal "vertices" (pluralize "vertex"))
  (assert-equal "indices" (pluralize "index"))
  (assert-equal "hives" (pluralize "hive"))
  (assert-equal "tomatoes" (pluralize "tomato"))
  (assert-equal "crises" (pluralize "crisis")))

(define-test test-pluralize-irregular
  (assert-equal "people" (pluralize "person"))
  (assert-equal "men" (pluralize "man"))
  (assert-equal "sexes" (pluralize "sex"))
  (assert-equal "kine" (pluralize "cow")))

(define-test test-add-irregular
  (assert-equal "womans" (pluralize "woman"))
  (irregular "woman" "women")
  (assert-equal "women" (pluralize "woman")))

(define-test test-uncountable
  (assert-equal "fish" (pluralize "fish"))
  (assert-equal "fish" (singularize "fish"))
  (assert-equal "sheep" (pluralize "sheep"))
  (assert-equal "sheep" (singularize "sheep")))

(define-test test-add-uncountable
  (assert-equal "cackles" (pluralize "cackle"))
  (uncountable "cackle")
  (assert-equal "cackle" (pluralize "cackle")))

(define-test test-singularize
  (assert-equal "cup" (singularize "cups"))
  (assert-equal "ox" (singularize "oxen"))
  (assert-equal "life" (singularize "lives")))

;;(run-tests)

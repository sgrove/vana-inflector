(defpackage :vana-inflector
  (:use :cl
        :cl-ppcre
        :vana-utils)
  (:export :pluralize
           :plural-of
           :singularize
           :singular-of
           :irregular?
           :irregular
           :uncountable?
           :uncountable))

(in-package :vana-inflector)

;; Adapted *cough*ripped*cough* from rails inflector.rb
;;; singular->plurals regular expressions
(setf **plurals**
      (reverse (list (list "$" "s")
        (list "s$" "s")
        (list "(ax|test)is$" "\\1es")
        (list "(octop|vir)us$" "\\1i")
        (list "(alias|status)$" "\\1es")
        (list "(bu)s$" "\\1ses")
        (list "(buffal|tomat)o$" "\\1oes")
        (list "([ti])um$" "\\1a")
        (list "sis$" "ses")
        (list "(?:([^f])fe|([lr])f)$" "\\1\\2ves")
        (list "(hive)$" "\\1s")
        (list "([^aeiouy]|qu)y$" "\\1ies")
        (list "(x|ch|ss|sh)$" "\\1es")
        (list "(matr|vert|ind)(?:ix|ex)$" "\\1ices")
        (list "([m|l])ouse$" "\\1ice")
        (list "^(ox)$" "\\1en")
        (list "(quiz)$" "\\1zes"))))

;;; plurals->singular regular expressions
(defvar **singulars**
  (reverse (list
    (list "s$" "")
    (list "(n)ews$" "\\1ews")
    (list "([ti])a$" "\\1um")
    (list "((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$" "\\1\\2sis")
    (list "(^analy)ses$" "\\1sis")
    (list "([^f])ves$" "\\1fe")
    (list "(hive)s$" "\\1")
    (list "(tive)s$" "\\1")
    (list "([lr])ves$" "\\1f")
    (list "([^aeiouy]|qu)ies$" "\\1y")
    (list "(s)eries$" "\\1eries")
    (list "(m)ovies$" "\\1ovie")
    (list "(x|ch|ss|sh)es$" "\\1")
    (list "([m|l])ice$" "\\1ouse")
    (list "(bus)es$" "\\1")
    (list "(o)es$" "\\1")
    (list "(shoe)s$" "\\1")
    (list "(cris|ax|test)es$" "\\1is")
    (list "(octop|vir)i$" "\\1us")
    (list "(alias|status)es$" "\\1")
    (list "^(ox)en" "\\1")
    (list "(vert|ind)ices$" "\\1ex")
    (list "(matr)ices$" "\\1ix")
    (list "(quiz)zes$" "\\1")
    (list "(database)s$" "\\1"))))

(defvar **uncountables**
  (list "equipment" "information" "rice" "money" "species" "series" "fish" "sheep" "jeans"))

(defvar **irregulars**
  (args->alist
   "person" "people"
   "man"    "men"
   "child"  "children"
   "sex"    "sexes"
   "move"   "moves"
   "cow"    "kine"))

;; Interface for adding new **uncountables**, querying, etc.
(defun uncountable (word)
  "Notifies the inflector that a word is uncountable"
  (setf **uncountables** (cons word **uncountables**))) 

(defun uncountable? (word)
  (member word **uncountables** :test #'string-equal))

(defun irregular (singular plural)
  "Adds a irregular single-plural set to the irregular list"
  (setf **irregulars** (acons singular plural **irregulars**)))

(defun irregular? (word)
  (or (-> word **irregulars**)
      (rassoc word **irregulars** :test #'string-equal)))

;; For a touch of added robustness
(defun irregular-plural? (word)
  (rassoc word **irregulars** :test #'string-equal))

(defun irregular-singular? (word)
  (-> word **irregulars**))

;; These two could be combined nicely, I'm sure
(defun get-irregular-singular (plural)
  (if (irregular-singular? plural)
      plural
      (car (rassoc plural **irregulars** :test #'string-equal))))

(defun get-irregular-plural (singular)
  (if (irregular-plural? singular)
      singular
      (-> singular **irregulars**)))

(defun plural-of (word)
  "Returns the plural of a word if it's singular, or itself if already plural"
  (cond ((uncountable? word) word)
        ((irregular?   word) (get-irregular-plural word))
        (t (inflector-helper word **plurals**))))

(defun singular-of (word)
  "Returns the singular of a word if it's singular, or itself if already singular"
  (cond ((uncountable? word) word)
        ((irregular?   word) (get-irregular-singular word))
        (t (inflector-helper word **singulars**))))

(defun inflector-helper (word regexes)
  (if (null regexes)
      word
      (multiple-value-bind (string match-found?)
          (cl-ppcre:regex-replace (first (first regexes)) word (second (first regexes)))
        (if match-found?
            string
            (inflector-helper word (rest regexes))))))

(defun pluralize (count word &optional plural)
  (if (not (= count 1))
      (or plural
          (plural-of word))
      word))

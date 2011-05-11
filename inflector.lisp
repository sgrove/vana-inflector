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
(defparameter *plurals*
  '(("(.*[aeiou])z$"             "\\1zzes")
    ("^(ox)$"                    "\\1en")
    ("([m|l])ouse$"              "\\1ice")
    ("(matr|vert|ind)(?:ix|ex)$" "\\1ices")
    ("(z|x|ch|ss|sh)$"             "\\1es")
    ("([^aeiouy]|qu)y$"          "\\1ies")
    ("(hive)$"                   "\\1s")
    ("(?:([^f])fe|([lr])f)$"     "\\1\\2ves")
    ("sis$"                      "ses")
    ("([ti])um$"                 "\\1a")
    ("(buffal|tomat)o$"          "\\1oes")
    ("(bu)s$"                    "\\1ses")
    ("(alias|status)$"           "\\1es")
    ("(octop)us$"                "\\1uses")
    ("(vir)us$"                  "\\1i")
    ("(ax|test)is$"              "\\1es")
    ("s$"                        "s")
    ("$"                         "s")))

;;; plurals->singular regular expressions
(defparameter *singulars*
  '(("(database)s$"        "\\1")
    ("(.*[aeiou]z)zes$"    "\\1")
    ("(matr)ices$"         "\\1ix")
    ("(vert|ind)ices$"     "\\1ex")
    ("^(ox)en"             "\\1")
    ("(alias|status)es$"   "\\1")
    ("(octop)(odes|uses)$" "\\1us")
    ("(octop|vir)i$"       "\\1us")
    ("(cris|ax|test)es$"   "\\1is")
    ("(shoe)s$"            "\\1")
    ("(o)es$"              "\\1")
    ("(bus)es$"            "\\1")
    ("([m|l])ice$"         "\\1ouse")
    ("(z|x|ch|ss|sh)es$"     "\\1")
    ("(m)ovies$"           "\\1ovie")
    ("(s)eries$"           "\\1eries")
    ("([^aeiouy]|qu)ies$"  "\\1y")
    ("([lr])ves$"          "\\1f")
    ("(tive)s$"            "\\1")
    ("(hive)s$"            "\\1")
    ("([^f])ves$"          "\\1fe")
    ("(^analy)ses$"        "\\1sis")
    ("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$" "\\1\\2sis")
    ("([ti])a$"           "\\1um")
    ("(n)ews$"            "\\1ews")
    ("s$" "")))

(defparameter *uncountables*
  (list "equipment" "information" "rice" "money" "species" "series" "fish"
	"sheep" "jeans" "news" ))

(defparameter *irregulars*
  (args->alist
   "is"     "are"
   "person" "people"
   "man"    "men"
   "woman"  "women"
   "child"  "children"
   "move"   "moves"
   "movie"  "movies"
   "buzz"   "buzzes"
   ))

;; Interface for adding new *uncountables*, querying, etc.
(defun uncountable (word)
  "Notifies the inflector that a word is uncountable"
  (pushnew word *uncountables* :test #'string-equal))

(defun uncountable? (word)
  (member word *uncountables* :test #'string-equal))

(defun irregular (singular plural)
  "Adds a irregular single-plural set to the irregular list"
  (push (cons singular plural) *irregulars*))

;; For a touch of added robustness
(defun irregular-plural? (word)
  (rassoc word *irregulars* :test #'string-equal))

(defun irregular-singular? (word)
  (-> word *irregulars*))

(defun irregular? (word)
  (or (irregular-singular? word)
      (irregular-plural? word)))

;; These two could be combined nicely, I'm sure
(defun get-irregular-singular (plural)
  (if (irregular-singular? plural)
      plural
      (car (rassoc plural *irregulars* :test #'string-equal))))

(defun get-irregular-plural (singular)
  (if (irregular-plural? singular)
      singular
      (-> singular *irregulars*)))

(defun plural (rule replacement)
  "Adds a plural rule, where RULE can be either a string or a regex, and REPLACEMENT can contain capture references defined in RULE"
  (push (list rule replacement) *plurals*))


(defun plural-of (word)
  "Returns the plural of a word if it's singular, or itself if already plural"
  (cond ((uncountable? word) word)
        ((irregular?   word) (get-irregular-plural word))
        (t (inflector-helper word *plurals*))))

(defun singular (rule replacement)
  "Adds a singular rule, where RULE can be either a string or a regex, and REPLACEMENT can contain capture references defined in RULE"
  (push (list rule replacement) *singulars*))

(defun singular-of (word)
  "Returns the singular of a word if it's singular, or itself if already singular"
  (cond ((uncountable? word) word)
        ((irregular?   word) (get-irregular-singular word))
        (t (inflector-helper word *singulars*))))

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

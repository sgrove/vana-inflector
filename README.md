Vana Inflector
========

A common lisp library to easily pluralize and singularize English words.

This is a support package for the [Vana web framework][1], and is a port of the same from Rails' ActiveSupport module.

Installation
------------------

(ql:quickload 'vana-inflector)

Usage
-----------
Example Usage:

    (use-package :vana-inflector)
    > (let ((dollars 1.7)
            (users 34)
            (purchases 1))
           (format nil "The site has ~D ~A, with a total of ~D ~A and $~D ~A"  
                   users (pluralize users "user") 
                   purchases (pluralize purchases "purchase") 
                   dollars (pluralize dollars "dollar")))
    "The site has 34 users, with a total of 1 purchase and $1.7 dollars"

Basic Usage, `plural-of` and `singular-of`:

    > (plural-of "octopus") 
    "octopi"
    > (plural-of "datum")
    "data"
    > (singular-of "children")
    "child"
    > (singular-of "cats")
    "cat"
    > (singular-of "data")
    "datum"

Basic Usage, `pluralize`:

    > (pluralize 2 "octopus")
    "octopi"
    > (pluralize 1 "octopus")
    "octopus"

You can pass in the default plural to be used. If not, the inflector is used to determine the plural.

    > (pluralize 2 "tooth" "teeth")
    "teeth"
    > (pluralize 2 "tooth")
    "tooths"

Use `irregular` to add an irregular:

    > (singular-of "feet")
    "feet"
    > (irregular "foot" "feet")
    > (singular-of "feet")
    "foot"
    > (plural-of "foot")
    "feet"

Use `uncountable` to add an uncountable:

    > (plural-of "advice")
    "advices"
    > (singular-of "advice")
    "advice"
    > (singular-of "advices")
    "advice"
    > (uncountable "advice")
    > (plural-of "advice")
    "advice"

TODO
-------
 * Expand the default lookup lists? Possible (but probably negligible) performance tradeoff for completeness
 * Add methods to transfer between CamelCase, whatJavaScriptDoes, this_underscore_style, and-lisp-style

License
---------------

Released under the MIT license, please see `LICENSE` for more details

Thanks
-------------

  - [Xach][2] - For [quicklisp][3], really made getting back into CL much easier.
  - Siebel - For [PCL][4], which has been a great reference.


  [1]: https://github.com/sgrove/vana
  [2]: http://xach.livejournal.com/
  [3]: http://www.quicklisp.org/
  [4]: http://gigamonkeys.com/book/

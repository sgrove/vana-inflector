Vana Inflector
========

A common lisp library to easily pluralize and singularize English words.

This is a support package for the [Vana web framework][1], and is a port of the same from Rails' ActiveSupport module.

Installation
------------------

(ql:quickload 'vana-inflector)

Usage
-----------
Basic Usage, `plural-of` and `singular-of`:

    (use-package :vana-inflector)
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

    (use-package :vana-inflector)
    > (pluralize 2 "octopus")
    "octopi"
    > (pluralize 1 "octopus")
    "octopus"

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

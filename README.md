Vana Inflector
========

A common lisp library to easily pluralize and singularize English words.

This is a support package for the [Vana web framework][1], and is a port of the same from Rails' ActiveSupport module.


Installation
------------------

(ql:quickload 'vana-inflector)

Usage
-----------
Basic Usage:

    (use-package :vana-inflector)
    > (pluralize "octopus") 
    "octopi"
    > (pluralize "datum")
    "data"
    > (singularize "children")
    "child"
    > (singularize "cats")
    "cat"
    > (singularize "data")
    "datum"

Use `irregular` to add an irregular:

    > (singularize "feet")
    "feet"
    > (irregular "foot" "feet")
    > (singularize "feet")
    "foot"
    > (pluralize "foot")
    "feet"

Use `uncountable` to add an uncountable:

    > (pluralize "advice")
    "advices"
    > (singularize "advice")
    "advice"
    > (singularize "advices")
    "advice"
    > (uncountable "advice")
    > (pluralize "advice")
    "advice"

TODO
-------
 * Expand the default lookup lists? Possible (but probably negligible) performance tradeoff for completeness
 * Add methods to transfer between CamelCase, whatJavaScriptDoes, and_this_underscore_style

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

.. _multi-way-if:

Multi-way if-expressions
------------------------

.. extension:: MultiWayIf
    :shortdesc: Enable multi-way if-expressions.

    :since: 7.6.1

    Allow the use of multi-way-``if`` syntax.

With :extension:`MultiWayIf` extension GHC accepts conditional expressions with
multiple branches: ::

      if | guard1 -> expr1
         | ...
         | guardN -> exprN

which is roughly equivalent to ::

      case () of
        _ | guard1 -> expr1
        ...
        _ | guardN -> exprN

Multi-way if expressions introduce a new layout context. So the example
above is equivalent to: ::

      if { | guard1 -> expr1
         ; | ...
         ; | guardN -> exprN
         }

The following behaves as expected: ::

      if | guard1 -> if | guard2 -> expr2
                        | guard3 -> expr3
         | guard4 -> expr4

because layout translates it as ::

      if { | guard1 -> if { | guard2 -> expr2
                          ; | guard3 -> expr3
                          }
         ; | guard4 -> expr4
         }

Layout with multi-way if works in the same way as other layout contexts,
except that the semi-colons between guards in a multi-way if are
optional. So it is not necessary to line up all the guards at the same
column; this is consistent with the way guards work in function
definitions and case expressions.

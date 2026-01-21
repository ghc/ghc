.. _multi-way-if:

Multi-way if-expressions
------------------------

.. extension:: MultiWayIf
    :shortdesc: Allow multi-way ``if``-expressions.

    :since: 7.6.1

    Allow the use of multi-way-``if`` syntax.

With the :extension:`MultiWayIf` extension GHC accepts conditional expressions with
multiple branches: ::

      if | guard1 -> expr1
         | ...
         | guardN -> exprN

which is roughly equivalent to ::

      case () of
        _ | guard1 -> expr1
        ...
        _ | guardN -> exprN

Multi-way if expressions introduce a new kind of layout context that does not generate semicolons. The example
above is equivalent to: ::

      if { | guard1 -> expr1
           | ...
           | guardN -> exprN
         }

The following behaves as expected: ::

      if | guard1 -> if | guard2 -> expr2
                        | guard3 -> expr3
         | guard4 -> expr4

because layout translates it as ::

      if { | guard1 -> if { | guard2 -> expr2
                            | guard3 -> expr3
                          }
           | guard4 -> expr4
         }

Layout with multi-way if works in the same way as other layout contexts,
except that desugaring does not insert semicolons.
So it is not necessary to line up all the guards at the same
column; this is consistent with the way guards work in function
definitions and case expressions.

Note that multi-way if supports guards other than boolean conditions: ::

      if | parseNumbers settings
         , Just (exponent, mantissa) <- decomposeNumber str
         , let (integralPart, fractionPart) = parse mantissa
         , integralPart >= 0 -> ...
         | otherwise -> ...

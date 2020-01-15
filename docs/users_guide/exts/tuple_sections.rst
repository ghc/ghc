.. _tuple-sections:

Tuple sections
--------------

.. extension:: TupleSections
    :shortdesc: Enable tuple sections.

    :since: 6.12

    Allow the use of tuple section syntax

The :extension:`TupleSections` extension enables partially applied
tuple constructors. For example, the following program ::

      (, True)

is considered to be an alternative notation for the more unwieldy
alternative ::

      \x -> (x, True)

You can omit any combination of arguments to the tuple, as in the
following ::

      (, "I", , , "Love", , 1337)

which translates to ::

      \a b c d -> (a, "I", b, c, "Love", d, 1337)

If you have `unboxed tuples <#unboxed-tuples>`__ enabled, tuple sections
will also be available for them, like so ::

      (# , True #)

Because there is no unboxed unit tuple, the following expression ::

      (# #)

continues to stand for the unboxed singleton tuple data constructor.



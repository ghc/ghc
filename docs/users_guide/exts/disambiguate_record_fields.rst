.. _disambiguate-fields:

Record field disambiguation
---------------------------

.. extension:: DisambiguateRecordFields
    :shortdesc: Enable record field disambiguation.
        Implied by :extension:`RecordWildCards`.

    :since: 6.8.1

    Allow the compiler to automatically choose between identically-named
    record selectors based on type (if the choice is unambiguous).

In record construction and record pattern matching it is entirely
unambiguous which field is referred to, even if there are two different
data types in scope with a common field name. For example:

::

    module M where
      data S = MkS { x :: Int, y :: Bool }

    module Foo where
      import M

      data T = MkT { x :: Int }

      ok1 (MkS { x = n }) = n+1   -- Unambiguous
      ok2 n = MkT { x = n+1 }     -- Unambiguous

      bad1 k = k { x = 3 }        -- Ambiguous
      bad2 k = x k                -- Ambiguous

Even though there are two ``x``'s in scope, it is clear that the ``x``
in the pattern in the definition of ``ok1`` can only mean the field
``x`` from type ``S``. Similarly for the function ``ok2``. However, in
the record update in ``bad1`` and the record selection in ``bad2`` it is
not clear which of the two types is intended.

Haskell 98 regards all four as ambiguous, but with the
:extension:`DisambiguateRecordFields` extension, GHC will accept the former two. The
rules are precisely the same as those for instance declarations in
Haskell 98, where the method names on the left-hand side of the method
bindings in an instance declaration refer unambiguously to the method of
that class (provided they are in scope at all), even if there are other
variables in scope with the same name. This reduces the clutter of
qualified names when you import two records from different modules that
use the same field name.

Some details:

-  Field disambiguation can be combined with punning (see
   :ref:`record-puns`). For example: ::

       module Foo where
         import M
         x=True
         ok3 (MkS { x }) = x+1   -- Uses both disambiguation and punning

-  With :extension:`DisambiguateRecordFields` you can use *unqualified* field
   names even if the corresponding selector is only in scope *qualified*
   For example, assuming the same module ``M`` as in our earlier
   example, this is legal: ::

       module Foo where
         import qualified M    -- Note qualified

         ok4 (M.MkS { x = n }) = n+1   -- Unambiguous

   Since the constructor ``MkS`` is only in scope qualified, you must
   name it ``M.MkS``, but the field ``x`` does not need to be qualified
   even though ``M.x`` is in scope but ``x`` is not (In effect, it is
   qualified by the constructor).



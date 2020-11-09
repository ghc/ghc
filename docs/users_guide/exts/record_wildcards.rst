.. _record-wildcards:

Record wildcards
----------------

.. extension:: RecordWildCards
    :shortdesc: Enable record wildcards.
        Implies :extension:`DisambiguateRecordFields`.

    :implies: :extension:`DisambiguateRecordFields`.
    :since: 6.8.1

    Allow the use of wildcards in record construction and pattern matching.

Record wildcards are enabled by the language extension :extension:`RecordWildCards`. This
extension implies :extension:`DisambiguateRecordFields`.

For records with many fields, it can be tiresome to write out each field
individually in a record pattern, as in ::

    data C = C {a :: Int, b :: Int, c :: Int, d :: Int}
    f (C {a = 1, b = b, c = c, d = d}) = b + c + d

Record wildcard syntax permits a "``..``" in a record pattern, where
each elided field ``f`` is replaced by the pattern ``f = f``. For
example, the above pattern can be written as ::

    f (C {a = 1, ..}) = b + c + d

More details:

-  Record wildcards in patterns can be mixed with other patterns,
   including puns (:ref:`record-puns`); for example, in a pattern
   ``(C {a = 1, b, ..})``. Additionally, record wildcards can be used
   wherever record patterns occur, including in ``let`` bindings and at
   the top-level. For example, the top-level binding ::

       C {a = 1, ..} = e

   defines ``b``, ``c``, and ``d``.

-  Record wildcards can also be used in an expression, when constructing
   a record. For example, ::

       let {a = 1; b = 2; c = 3; d = 4} in C {..}

   in place of ::

       let {a = 1; b = 2; c = 3; d = 4} in C {a=a, b=b, c=c, d=d}

   The expansion is purely syntactic, so the record wildcard expression
   refers to the nearest enclosing variables that are spelled the same
   as the omitted field names.

-  For both pattern and expression wildcards, the "``..``" expands to
   the missing *in-scope* record fields. Specifically the expansion of
   "``C {..}``" includes ``f`` if and only if:

   -  ``f`` is a record field of constructor ``C``.

   -  The record field ``f`` is in scope somehow (either qualified or
      unqualified).

   These rules restrict record wildcards to the situations in which the
   user could have written the expanded version. For example ::

       module M where
         data R = R { a,b,c :: Int }
       module X where
         import M( R(R,a,c) )
         f a b = R { .. }

   The ``R{..}`` expands to ``R{a=a}``, omitting ``b`` since the
   record field is not in scope, and omitting ``c`` since the variable
   ``c`` is not in scope (apart from the binding of the record selector
   ``c``, of course).

-  When record wildcards are use in record construction, a field ``f``
   is initialised only if ``f`` is in scope,
   and is not imported or bound at top level.
   For example, ``f`` can be bound by an enclosing pattern match or
   let/where-binding. For example ::

        module M where
          import A( a )

          data R = R { a,b,c,d :: Int }

          c = 3 :: Int

          f b = R { .. }  -- Expands to R { b = b, d = d }
            where
              d = b+1

   Here, ``a`` is imported, and ``c`` is bound at top level, so neither
   contribute to the expansion of the "``..``".
   The motivation here is that it should be
   easy for the reader to figure out what the "``..``" expands to.

-  Record wildcards cannot be used (a) in a record update construct, and
   (b) for data constructors that are not declared with record fields.
   For example: ::

       f x = x { v=True, .. }   -- Illegal (a)

       data T = MkT Int Bool
       g = MkT { .. }           -- Illegal (b)
       h (MkT { .. }) = True    -- Illegal (b)




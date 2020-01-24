.. _record-puns:

Record puns
-----------

.. extension:: NamedFieldPuns
    :shortdesc: Enable record puns.

    :since: 6.10.1

    Allow use of record puns.

Record puns are enabled by the language extension :extension:`NamedFieldPuns`.

When using records, it is common to write a pattern that binds a
variable with the same name as a record field, such as: ::

    data C = C {a :: Int}
    f (C {a = a}) = a

Record punning permits the variable name to be elided, so one can simply
write ::

    f (C {a}) = a

to mean the same pattern as above. That is, in a record pattern, the
pattern ``a`` expands into the pattern ``a = a`` for the same name
``a``.

Note that:

-  Record punning can also be used in an expression, writing, for
   example, ::

       let a = 1 in C {a}

   instead of ::

       let a = 1 in C {a = a}

   The expansion is purely syntactic, so the expanded right-hand side
   expression refers to the nearest enclosing variable that is spelled
   the same as the field name.

-  Puns and other patterns can be mixed in the same record: ::

       data C = C {a :: Int, b :: Int}
       f (C {a, b = 4}) = a

-  Puns can be used wherever record patterns occur (e.g. in ``let``
   bindings or at the top-level).

-  A pun on a qualified field name is expanded by stripping off the
   module qualifier. For example: ::

       f (C {M.a}) = a

   means ::

       f (M.C {M.a = a}) = a

   (This is useful if the field selector ``a`` for constructor ``M.C``
   is only in scope in qualified form.)



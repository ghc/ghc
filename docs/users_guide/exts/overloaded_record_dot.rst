.. _overloaded-record-dot:

Overloaded record dot
---------------------

.. extension:: OverloadedRecordDot
    :shortdesc: Record '.' syntax

    :since: 9.2.0

    Provides record '.' syntax e.g. ``x.foo``

When ``OverloadedRecordDot`` is enabled one can write ``a.b`` to mean the ``b`` field of the ``a`` record expression.

Example:

.. code-block:: haskell

  {-# LANGUAGE OverloadedRecordDot #-}
  {-# LANGUAGE DuplicateRecordFields #-}

  data Person = Person { name :: String }
  data Company = Company { name :: String, owner :: Person }

  main = do
    let c = Company { name = "Acme Corp."
                    , owner = Person { name = "Wile E. Coyote" } }
    print $ c.name ++ " is run by " ++ c.owner.name

You may also write ``(.b)`` to mean a function that "projects the ``b`` field from its argument". For example, ``(.b) a`` means the same thing as ``a.b``).

``OverloadedRecordDot`` is normally implemented by desugaring record ``.`` expressions to ``GHC.Records.getField`` expressions. By enabling ``OverloadedRecordDot`` and ``RebindableSyntax`` together it is possible to desugar ``.`` expressions into your own ``getField`` implementations.

When considering ``a.b``, the ``b`` field that is meant is determined by solving ``HasField`` constraints. See :ref:`solving-hasfield-constraints`.

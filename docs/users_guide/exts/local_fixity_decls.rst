.. _local-fixity-declarations:

Local Fixity Declarations
-------------------------

A careful reading of the Haskell 98 Report reveals that fixity
declarations (``infix``, ``infixl``, and ``infixr``) are permitted to
appear inside local bindings such those introduced by ``let`` and
``where``. However, the Haskell Report does not specify the semantics of
such bindings very precisely.

In GHC, a fixity declaration may accompany a local binding: ::

    let f = ...
        infixr 3 `f`
    in
        ...

and the fixity declaration applies wherever the binding is in scope. For
example, in a ``let``, it applies in the right-hand sides of other
``let``-bindings and the body of the ``let``\ C. Or, in recursive ``do``
expressions (:ref:`recursive-do-notation`), the local fixity
declarations of a ``let`` statement scope over other statements in the
group, just as the bound name does.

Moreover, a local fixity declaration *must* accompany a local binding
of that name: it is not possible to revise the fixity of name bound
elsewhere, as in ::

    let infixr 9 $ in ...

Because local fixity declarations are technically Haskell 98, no extension is
necessary to enable them.

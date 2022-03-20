.. _lambda-case:

Lambda-case
-----------

.. extension:: LambdaCase
    :shortdesc: Enable lambda-case expressions.

    :since: 7.6.1

    Allow the use of lambda-case syntax.

The :extension:`LambdaCase` extension enables expressions of the form ::

      \case { p1 -> e1; ...; pN -> eN }

which is equivalent to ::

      \freshName -> case freshName of { p1 -> e1; ...; pN -> eN }

Since GHC 9.4.1, it also allow expressions with multiple scrutinees (see GHC
proposal `#302 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0302-cases.rst>`_)
of the form ::

      \cases { p11 ... pM1 -> e1; ...; p1N ... pMN -> eN }

which is equivalent to a function defined as

      f p11 ... pM1 -> e1
      ...
      f p1N ... pMN -> eN


Note that both ``\case`` and ``\cases`` start a layout, so you can write ::

      \case
        p1 -> e1
        ...
        pN -> eN

Additionally, since GHC 9.0.1, combining :extension:`LambdaCase` with
:extension:`Arrows` allows ``\case`` (and since GHC 9.4.1 ``\cases``)
syntax to be used as a command in ``proc`` notation: ::

      proc x -> (f -< x) `catchA` \case
        p1 -> cmd1
        ...
        pN -> cmdN

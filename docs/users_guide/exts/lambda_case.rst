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

Note that ``\case`` starts a layout, so you can write ::

      \case
        p1 -> e1
        ...
        pN -> eN

Additionally, since GHC 9.0.1, combining :extension:`LambdaCase` with
:extension:`Arrows` allows ``\case`` syntax to be used as a command in
``proc`` notation: ::

      proc x -> (f -< x) `catchA` \case
        p1 -> cmd1
        ...
        pN -> cmdN

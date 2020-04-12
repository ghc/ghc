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



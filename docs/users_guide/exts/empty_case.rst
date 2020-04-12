.. _empty-case:

Empty case alternatives
-----------------------

.. extension:: EmptyCase
    :shortdesc: Allow empty case alternatives.

    :since: 7.8.1

    Allow empty case expressions.

The :extension:`EmptyCase` extension enables case expressions, or lambda-case
expressions, that have no alternatives, thus: ::

    case e of { }   -- No alternatives

or ::

    \case { }       -- -XLambdaCase is also required

This can be useful when you know that the expression being scrutinised
has no non-bottom values. For example:

::

      data Void
      f :: Void -> Int
      f x = case x of { }

With dependently-typed features it is more useful (see :ghc-ticket:`2431`). For
example, consider these two candidate definitions of ``absurd``:

::

    data a :~: b where
      Refl :: a :~: a

    absurd :: True :~: False -> a
    absurd x = error "absurd"    -- (A)
    absurd x = case x of {}      -- (B)

We much prefer (B). Why? Because GHC can figure out that
``(True :~: False)`` is an empty type. So (B) has no partiality and GHC
is able to compile with :ghc-flag:`-Wincomplete-patterns` and
:ghc-flag:`-Werror`. On the other hand (A) looks dangerous, and GHC doesn't
check to make sure that, in fact, the function can never get called.



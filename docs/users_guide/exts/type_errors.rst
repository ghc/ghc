.. _custom-errors:

Custom compile-time errors
==========================

When designing embedded domain specific languages in Haskell, it is useful to
have something like ``error`` at the type level. In this way, the EDSL designer
may show a type error that is specific to the DSL, rather than the standard GHC
type error.

For example, consider a type class that is not intended to be used with
functions, but the user accidentally used it at a function type, perhaps
because they missed an argument to some function. Then, instead of getting the
standard GHC message about a missing instance, it would be nicer to emit a more
friendly message specific to the EDSL. Similarly, the reduction of a type-level
function may get stuck due to an error, at which point it would be nice to
report an EDSL specific error, rather than a generic error about an ambiguous
type.

To solve this, GHC provides a single type-level function, ::

    type family TypeError (msg :: ErrorMessage) :: k

along with a small type-level language (via :extension:`DataKinds`)
for constructing pretty-printed error messages, ::

    -- ErrorMessage is intended to be used as a kind
    data ErrorMessage =
          Text Symbol                        -- Show this text as is
        | forall t. ShowType t               -- Pretty print a type
        | ErrorMessage :<>: ErrorMessage     -- Put two chunks of error message next to each other
        | ErrorMessage :$$: ErrorMessage     -- Put two chunks of error message above each other

in the :base-ref:`GHC.TypeLits.` module.

For instance, we might use this interface to provide a more useful error
message for applications of ``show`` on unsaturated functions like this, ::

    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE TypeOperators #-}
    {-# LANGUAGE UndecidableInstances #-}

    import GHC.TypeLits

    instance TypeError (Text "Cannot 'Show' functions." :$$:
                        Text "Perhaps there is a missing argument?")
             => Show (a -> b) where
       showsPrec = error "unreachable"

    main = print negate

Which will produce the following compile-time error,

.. code-block:: none

    Test.hs:12:8: error:
        • Cannot 'Show' functions.
          Perhaps there is a missing argument?
        • In the expression: print negate
          In an equation for ‘main’: main = print negate




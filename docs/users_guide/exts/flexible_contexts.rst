.. _flexible-contexts:

Loosening restrictions on class contexts
----------------------------------------

.. extension:: FlexibleContexts
    :shortdesc: Remove some restrictions on class contexts

    :since: 6.8.1

    Remove the type-variable restriction on class contexts.

The :extension:`FlexibleContexts` extension lifts the Haskell 98 restriction that
the type-class constraints (anywhere they appear) must have the form *(class
type-variable)* or *(class (type-variable type1 type2 ... typen))*. With
:extension:`FlexibleContexts` these type signatures are perfectly okay::

      g :: Eq [a] => ...
      g :: Ord (T a ()) => ...


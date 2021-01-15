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

This extension does not affect equality constraints in an instance
context; they are permitted by :extension:`TypeFamilies` or :extension:`GADTs`.
      
Note that :extension:`FlexibleContexts` affects usages of class constraints,
in type signatures and other contexts. In contrast, :extension:`FlexibleInstances`
loosens a similar restriction in place when declaring a new instance.

.. _flexible-contexts:

The context of a type signature
-------------------------------

The :extension:`FlexibleContexts` extension lifts the Haskell 98 restriction that
the type-class constraints in a type signature must have the form *(class
type-variable)* or *(class (type-variable type1 type2 ... typen))*. With
:extension:`FlexibleContexts` these type signatures are perfectly okay
::

      g :: Eq [a] => ...
      g :: Ord (T a ()) => ...

The flag :extension:`FlexibleContexts` also lifts the corresponding restriction
on class declarations (:ref:`superclass-rules`) and instance
declarations (:ref:`instance-rules`).



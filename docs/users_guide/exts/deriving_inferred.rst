.. _deriving-inferred:

Inferred context for deriving clauses
-------------------------------------

The Haskell Report is vague about exactly when a ``deriving`` clause is
legal. For example: ::

      data T0 f a = MkT0 a         deriving( Eq )
      data T1 f a = MkT1 (f a)     deriving( Eq )
      data T2 f a = MkT2 (f (f a)) deriving( Eq )

The natural generated ``Eq`` code would result in these instance
declarations: ::

      instance Eq a         => Eq (T0 f a) where ...
      instance Eq (f a)     => Eq (T1 f a) where ...
      instance Eq (f (f a)) => Eq (T2 f a) where ...

The first of these is obviously fine. The second is still fine, although
less obviously. The third is not Haskell 98, and risks losing
termination of instances.

GHC takes a conservative position: it accepts the first two, but not the
third. The rule is this: each constraint in the inferred instance
context must consist only of type variables, with no repetitions.

This rule is applied regardless of flags. If you want a more exotic
context, you can write it yourself, using the `standalone deriving
mechanism <#stand-alone-deriving>`__.



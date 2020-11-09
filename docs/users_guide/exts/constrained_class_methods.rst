.. _class-method-types:

Constrained class method types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: ConstrainedClassMethods
    :shortdesc: Enable constrained class methods.

    :since: 6.8.1

    Allows the definition of further constraints on individual class methods.

Haskell 98 prohibits class method types to mention constraints on the
class type variable, thus: ::

      class Seq s a where
        fromList :: [a] -> s a
        elem     :: Eq a => a -> s a -> Bool

The type of ``elem`` is illegal in Haskell 98, because it contains the
constraint ``Eq a``, which constrains only the class type variable (in
this case ``a``). More precisely, a constraint in a class method signature is rejected if

- The constraint mentions at least one type variable.  So this is allowed: ::

     class C a where
       op1 :: HasCallStack => a -> a
       op2 :: (?x::Int) => Int -> a

- All of the type variables mentioned are bound by the class declaration, and none is locally quantified.  Examples: ::

     class C a where
       op3 :: Eq a => a -> a    -- Rejected: constrains class variable only
       op4 :: D b => a -> b     -- Accepted: constrains a locally-quantified variable `b`
       op5 :: D (a,b) => a -> b -- Accepted: constrains a locally-quantified variable `b`


GHC lifts this restriction with language extension
:extension:`ConstrainedClassMethods`. The restriction is a pretty stupid one in
the first place, so :extension:`ConstrainedClassMethods` is implied by
:extension:`MultiParamTypeClasses`.



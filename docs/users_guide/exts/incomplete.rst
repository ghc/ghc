.. _incomplete:

Switching off syntax for incomplete case matches
------------------------------------------------

.. extension:: NoIncomplete
    :shortdesc: Disable a variety of cases in Haskell where obviously-incomplete pattern
    matches would otherwise be allowed.

    :default: off
    :since: 9.2

Specifically, NoIncomplete makes the following things illegal:

   - Function definitions that fail to match some value, e.g.::

      f (Just x) = rhs
      -- but no Nothing case

     Corresponding warning flag: ``-fincomplete-patterns``.

   - Case expressions that fail to match some value, e.g.::

      case m of
        Just x -> rhs
        -- no Nothing case

     Corresponding warning flag: ``-fincomplete-patterns``.

   - Guards that fail to be detectably-complete, e.g.::

      myAbs x | x < 0 = negate x

      Corresponding warning flag: ``-fincomplete-patterns``.

   - Pattern bindings that may fail to match, e.g.::

      let Just x = m
      in body

     Corresponding warning flag: ``-fincomplete-uni-patterns``.

   - Lambdas that may fail to match, e.g.::

      (\(Just x) -> x)

     Corresponding warning flag: ``-fincomplete-uni-patterns``.

   - A record construction which is incomplete because there are missing fields, e.g.::

      data T = MkT { x :: Int, y :: Bool }
      t = MkT { x = 5 } -- No specification of y.

     Corresponding warning flag: ``-fmissing-fields``.

   - A record update where the field to be updated does not occur in every constructor of the record type, e.g.::

      data T = T1 { x :: Int } | T2
      f r = r { x = 3 } -- Would fail if T2 were passed to f

     Corresponding warning flag: ``-fincomplete-record-updates``.

   - An instance declaration where a method goes unspecified despite no default in the corresponding class declaration, e.g.::

      class C a where
        op1 :: a -> a
        op2 :: a -> [a]

      instance C Int where
        op1 = rhs
        -- No definition for op2, and no default method in the class declaration.

     Corresponding warning flag: ``-fmissing-methods``.

   - With ``-XFieldSelectors``, a data declaration using record syntax which defines fields that fail to occur in every constructor, e.g.::

      data T = T1 { x :: Int } | T2
      -- The defined field selector function x :: T -> Int would have been incomplete.

     Corresponding warning flag: ``-fpartial-fields``.

   - With ``-XMultiWayIf``, any multi-way if expression that is not guaranteed to match in some way (i.e. by having an ``otherwise`` or ``True`` branch), e.g.::

      if | x == 1 -> "a"
         | y < 7  -> "b"

     Corresponding warning flag: ``-fincomplete-patterns``.

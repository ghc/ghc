.. _overloaded-record-update:

Overloaded record update
------------------------

.. extension:: OverloadedRecordUpdate
    :shortdesc: Record '.' syntax record updates

    :since: 9.2.0

    Provides record '.' syntax in record updates e.g. ``x{foo.bar = 1}``.

**EXPERIMENTAL**
*This design of this extension may well change in the future. It would be inadvisable to start using this extension for long-lived libraries just yet.*

It's usual (but not required) that this extension be used in conjunction with :ref:`overloaded-record-dot`.

Example:

.. code-block:: haskell

  {-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, ScopedTypeVariables, PolyKinds, TypeApplications, DataKinds, FlexibleInstances #-}
  {-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
  {-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate, RebindableSyntax #-}

  import Prelude

  class HasField x r a | x r -> a where
    hasField :: r -> (a -> r, a)

  getField :: forall x r a . HasField x r a => r -> a
  getField = snd . hasField @x -- Note: a.x = is getField @"x" a.
  setField :: forall x r a . HasField x r a => r -> a -> r
  setField = fst . hasField @x -- Note : a{x = b} is setField @"x" a b.

  data Person = Person { name :: String } deriving Show
  instance HasField "name" Person String where
      hasField r = (\x -> case r of Person { .. } -> Person { name = x, .. }, name r)

  data Company = Company { company :: String, owner :: Person } deriving Show
  instance HasField "company" Company String where
      hasField r = (\x -> case r of Company { .. } -> Company { company = x, .. }, company r)
  instance HasField "owner" Company Person where
      hasField r = (\x -> case r of Company { .. } -> Company { owner = x, .. }, owner r)

  main = do
    let c = Company {company = "Acme Corp.", owner = Person { name = "Wile E. Coyote" }}

    -- Top-level update
    print $ c{company = "Acme United"} -- Company {company = "Acme United", owner = Person {name = "Wile E. Coyote"}}

    -- Nested update
    print $ c{owner.name = "Walter C. Johnsen"} -- Company {company = "Acme Corp.", owner = Person {name = "Walter C. Johnsen"}}

    -- Punned update
    let name = "Walter C. Johnsen"
    print $ c{owner.name}  -- Company {company = "Acme Corp.", owner = Person {name = "Walter C. Johnsen"}}

``OverloadedRecordUpdate`` works by desugaring record ``.`` update expressions to expressions involving the functions ``setField`` and ``getField``. Note that all record updates will be desugared to ``setField`` expressions whether they use ``.`` notation or not.

At this time, ``RebindableSyntax`` must be enabled when ``OverloadedRecordUpdate`` is and users are required to provide definitions for ``getField`` and ``setField``. We anticipate this restriction to be lifted in a future release of GHC with builtin support for ``setField``.

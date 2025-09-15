{-# LANGUAGE TypeFamilies, NamedWildCards, PolyKinds #-}

-- All declarations below are accepted when the NamedWildCards extension is not
-- enabled and the identifiers starting with _ are parsed as type variables.
-- They should remain valid when the extension is on.
--
-- See #11098 and comments in #10982

module NamedWildcardsAsTyVars where

type Synonym _a = _a -> _a

data A a _b = ACon a a Int

data B _a b = BCon _a (_a, Bool)

type family C a b where
  C _a _b = _a -> _a

type family D a b where
  D _a b = _a -> (_a, Int)

data family E a b
data instance E a _b = ECon a (a, Int)

data family F a b
data instance F _a b = FCon _a _a Bool

class G _a where
    gfoo :: _a -> _a

instance G Int where
    gfoo = (*2)

type family H a b where
  H _a _a = Int
  H _a _b = Bool

hfoo :: H String String
hfoo = 10

hbar :: H String Int
hbar = False

type family I (_a :: k) where
    I _t = Int

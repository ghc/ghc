{-# LANGUAGE TypeFamilies, TypeInType, ExplicitForAll, GADTs,
             UndecidableInstances, RankNTypes, ScopedTypeVariables #-}

module T14066a where

import Data.Proxy
import Data.Kind
import Data.Type.Bool


type family Bar x y where
  Bar (x :: a) (y :: b) = Int
  Bar (x :: c) (y :: d) = Bool   -- a,b,c,d should be SigTvs and unify appropriately


  -- the two k's, even though they have different scopes, should unify in the
  -- kind-check and then work in the type-check because Prox3 has been generalized

data Prox3 a where
  MkProx3a :: Prox3 (a :: k1)
  MkProx3b :: Prox3 (a :: k2)

  -- This probably should be rejected, as it's polymorphic recursion without a CUSK.
  -- But GHC accepts it because the polymorphic occurrence is at a type variable.
data T0 a = forall k (b :: k). MkT0 (T0 b) Int

  -- k and j should unify
type family G x a where
  G Int (b :: k) = Int
  G Bool (c :: j) = Bool

-- this last example just checks that GADT pattern-matching on kinds still works.
-- nothing new here.
data T (a :: k) where
  MkT :: T (a :: Type -> Type)

data S (a :: Type -> Type) where
  MkS :: S a

f :: forall k (a :: k). Proxy a -> T a -> ()
f _ MkT = let y :: S a
              y = MkS
          in ()

-- This is questionable. Should we use the RHS to determine dependency? It works
-- now, but if it stops working in the future, that's not a deal-breaker.
type P k a = Proxy (a :: k)


-- This is a challenge. It should be accepted, but only because c's kind is learned
-- to be Proxy True, allowing b to be assigned kind `a`. If we don't know c's kind,
-- then GHC would need to be convinced that If (c's kind) b d always has kind `a`.
-- Naively, we don't know about c's kind early enough.

data SameKind :: forall k. k -> k -> Type
type family IfK (e :: Proxy (j :: Bool)) (f :: m) (g :: n) :: If j m n where
   IfK (_ :: Proxy True)  f _ = f
   IfK (_ :: Proxy False) _ g = g
x :: forall c. (forall a b (d :: a). SameKind (IfK c b d) d) -> (Proxy (c :: Proxy True))
x _ = Proxy


f2 :: forall b. b -> Proxy Maybe
f2 x = fstOf3 y :: Proxy Maybe
  where
    y :: (Proxy a, Proxy c, b)
    y = (Proxy, Proxy, x)

fstOf3 (x, _, _) = x

f3 :: forall b. b -> Proxy Maybe
f3 x = fst y :: Proxy Maybe
  where
    y :: (Proxy a, b)
    y = (Proxy, x)

-- cf. dependent/should_fail/T14066h. Here, y's type does *not* capture any variables,
-- so it is generalized, even with MonoLocalBinds.
f4 x = (fst y :: Proxy Int, fst y :: Proxy Maybe)
  where
    y :: (Proxy a, Int)
    y = (Proxy, x)

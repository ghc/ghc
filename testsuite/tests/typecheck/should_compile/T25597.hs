{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module T25597 where
import           Data.Kind (Type)

data Env (f :: k -> Type) (as :: [k]) where
  ENil  :: Env f '[]
  ECons :: f a -> Env f as -> Env f (a ': as)

data Sig2 k = [k] :~> k

data DimSimple (s :: Sig2 k) where
  DimSimple :: OfLength as -> DimSimple (as ':~> a)

data OfLength as where
  LZ :: OfLength '[]
  LS :: OfLength as -> OfLength (a ': as)

class LiftOfLength f as t | t -> as where
  liftOfLength :: OfLength as -> f t

instance t ~ (as ':~> a) => LiftOfLength DimSimple as t where
  liftOfLength = undefined

data EnvI (sem :: [k] -> k -> Type) (a :: k)

type family Func sem as r where
  Func sem '[] r       = r
  Func sem (a ': as) r = sem a -> Func sem as r


type family FuncU (sem :: [k] -> k -> Type) (ss :: [Sig2 k])
                  (r :: k) = res | res -> sem r where
  FuncU sem '[] r = EnvI sem r
  FuncU sem ((as ':~> a) ': ss) r = Func (EnvI sem) as (EnvI sem a)
                                    -> FuncU sem ss r

lifts :: Env DimSimple ss -> FuncU sem ss r
lifts _ = undefined

-- The following version specialized to singletons does not cause an issue
type family FuncS (sem :: [k] -> k -> Type) (s :: Sig2 k)
                  (r :: k) = res | res -> sem r where
  FuncS sem (as ':~> a) r = Func (EnvI sem) as (EnvI sem a) -> EnvI sem r


lift :: DimSimple s -> FuncS sem s r
lift _ = undefined

-- The following code causes non termination of type checking in GHC 9.2, 9.8, 9.10, and 9.12
f :: (EnvI Sem a -> EnvI Sem b) -> EnvI Sem (a -> b)
f = lifts (ECons (liftOfLength (LS LZ)) ENil)

data Sem (env :: [Type]) a

-- Following versions have no issues in GHC 9.8
-- (I haven't tested other compilers but expect the similar results)
-- f = undefined $ lifts (ECons (liftOfLength (LS LZ)) ENil)
-- f = let h = lifts (ECons (liftOfLength (LS LZ)) ENil) in h
-- f = h where h = lifts (ECons (liftOfLength (LS LZ)) ENil)
-- f = lifts (ECons (DimSimple (LS LZ)) ENil)
-- f = lifts d where {d :: Env DimSimple '[ '[a] :~> b ]; d = (ECons (liftOfLength (LS LZ)) ENil) }
-- f = lift (liftOfLength (LS LZ))
-- f = (lifts :: Env DimSimple ss -> FuncU Sem ss r) (ECons (liftOfLength (LS LZ)) ENil)
-- f without its signature

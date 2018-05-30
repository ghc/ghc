-- TODO: arnaud: copyright notice

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS -Wno-missing-methods #-}

-- | This module defines the semi-ring (aka Rig) of weights, and associated
-- functions. Weights annotate arrow types to indicate the linearity of the
-- arrow (in the sense of linear types).
module Weight where
  -- TODO: arnaud list of exports
  --

import GhcPrelude

import Binary
import Control.Monad
import Data.Data
import Data.String
import Outputable
import Name
import NameEnv

--
-- * Core properties of weights
--
data CoreWeighted a = CoreWeighted {coreWeightedWeight :: CoreRig , coreWeightedThing :: a}
  deriving (Functor,Foldable,Traversable,Data)

data CoreRig = CZero | COne | COmega
  deriving (Eq,Ord,Data)

instance Num CoreRig where
  CZero * _ = CZero
  _ * CZero = CZero
  COmega * COne = COmega
  COne * COmega = COmega
  COne * COne   = COne
  COmega * COmega = COmega

  CZero + x = x
  x + CZero = x
  _ + _ = COmega

instance Outputable CoreRig where
  ppr CZero = fromString "0"
  ppr COne = fromString "1"
  ppr COmega = fromString "ω"

instance Binary CoreRig where
  put_ bh CZero = putByte bh 0
  put_ bh COne = putByte bh 1
  put_ bh COmega = putByte bh 2

  get bh = do
    h <- getByte bh
    case h of
      0 -> return CZero
      1 -> return COne
      2 -> return COmega
      _ -> fail "Invalid binary data for multiplicity found"

-- | @subweight w1 w2@ check whether a value of weight @w1@ is allowed where a
-- value of weight @w2@ is expected. This is a partial order.
subweightC :: CoreRig -> CoreRig -> Bool
subweightC _     COmega = True
subweightC CZero  CZero  = True
-- It is no mistake: 'Zero' is not a subweight of 'One': a value which must be
-- used zero times cannot be used one time.
-- Zero = {0}
-- One  = {1}
-- Omega = {0...}
subweightC COne   COne   = True
subweightC _     _     = False

-- | @sup w1 w2@ returns the smallest weight larger than or equal to both @w1@
-- and @w2@.
supC :: CoreRig -> CoreRig -> CoreRig
supC CZero  CZero  = CZero
supC COne   COne   = COne
supC COmega COmega = COmega
supC _     _     = COmega

unrestricted :: a -> CoreWeighted a
unrestricted a = CoreWeighted COmega a

tyweight :: a -> CoreWeighted a
tyweight a = CoreWeighted COmega a

linear :: a -> CoreWeighted a
linear a = CoreWeighted COne a

mkCoreWeighted :: CoreRig -> a -> CoreWeighted a
mkCoreWeighted c a = CoreWeighted c a

instance Outputable a => Outputable (CoreWeighted a) where
   ppr (CoreWeighted cnt t) = -- ppr cnt <> ppr t
                          ppr t

instance Binary a => Binary (CoreWeighted a) where
  put_ bh (CoreWeighted r x) = put_ bh r >> put_ bh x
  get bh = do
    r <- get bh
    x <- get bh
    return $ CoreWeighted r x

coreWeightedSet :: CoreWeighted a -> b -> CoreWeighted b
coreWeightedSet x b = fmap (\_->b) x

scaleCoreWeighted :: CoreRig -> CoreWeighted a -> CoreWeighted a
scaleCoreWeighted w x =
  x { coreWeightedWeight = w * coreWeightedWeight x }

--
-- * Usage environments
--

-- | Like in the mathematical presentation, we have a context on which the
-- semi-ring of weights acts (that is, 'UsageEnv' is a 'Rig'-module). Unlike the
-- mathematical presentation they are not type contexts, but only contain
-- weights corresponding to the weight required for a given variable in a
-- type-checked expression. The reason is twofold: it interacts less with the
-- rest of the type-checking infrastructure so it is easier to fit into the
-- existing implementation, and it is always an inferred datum (in the sense of
-- bidirectional type checking, i.e. it is an output of the type-checking
-- procedure) which makes it possible to use addition and scaling like in the
-- mathematical presentation, rather than subtraction and division which are
-- much harder to get right. The module structure is the point-wise extension of
-- the action of 'Rig' on itself, every absent name being considered to map to
-- 'Zero'.
newtype CoreUsageEnv = CoreUsageEnv (NameEnv CoreRig)

unitCUE :: NamedThing n => n -> CoreRig -> CoreUsageEnv
unitCUE x w = CoreUsageEnv $ unitNameEnv (getName x) w

mkCUE :: [CoreWeighted Name] -> CoreUsageEnv
mkCUE ws = CoreUsageEnv $ mkNameEnv (map (\wx -> (coreWeightedThing wx,coreWeightedWeight wx)) ws)

zeroCUE, emptyCUE :: CoreUsageEnv
zeroCUE = CoreUsageEnv emptyNameEnv

emptyCUE = zeroCUE

addCUE :: CoreUsageEnv -> CoreUsageEnv -> CoreUsageEnv
addCUE (CoreUsageEnv e1) (CoreUsageEnv e2) = CoreUsageEnv $
  plusNameEnv_C (+) e1 e2

addCUEs :: [CoreUsageEnv] -> CoreUsageEnv
addCUEs = foldr addCUE emptyCUE

scaleCUE :: CoreRig -> CoreUsageEnv -> CoreUsageEnv
scaleCUE w (CoreUsageEnv e) = CoreUsageEnv $
  mapNameEnv (w*) e

supCUE :: CoreUsageEnv -> CoreUsageEnv -> CoreUsageEnv
supCUE (CoreUsageEnv e1) (CoreUsageEnv e2) = CoreUsageEnv $
  plusNameEnv_CD supC e1 CZero e2 CZero

-- TODO: arnaud: both delete function: unify argument order with existing similar functions.
-- | @deleteUEAsserting w x env@ deletes the binding to @x@ in @env@ under one
-- condition: if @x@ is bound to @w'@ in @env@, then @w'@ must be a subweight of
-- @w@, if @x@ is not bound in @env@ then 'Zero' must be a subweight of @W@. If
-- the condition is not met, then @Nothing@ is returned.
deleteCUEAsserting :: CoreRig -> Name -> CoreUsageEnv -> Maybe CoreUsageEnv
deleteCUEAsserting w x (CoreUsageEnv e) | Just w' <- lookupNameEnv e x = do
  guard (subweightC w' w)
  return $ CoreUsageEnv (delFromNameEnv e x)
deleteCUEAsserting w _x (CoreUsageEnv e) = do
  guard (subweightC CZero w)
  return $ CoreUsageEnv e

deleteCUE :: Name -> CoreUsageEnv -> CoreUsageEnv
deleteCUE x (CoreUsageEnv e) = CoreUsageEnv $ delFromNameEnv e x


-- | |lookupUE x env| returns the weight assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero|.
lookupCUE :: NamedThing n => CoreUsageEnv -> n -> CoreRig
lookupCUE (CoreUsageEnv e) x =
  case lookupNameEnv e (getName x) of
    Just w  -> w
    Nothing -> CZero

instance Outputable CoreUsageEnv where
  ppr (CoreUsageEnv ne) = text "UsageEnv:" <+> ppr ne

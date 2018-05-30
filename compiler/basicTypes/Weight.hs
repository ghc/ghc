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

data Rig = Zero | One | Omega
  deriving (Eq,Ord,Data)

instance Num Rig where
  Zero * _ = Zero
  _ * Zero = Zero
  Omega * One = Omega
  One * Omega = Omega
  One * One   = One
  Omega * Omega = Omega

  Zero + x = x
  x + Zero = x
  _ + _ = Omega

instance Outputable Rig where
  ppr Zero = fromString "0"
  ppr One = fromString "1"
  ppr Omega = fromString "ω"

instance Binary Rig where
  put_ bh Zero = putByte bh 0
  put_ bh One = putByte bh 1
  put_ bh Omega = putByte bh 2

  get bh = do
    h <- getByte bh
    case h of
      0 -> return Zero
      1 -> return One
      2 -> return Omega
      _ -> fail "Invalid binary data for multiplicity found"


-- | @subweight w1 w2@ check whether a value of weight @w1@ is allowed where a
-- value of weight @w2@ is expected. This is a partial order.
subweight :: Rig -> Rig -> Bool
subweight _     Omega = True
subweight Zero  Zero  = True
-- It is no mistake: 'Zero' is not a subweight of 'One': a value which must be
-- used zero times cannot be used one time.
-- Zero = {0}
-- One  = {1}
-- Omega = {0...}
subweight One   One   = True
subweight _     _     = False

-- | @sup w1 w2@ returns the smallest weight larger than or equal to both @w1@
-- and @w2@.
sup :: Rig -> Rig -> Rig
sup Zero  Zero  = Zero
sup One   One   = One
sup Omega Omega = Omega
sup _     _     = Omega


--
-- * Utilities
--

-- | A shorthand for data with an attached 'Rig' element (the weight).
data Weighted a = Weighted {weightedWeight :: Rig, weightedThing :: a}
  deriving (Functor,Foldable,Traversable,Data)

unrestricted, linear, staticOnly, tyweight :: a -> Weighted a
unrestricted = Weighted Omega
linear = Weighted One
staticOnly = Weighted Zero

-- Used for type arguments in core
tyweight = Weighted Omega

knownOmega :: Weighted a -> a
knownOmega = weightedThing

irrelevantWeight :: Weighted a -> a
irrelevantWeight = weightedThing

mkWeighted :: Rig -> a -> Weighted a
mkWeighted = Weighted

instance Outputable a => Outputable (Weighted a) where
   ppr (Weighted cnt t) = -- ppr cnt <> ppr t
                          ppr t

-- MattP: For now we don't print the weight by default as it creeps into
-- error messages.

instance Binary a => Binary (Weighted a) where
  put_ bh (Weighted r x) = put_ bh r >> put_ bh x
  get bh = do
    r <- get bh
    x <- get bh
    return $ Weighted r x

weightedSet :: Weighted a -> b -> Weighted b
weightedSet x b = fmap (\_->b) x

scaleWeighted :: Rig -> Weighted a -> Weighted a
scaleWeighted w x =
  x { weightedWeight = w * weightedWeight x }


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
newtype UsageEnv = UsageEnv (NameEnv Rig)

unitUE :: NamedThing n => n -> Rig -> UsageEnv
unitUE x w = UsageEnv $ unitNameEnv (getName x) w

mkUE :: [Weighted Name] -> UsageEnv
mkUE ws = UsageEnv $ mkNameEnv (map (\wx -> (weightedThing wx,weightedWeight wx)) ws)

zeroUE, emptyUE :: UsageEnv
zeroUE = UsageEnv emptyNameEnv

emptyUE = zeroUE

addUE :: UsageEnv -> UsageEnv -> UsageEnv
addUE (UsageEnv e1) (UsageEnv e2) = UsageEnv $
  plusNameEnv_C (+) e1 e2

addUEs :: [UsageEnv] -> UsageEnv
addUEs = foldr addUE emptyUE

scaleUE :: Rig -> UsageEnv -> UsageEnv
scaleUE w (UsageEnv e) = UsageEnv $
  mapNameEnv (w*) e

supUE :: UsageEnv -> UsageEnv -> UsageEnv
supUE (UsageEnv e1) (UsageEnv e2) = UsageEnv $
  plusNameEnv_CD sup e1 Zero e2 Zero

-- TODO: arnaud: both delete function: unify argument order with existing similar functions.
-- | @deleteUEAsserting w x env@ deletes the binding to @x@ in @env@ under one
-- condition: if @x@ is bound to @w'@ in @env@, then @w'@ must be a subweight of
-- @w@, if @x@ is not bound in @env@ then 'Zero' must be a subweight of @W@. If
-- the condition is not met, then @Nothing@ is returned.
deleteUEAsserting :: Rig -> Name -> UsageEnv -> Maybe UsageEnv
deleteUEAsserting w x (UsageEnv e) | Just w' <- lookupNameEnv e x = do
  guard (subweight w' w)
  return $ UsageEnv (delFromNameEnv e x)
deleteUEAsserting w _x (UsageEnv e) = do
  guard (subweight Zero w)
  return $ UsageEnv e

deleteUE :: Name -> UsageEnv -> UsageEnv
deleteUE x (UsageEnv e) = UsageEnv $ delFromNameEnv e x


-- | |lookupUE x env| returns the weight assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero|.
lookupUE :: NamedThing n => UsageEnv -> n -> Rig
lookupUE (UsageEnv e) x =
  case lookupNameEnv e (getName x) of
    Just w  -> w
    Nothing -> Zero

instance Outputable UsageEnv where
  ppr (UsageEnv ne) = text "UsageEnv:" <+> ppr ne

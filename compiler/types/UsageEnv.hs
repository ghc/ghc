{-# LANGUAGE ViewPatterns #-}
module UsageEnv where

import GhcPrelude
import Weight
import NameEnv
import Outputable
import Name
import Type (flattenRig)

import Control.Monad
import Data.Maybe

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

subweight :: Rig -> Rig -> Bool
subweight r1 r2 = case subweightMaybe r1 r2 of
                    Smaller -> True
                    _ -> False


data IsSubweight = Smaller -- Definitely a subweight
                 | Larger  -- Definitely not a subweight
                 | Unknown -- Could be a subweight, need to ask the typechecker
                 deriving (Show, Eq, Ord)

isUnknown :: IsSubweight -> Bool
isUnknown Unknown = True
isUnknown _ = False

instance Outputable IsSubweight where
  ppr = text . show


-- | @subweight w1 w2@ check whether a value of weight @w1@ is allowed where a
-- value of weight @w2@ is expected. This is a partial order.
subweightMaybe :: Rig -> Rig -> IsSubweight
subweightMaybe (flattenRig -> r1) (flattenRig -> r2) = go r1 r2
  where
    go _     Omega = Smaller
    go Zero  Zero  = Smaller
    go _     Zero  = Larger
    go Zero  One   = Larger
    -- It is no mistake: 'Zero' is not a subweight of 'One': a value which must be
    -- used zero times cannot be used one time.
    -- Zero = {0}
    -- One  = {1}
    -- Omega = {0...}
    go One   One   = Smaller
--    go (RigTy t) (RigTy t') = Unknown
    go _     _     = Unknown

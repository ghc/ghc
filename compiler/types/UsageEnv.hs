{-# LANGUAGE ViewPatterns #-}
module UsageEnv where

import Data.Foldable
import GhcPrelude
import Weight
import Name
import NameEnv
import UniqFM ( nonDetEltsUFM, plusUFM_CD )
import Outputable
import Name
import TyCoRep ( Rig, Weighted )

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

addUEs :: Foldable t => t UsageEnv -> UsageEnv
addUEs = foldr addUE zeroUE

scaleUE :: Rig -> UsageEnv -> UsageEnv
scaleUE w (UsageEnv e) = UsageEnv $
  mapNameEnv (w*) e

supUE :: UsageEnv -> UsageEnv -> UsageEnv
supUE (UsageEnv e1) (UsageEnv e2) = UsageEnv $
  plusNameEnv_CD sup e1 Zero e2 Zero

supUEs :: [UsageEnv] -> UsageEnv
supUEs [] = zeroUE -- TODO: arnaud: this is incorrect, it should be the bottom
                   -- usage env, but I haven't defined it yet. Then we could use
                   -- a foldr and wouldn't need to special-case the empty list
                   -- as currently.
supUEs l = foldr1 supUE l

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

deleteUE :: NamedThing n => n -> UsageEnv -> UsageEnv
deleteUE x (UsageEnv e) = UsageEnv $ delFromNameEnv e (getName x)

deleteListUE :: NamedThing n => [n] -> UsageEnv -> UsageEnv
deleteListUE xs e = foldl' (flip deleteUE) e xs


-- | |lookupUE x env| returns the weight assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero|.
lookupUE :: NamedThing n => UsageEnv -> n -> Rig
lookupUE (UsageEnv e) x =
  case lookupNameEnv e (getName x) of
    Just w  -> w
    Nothing -> Zero

mapUE :: (Rig -> Rig) -> UsageEnv -> UsageEnv
mapUE f (UsageEnv ue) = UsageEnv $ fmap f ue

allUE :: (Rig -> Bool) -> UsageEnv -> Bool
allUE p (UsageEnv ue) = all p (nonDetEltsUFM ue)

instance Outputable UsageEnv where
  ppr (UsageEnv ne) = text "UsageEnv:" <+> ppr ne

subweight :: Rig -> Rig -> Bool
subweight r1 r2 = case subweightMaybe r1 r2 of
                    Smaller -> True
                    _ -> False

subweightUE :: UsageEnv -> UsageEnv -> Bool
subweightUE (UsageEnv lhs) (UsageEnv rhs) =
    all (uncurry subweight) (nonDetEltsUFM pairs)
  where
    pairs =
      plusUFM_CD (,) lhs Zero rhs Zero

{-# LANGUAGE ViewPatterns #-}
module UsageEnv where

import Data.Foldable
import GhcPrelude
import Multiplicity
import Name
import NameEnv
import UniqFM ( nonDetEltsUFM, plusUFM_CD )
import Outputable
import TyCoRep ( Mult, Scaled )

import Control.Monad

--
-- * Usage environments
--

-- | Like in the mathematical presentation, we have a context on which the
-- semi-ring of multiplicities acts (that is, 'UsageEnv' is a 'Mult'-module). Unlike the
-- mathematical presentation they are not type contexts, but only contain
-- multiplicities corresponding to the multiplicity required for a given variable in a
-- type-checked expression. The reason is twofold: it interacts less with the
-- rest of the type-checking infrastructure so it is easier to fit into the
-- existing implementation, and it is always an inferred datum (in the sense of
-- bidirectional type checking, i.e. it is an output of the type-checking
-- procedure) which makes it possible to use addition and scaling like in the
-- mathematical presentation, rather than subtraction and division which are
-- much harder to get right. The module structure is the point-wise extension of
-- the action of 'Mult' on itself, every absent name being considered to map to
-- 'Zero'.
newtype UsageEnv = UsageEnv (NameEnv Mult)

unitUE :: NamedThing n => n -> Mult -> UsageEnv
unitUE x w = UsageEnv $ unitNameEnv (getName x) w

mkUE :: [Scaled Name] -> UsageEnv
mkUE ws = UsageEnv $ mkNameEnv (map (\wx -> (scaledThing wx,scaledMult wx)) ws)

zeroUE, emptyUE :: UsageEnv
zeroUE = UsageEnv emptyNameEnv

emptyUE = zeroUE

addUE :: UsageEnv -> UsageEnv -> UsageEnv
addUE (UsageEnv e1) (UsageEnv e2) = UsageEnv $
  plusNameEnv_C (+) e1 e2

addUEs :: Foldable t => t UsageEnv -> UsageEnv
addUEs = foldr addUE zeroUE

scaleUE :: Mult -> UsageEnv -> UsageEnv
scaleUE w (UsageEnv e) = UsageEnv $
  mapNameEnv (w*) e

supUE :: UsageEnv -> UsageEnv -> UsageEnv
supUE (UsageEnv e1) (UsageEnv e2) = UsageEnv $
  plusNameEnv_CD sup e1 Zero e2 Zero

supUEs :: [UsageEnv] -> UsageEnv
supUEs [] = zeroUE -- This is incorrect, it should be the bottom usage env, but
                   -- it isn't defined yet. Then we could use a foldr and
                   -- wouldn't need to special-case the empty list as
                   -- currently. As a consequence empty cases do not have the
                   -- right typing rule. This should be easier to solve after
                   -- inference is implemented.
supUEs l = foldr1 supUE l

-- | @deleteUEAsserting w x env@ deletes the binding to @x@ in @env@ under one
-- condition: if @x@ is bound to @w'@ in @env@, then @w'@ must be a submult of
-- @w@, if @x@ is not bound in @env@ then 'Zero' must be a submult of @W@. If
-- the condition is not met, then @Nothing@ is returned.
deleteUEAsserting :: UsageEnv -> Mult -> Name -> Maybe UsageEnv
deleteUEAsserting (UsageEnv e) w x | Just w' <- lookupNameEnv e x = do
  guard (submult w' w)
  return $ UsageEnv (delFromNameEnv e x)
deleteUEAsserting (UsageEnv e) w _x = do
  guard (submult Zero w)
  return $ UsageEnv e

deleteUE :: NamedThing n => UsageEnv -> n -> UsageEnv
deleteUE (UsageEnv e) x = UsageEnv $ delFromNameEnv e (getName x)

deleteListUE :: NamedThing n => UsageEnv -> [n] -> UsageEnv
deleteListUE e xs = foldl' deleteUE e xs


-- | |lookupUE x env| returns the multiplicity assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero|.
lookupUE :: NamedThing n => UsageEnv -> n -> Mult
lookupUE (UsageEnv e) x =
  case lookupNameEnv e (getName x) of
    Just w  -> w
    Nothing -> Zero

mapUE :: (Mult -> Mult) -> UsageEnv -> UsageEnv
mapUE f (UsageEnv ue) = UsageEnv $ fmap f ue

allUE :: (Mult -> Bool) -> UsageEnv -> Bool
allUE p (UsageEnv ue) = all p (nonDetEltsUFM ue)

instance Outputable UsageEnv where
  ppr (UsageEnv ne) = text "UsageEnv:" <+> ppr ne

submult :: Mult -> Mult -> Bool
submult r1 r2 = case submultMaybe r1 r2 of
                    Smaller -> True
                    _ -> False

submultUE :: UsageEnv -> UsageEnv -> Bool
submultUE (UsageEnv lhs) (UsageEnv rhs) =
    all (uncurry submult) (nonDetEltsUFM pairs)
  where
    pairs =
      plusUFM_CD (,) lhs Zero rhs Zero

{-# LANGUAGE ViewPatterns #-}
module UsageEnv (UsageEnv, addUsage, multUsage, usageToMult, emptyUE, zeroUE,
                 lookupUE, scaleUE, deleteUE, addUE, Usage(..), unitUE,
                 supUE, supUEs) where

import Data.Foldable
import GhcPrelude
import Multiplicity
import Name
import NameEnv
import Outputable

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
data Usage = Zero | MUsage Mult

instance Outputable Usage where
  ppr Zero = text "0"
  ppr (MUsage x) = ppr x

usageToMult :: Usage -> Mult
usageToMult Zero       = Omega
usageToMult (MUsage m) = m

addUsage :: Usage -> Usage -> Usage
addUsage Zero x = x
addUsage x Zero = x
addUsage (MUsage x) (MUsage y) = MUsage $ mkMultAdd x y

multUsage :: Usage -> Usage -> Usage
multUsage Zero _ = Zero
multUsage _ Zero = Zero
multUsage (MUsage x) (MUsage y) = MUsage $ mkMultMul x y


newtype UsageEnv = UsageEnv (NameEnv Mult)

unitUE :: NamedThing n => n -> Mult -> UsageEnv
unitUE x w = UsageEnv $ unitNameEnv (getName x) w

zeroUE, emptyUE :: UsageEnv
zeroUE = UsageEnv emptyNameEnv

emptyUE = zeroUE

addUE :: UsageEnv -> UsageEnv -> UsageEnv
addUE (UsageEnv e1) (UsageEnv e2) = UsageEnv $
  plusNameEnv_C mkMultAdd e1 e2

scaleUE :: Mult -> UsageEnv -> UsageEnv
scaleUE w (UsageEnv e) = UsageEnv $
  mapNameEnv (mkMultMul w) e

supUE :: UsageEnv -> UsageEnv -> UsageEnv
supUE (UsageEnv e1) (UsageEnv e2) = UsageEnv $
  plusNameEnv_CD mkMultSup e1 Omega e2 Omega
-- Note: If you are changing this logic, check 'mkMultSup' in Multiplicity as well.

supUEs :: [UsageEnv] -> UsageEnv
supUEs [] = zeroUE -- This is incorrect, it should be the bottom usage env, but
                   -- it isn't defined yet. Then we could use a foldr and
                   -- wouldn't need to special-case the empty list as
                   -- currently. As a consequence empty cases do not have the
                   -- right typing rule. This should be easier to solve after
                   -- inference is implemented.
supUEs l = foldr1 supUE l

deleteUE :: NamedThing n => UsageEnv -> n -> UsageEnv
deleteUE (UsageEnv e) x = UsageEnv $ delFromNameEnv e (getName x)


-- | |lookupUE x env| returns the multiplicity assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero|.
lookupUE :: NamedThing n => UsageEnv -> n -> Usage
lookupUE (UsageEnv e) x =
  case lookupNameEnv e (getName x) of
    Just w  -> MUsage w
    Nothing -> Zero

instance Outputable UsageEnv where
  ppr (UsageEnv ne) = text "UsageEnv:" <+> ppr ne

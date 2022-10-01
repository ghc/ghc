module GHC.Core.UsageEnv
  ( Usage(..)
  , UsageEnv
  , addUE
  , addUsage
  , bottomUE
  , deleteUE
  , lookupUE
  , popUE
  , scaleUE
  , scaleUsage
  , supUE
  , supUEs
  , singleUsageUE
  , zeroUE
  ) where

import Data.Foldable
import GHC.Prelude
import GHC.Core.Multiplicity
import GHC.Types.Var
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Utils.Outputable
import GHC.Utils.Panic

--
-- * Usage environments
--

-- The typechecker and the linter output usage environments. See Note [Usages]
-- in Multiplicity. Every absent name being considered to map to 'Zero' of
-- 'Bottom' depending on a flag. See Note [Zero as a usage] in Multiplicity, see
-- Note [Bottom as a usage] in Multiplicity.

data Usage = Zero | Bottom | MUsage Mult

instance Outputable Usage where
  ppr Zero = text "0"
  ppr Bottom = text "Bottom"
  ppr (MUsage x) = ppr x

addUsage :: Usage -> Usage -> Usage
addUsage Zero x = x
addUsage x Zero = x
addUsage Bottom x = x
addUsage x Bottom = x
addUsage (MUsage x) (MUsage y) = MUsage $ mkMultAdd x y

scaleUsage :: Mult -> Usage -> Usage
scaleUsage OneTy Bottom     = Bottom
scaleUsage _     Zero       = Zero
scaleUsage x     Bottom     = MUsage x
scaleUsage x     (MUsage y) = MUsage $ mkMultMul x y

-- For now, we use extra multiplicity Bottom for empty case.
data UsageEnv = UsageEnv !(NameEnv Mult) Bool

-- | Record a single usage of an Id, i.e. {n: 1}
-- Exception: We do not record external names (both GlobalIds and top-level LocalIds)
-- because they're not relevant to linearity checking.
singleUsageUE :: Id -> UsageEnv
singleUsageUE x | isExternalName n = zeroUE
                | otherwise = UsageEnv (unitNameEnv n OneTy) False
  where n = getName x

zeroUE, bottomUE :: UsageEnv
zeroUE = UsageEnv emptyNameEnv False

bottomUE = UsageEnv emptyNameEnv True

addUE :: UsageEnv -> UsageEnv -> UsageEnv
addUE (UsageEnv e1 b1) (UsageEnv e2 b2) =
  UsageEnv (plusNameEnv_C mkMultAdd e1 e2) (b1 || b2)

scaleUE :: Mult -> UsageEnv -> UsageEnv
scaleUE OneTy ue = ue
scaleUE w (UsageEnv e _) =
  UsageEnv (mapNameEnv (mkMultMul w) e) False

supUE :: UsageEnv -> UsageEnv -> UsageEnv
supUE (UsageEnv e1 False) (UsageEnv e2 False) =
  UsageEnv (plusNameEnv_CD mkMultSup e1 ManyTy e2 ManyTy) False
supUE (UsageEnv e1 b1) (UsageEnv e2 b2) = UsageEnv (plusNameEnv_CD2 combineUsage e1 e2) (b1 && b2)
   where combineUsage (Just x) (Just y) = mkMultSup x y
         combineUsage Nothing  (Just x) | b1        = x
                                        | otherwise = ManyTy
         combineUsage (Just x) Nothing  | b2        = x
                                        | otherwise = ManyTy
         combineUsage Nothing  Nothing  = pprPanic "supUE" (ppr e1 <+> ppr e2)
-- Note: If you are changing this logic, check 'mkMultSup' in Multiplicity as well.

-- Used with @f = '[]'@ and @f = 'NonEmpty'@
supUEs :: Foldable f => f UsageEnv -> UsageEnv
supUEs = foldr supUE bottomUE

-- INLINE to ensure specialization at use site, and to avoid multiple specialization on the same
-- type
{-# INLINE supUEs #-}

deleteUE :: NamedThing n => UsageEnv -> n -> UsageEnv
deleteUE (UsageEnv e b) x = UsageEnv (delFromNameEnv e (getName x)) b

-- | |lookupUE x env| returns the multiplicity assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero| or |Bottom|.
lookupUE :: NamedThing n => UsageEnv -> n -> Usage
lookupUE (UsageEnv e has_bottom) x =
  case lookupNameEnv e (getName x) of
    Just w  -> MUsage w
    Nothing -> if has_bottom then Bottom else Zero

popUE :: NamedThing n => UsageEnv -> n -> (Usage, UsageEnv)
popUE ue x = (lookupUE ue x, deleteUE ue x)

instance Outputable UsageEnv where
  ppr (UsageEnv ne b) = text "UsageEnv:" <+> ppr ne <+> ppr b

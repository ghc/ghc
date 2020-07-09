{-# LANGUAGE ViewPatterns #-}
module GHC.Core.UsageEnv (UsageEnv, addUsage, scaleUsage, zeroUE,
                          lookupUE, scaleUE, deleteUE, addUE, Usage(..), unitUE,
                          bottomUE, supUE, supUEs) where

import Data.Foldable
import GHC.Prelude
import GHC.Core.Multiplicity
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Utils.Outputable

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
scaleUsage One Bottom     = Bottom
scaleUsage _   Zero       = Zero
scaleUsage x   Bottom     = MUsage x
scaleUsage x   (MUsage y) = MUsage $ mkMultMul x y

-- For now, we use extra multiplicity Bottom for empty case.
data UsageEnv = UsageEnv (NameEnv Mult) Bool

unitUE :: NamedThing n => n -> Mult -> UsageEnv
unitUE x w = UsageEnv (unitNameEnv (getName x) w) False

zeroUE, bottomUE :: UsageEnv
zeroUE = UsageEnv emptyNameEnv False

bottomUE = UsageEnv emptyNameEnv True

addUE :: UsageEnv -> UsageEnv -> UsageEnv
addUE (UsageEnv e1 b1) (UsageEnv e2 b2) =
  UsageEnv (plusNameEnv_C mkMultAdd e1 e2) (b1 || b2)

scaleUE :: Mult -> UsageEnv -> UsageEnv
scaleUE One ue = ue
scaleUE w (UsageEnv e _) =
  UsageEnv (mapNameEnv (mkMultMul w) e) False

supUE :: UsageEnv -> UsageEnv -> UsageEnv
supUE (UsageEnv e1 False) (UsageEnv e2 False) =
  UsageEnv (plusNameEnv_CD mkMultSup e1 Many e2 Many) False
supUE (UsageEnv e1 b1) (UsageEnv e2 b2) = UsageEnv (plusNameEnv_CD2 combineUsage e1 e2) (b1 && b2)
   where combineUsage (Just x) (Just y) = mkMultSup x y
         combineUsage Nothing  (Just x) | b1        = x
                                        | otherwise = Many
         combineUsage (Just x) Nothing  | b2        = x
                                        | otherwise = Many
         combineUsage Nothing  Nothing  = pprPanic "supUE" (ppr e1 <+> ppr e2)
-- Note: If you are changing this logic, check 'mkMultSup' in Multiplicity as well.

supUEs :: [UsageEnv] -> UsageEnv
supUEs = foldr supUE bottomUE


deleteUE :: NamedThing n => UsageEnv -> n -> UsageEnv
deleteUE (UsageEnv e b) x = UsageEnv (delFromNameEnv e (getName x)) b

-- | |lookupUE x env| returns the multiplicity assigned to |x| in |env|, if |x| is not
-- bound in |env|, then returns |Zero| or |Bottom|.
lookupUE :: NamedThing n => UsageEnv -> n -> Usage
lookupUE (UsageEnv e has_bottom) x =
  case lookupNameEnv e (getName x) of
    Just w  -> MUsage w
    Nothing -> if has_bottom then Bottom else Zero

instance Outputable UsageEnv where
  ppr (UsageEnv ne b) = text "UsageEnv:" <+> ppr ne <+> ppr b

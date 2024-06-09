{-# LANGUAGE MultiWayIf, PatternSynonyms #-}

-- TODO Everthing in this module should be moved to
-- Language.Haskell.Syntax.Decls

module Language.Haskell.Syntax.Specificity (
        -- * ForAllTyFlags
        ForAllTyFlag(Invisible,Required,Specified,Inferred),
        Specificity(..),
        isVisibleForAllTyFlag, isInvisibleForAllTyFlag, isInferredForAllTyFlag,
        isSpecifiedForAllTyFlag,
        coreTyLamForAllTyFlag,
        ) where

import Prelude

import Data.Data

-- | ForAllTyFlag
--
-- Is something required to appear in source Haskell ('Required'),
-- permitted by request ('Specified') (visible type application), or
-- prohibited entirely from appearing in source Haskell ('Inferred')?
-- See Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility] in "GHC.Core.TyCo.Rep"
data ForAllTyFlag = Invisible !Specificity
                  | Required
  deriving (Eq, Ord, Data)
  -- (<) on ForAllTyFlag means "is less visible than"

-- | Whether an 'Invisible' argument may appear in source Haskell.
data Specificity = InferredSpec
                   -- ^ the argument may not appear in source Haskell, it is
                   -- only inferred.
                 | SpecifiedSpec
                   -- ^ the argument may appear in source Haskell, but isn't
                   -- required.
  deriving (Eq, Ord, Data)

pattern Inferred, Specified :: ForAllTyFlag
pattern Inferred  = Invisible InferredSpec
pattern Specified = Invisible SpecifiedSpec

{-# COMPLETE Required, Specified, Inferred #-}

-- | Does this 'ForAllTyFlag' classify an argument that is written in Haskell?
isVisibleForAllTyFlag :: ForAllTyFlag -> Bool
isVisibleForAllTyFlag af = not (isInvisibleForAllTyFlag af)

-- | Does this 'ForAllTyFlag' classify an argument that is not written in Haskell?
isInvisibleForAllTyFlag :: ForAllTyFlag -> Bool
isInvisibleForAllTyFlag (Invisible {}) = True
isInvisibleForAllTyFlag Required       = False

isInferredForAllTyFlag :: ForAllTyFlag -> Bool
-- More restrictive than isInvisibleForAllTyFlag
isInferredForAllTyFlag (Invisible InferredSpec) = True
isInferredForAllTyFlag _                        = False

isSpecifiedForAllTyFlag :: ForAllTyFlag -> Bool
-- More restrictive than isInvisibleForAllTyFlag
isSpecifiedForAllTyFlag (Invisible SpecifiedSpec) = True
isSpecifiedForAllTyFlag _                         = False

coreTyLamForAllTyFlag :: ForAllTyFlag
-- ^ The ForAllTyFlag on a (Lam a e) term, where `a` is a type variable.
-- If you want other ForAllTyFlag, use a cast.
-- See Note [Required foralls in Core] in GHC.Core.TyCo.Rep
coreTyLamForAllTyFlag = Specified

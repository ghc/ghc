{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Types.DefaultEnv
   ( ClassDefaults (..)
   , DefaultProvenance (..)
   , DefaultEnv
   , emptyDefaultEnv
   , isEmptyDefaultEnv
   , defaultEnv
   , unitDefaultEnv
   , lookupDefaultEnv
   , filterDefaultEnv
   , defaultList
   , plusDefaultEnv
   , mkDefaultEnv
   , insertDefaultEnv
   , isHaskell2010Default
   )
where

import GHC.Core.Class (Class (className))
import GHC.Prelude
import GHC.Hs.Extension (GhcRn)
import GHC.Tc.Utils.TcType (Type)
import GHC.Types.Name (Name, nameUnique, stableNameCmp)
import GHC.Types.Name.Env
import GHC.Types.Unique.FM (lookupUFM_Directly)
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Unit.Module.Warnings (WarningTxt)
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable

import Data.Data (Data)
import Data.List (sortBy)
import Data.Function (on)

-- See Note [Named default declarations] in GHC.Tc.Gen.Default

-- | Default environment mapping class name @Name@ to their default type lists
--
-- NB: this includes Haskell98 default declarations, at the 'Num' key.
type DefaultEnv = NameEnv ClassDefaults

{- Note [DefaultProvenance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each `ClassDefault` is annotated with its `DefaultProvenance`, which
says where the default came from.  Specifically
* `DP_Local loc h98`: the default came from an explicit `default` declaration in the module
   being compiled, at location `loc`, and the boolean `h98` indicates whether
   it was from a Haskell 98 default declaration (e.g. `default (Int, Double)`).
* `DP_Imported M`: the default was imported, it is explicitly exported by module `M`.
* `DP_Builtin`:  the default was automatically provided by GHC.
   see Note [Builtin class defaults] in GHC.Tc.Utils.Env

These annotations are used to disambiguate multiple defaults for the same class.
For example, consider the following modules:

  module M( default C ) where { default C( ... ) }
  module M2( default C) where { import M }
  module N( default C () where { default C(... ) }

  module A where { import M2 }
  module B where { import M2; import N }
  module A1 where { import N; default C ( ... ) }
  module B2 where { default C ( ... ); default C ( ... ) }

When compiling N, the default for C is annotated with DP_Local loc.
When compiling M2, the default for C is annotated with DP_Local M.
When compiling A, the default for C is annotated with DP_Imported M2.

Cases we needed to disambiguate:
  * Compiling B, two defaults for C: DP_Imported M2, DP_Imported N.
  * Compiling A1, two defaults for C: DP_Imported N, DP_Local loc.
  * Compiling B2, two defaults for C: DP_Local loc1, DP_Local loc2.

For how we disambiguate these cases,
See Note [Disambiguation of multiple default declarations] in GHC.Tc.Module.
-}

-- | The provenance of a collection of default types for a class.
-- see Note [DefaultProvenance] for more details
data DefaultProvenance
  -- | A locally defined default declaration.
  = DP_Local
     { defaultDeclLoc :: SrcSpan -- ^ The 'SrcSpan' of the default declaration
     , defaultDeclH98 :: Bool    -- ^ Is this a Haskell 98 default declaration?
     }
  -- | Built-in class defaults.
  | DP_Builtin
  -- | Imported class defaults.
  | DP_Imported Module -- ^ The module from which the defaults were imported
  deriving (Eq, Data)

instance Outputable DefaultProvenance where
  ppr (DP_Local loc h98) = ppr loc <> (if h98 then text " (H98)" else empty)
  ppr DP_Builtin         = text "built-in"
  ppr (DP_Imported mod)  = ppr mod

isHaskell2010Default :: DefaultProvenance -> Bool
isHaskell2010Default = \case
  DP_Local { defaultDeclH98 = isH98 } -> isH98
  DP_Builtin -> True
  DP_Imported {} -> False

-- | Defaulting type assignments for the given class.
data ClassDefaults
  = ClassDefaults { cd_class   :: Class -- ^ The class whose defaults are being defined
                  , cd_types   :: [Type]
                  , cd_provenance :: DefaultProvenance
                    -- ^ Where the defaults came from
                    -- see Note [Default exports] in GHC.Tc.Gen.Export
                  , cd_warn    :: Maybe (WarningTxt GhcRn)
                    -- ^ Warning emitted when the default is used
                  }
  deriving Data

instance Outputable ClassDefaults where
  ppr ClassDefaults {cd_class = cls, cd_types = tys} = text "default" <+> ppr cls
        <+> parens (interpp'SP tys)

emptyDefaultEnv :: DefaultEnv
emptyDefaultEnv = emptyNameEnv

isEmptyDefaultEnv :: DefaultEnv -> Bool
isEmptyDefaultEnv = isEmptyNameEnv

unitDefaultEnv :: ClassDefaults -> DefaultEnv
unitDefaultEnv d = unitNameEnv (className $ cd_class d) d

defaultEnv :: [ClassDefaults] -> DefaultEnv
defaultEnv = mkNameEnvWith (className . cd_class)

defaultList :: DefaultEnv -> [ClassDefaults]
defaultList = sortBy (stableNameCmp `on` className . cd_class) . nonDetNameEnvElts
              -- sortBy recovers determinism

insertDefaultEnv :: ClassDefaults -> DefaultEnv -> DefaultEnv
insertDefaultEnv d env = extendNameEnv env (className $ cd_class d) d

lookupDefaultEnv :: DefaultEnv -> Name -> Maybe ClassDefaults
lookupDefaultEnv env = lookupUFM_Directly env . nameUnique

filterDefaultEnv :: (ClassDefaults -> Bool) -> DefaultEnv -> DefaultEnv
filterDefaultEnv = filterNameEnv

plusDefaultEnv :: DefaultEnv -> DefaultEnv -> DefaultEnv
plusDefaultEnv = plusNameEnv

-- | Create a 'DefaultEnv' from a list of (Name, ClassDefaults) pairs
-- it is useful if we don't want to poke into the 'ClassDefaults' structure
-- to get the 'Name' of the class, it can be problematic, see #25858
mkDefaultEnv :: [(Name, ClassDefaults)] -> DefaultEnv
mkDefaultEnv = mkNameEnv

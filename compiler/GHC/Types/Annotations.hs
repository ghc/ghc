-- |
-- Support for source code annotation feature of GHC. That is the ANN pragma.
--
-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
--
{-# LANGUAGE DeriveFunctor #-}
module GHC.Types.Annotations (
        -- * Main Annotation data types
        Annotation(..), AnnPayload,
        AnnTarget(..), CoreAnnTarget,

        -- * AnnEnv for collecting and querying Annotations
        AnnEnv,
        mkAnnEnv, extendAnnEnvList, plusAnnEnv, emptyAnnEnv,
        findAnns, findAnnsByTypeRep,
        deserializeAnns
    ) where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Unit.Module ( Module )
import GHC.Unit.Module.Env
import GHC.Types.Name.Env
import GHC.Types.Name
import GHC.Utils.Outputable
import GHC.Serialized

import Control.Monad
import Data.Maybe
import Data.Typeable
import Data.Word        ( Word8 )


-- | Represents an annotation after it has been sufficiently desugared from
-- it's initial form of 'GHC.Hs.Decls.AnnDecl'
data Annotation = Annotation {
        ann_target :: CoreAnnTarget,    -- ^ The target of the annotation
        ann_value  :: AnnPayload
    }

type AnnPayload = Serialized    -- ^ The "payload" of an annotation
                                --   allows recovery of its value at a given type,
                                --   and can be persisted to an interface file

-- | An annotation target
data AnnTarget name
  = NamedTarget name          -- ^ We are annotating something with a name:
                              --      a type or identifier
  | ModuleTarget Module       -- ^ We are annotating a particular module
  deriving (Functor)

-- | The kind of annotation target found in the middle end of the compiler
type CoreAnnTarget = AnnTarget Name

instance Outputable name => Outputable (AnnTarget name) where
    ppr (NamedTarget nm) = text "Named target" <+> ppr nm
    ppr (ModuleTarget mod) = text "Module target" <+> ppr mod

instance Binary name => Binary (AnnTarget name) where
    put_ bh (NamedTarget a) = do
        putByte bh 0
        put_ bh a
    put_ bh (ModuleTarget a) = do
        putByte bh 1
        put_ bh a
    get bh = do
        h <- getByte bh
        case h of
            0 -> liftM NamedTarget  $ get bh
            _ -> liftM ModuleTarget $ get bh

instance Outputable Annotation where
    ppr ann = ppr (ann_target ann)

-- | A collection of annotations
data AnnEnv = MkAnnEnv { ann_mod_env :: !(ModuleEnv [AnnPayload])
                       , ann_name_env :: !(NameEnv [AnnPayload])
                       }

-- | An empty annotation environment.
emptyAnnEnv :: AnnEnv
emptyAnnEnv = MkAnnEnv emptyModuleEnv emptyNameEnv

-- | Construct a new annotation environment that contains the list of
-- annotations provided.
mkAnnEnv :: [Annotation] -> AnnEnv
mkAnnEnv = extendAnnEnvList emptyAnnEnv

-- | Add the given annotation to the environment.
extendAnnEnvList :: AnnEnv -> [Annotation] -> AnnEnv
extendAnnEnvList env =
  foldl' extendAnnEnv env

extendAnnEnv :: AnnEnv -> Annotation -> AnnEnv
extendAnnEnv (MkAnnEnv mod_env name_env) (Annotation tgt payload) =
  case tgt of
    NamedTarget name -> MkAnnEnv mod_env (extendNameEnv_C (++) name_env name [payload])
    ModuleTarget mod -> MkAnnEnv (extendModuleEnvWith (++) mod_env mod [payload]) name_env

-- | Union two annotation environments.
plusAnnEnv :: AnnEnv -> AnnEnv -> AnnEnv
plusAnnEnv a b =
  MkAnnEnv { ann_mod_env = plusModuleEnv_C (++) (ann_mod_env a) (ann_mod_env b)
           , ann_name_env = plusNameEnv_C (++) (ann_name_env a) (ann_name_env b)
           }

-- | Find the annotations attached to the given target as 'Typeable'
--   values of your choice. If no deserializer is specified,
--   only transient annotations will be returned.
findAnns :: Typeable a => ([Word8] -> a) -> AnnEnv -> CoreAnnTarget -> [a]
findAnns deserialize env
  = mapMaybe (fromSerialized deserialize) . findAnnPayloads env

-- | Find the annotations attached to the given target as 'Typeable'
--   values of your choice. If no deserializer is specified,
--   only transient annotations will be returned.
findAnnsByTypeRep :: AnnEnv -> CoreAnnTarget -> TypeRep -> [[Word8]]
findAnnsByTypeRep env target tyrep
  = [ ws | Serialized tyrep' ws <- findAnnPayloads env target
    , tyrep' == tyrep ]

-- | Find payloads for the given 'CoreAnnTarget' in an 'AnnEnv'.
findAnnPayloads :: AnnEnv -> CoreAnnTarget -> [AnnPayload]
findAnnPayloads env target =
  case target of
    ModuleTarget mod -> lookupWithDefaultModuleEnv (ann_mod_env env) [] mod
    NamedTarget name -> fromMaybe [] $ lookupNameEnv (ann_name_env env) name

-- | Deserialize all annotations of a given type. This happens lazily, that is
--   no deserialization will take place until the [a] is actually demanded and
--   the [a] can also be empty (the UniqFM is not filtered).
deserializeAnns :: Typeable a => ([Word8] -> a) -> AnnEnv -> (ModuleEnv [a], NameEnv [a])
deserializeAnns deserialize env
  = ( mapModuleEnv deserAnns (ann_mod_env env)
    , mapNameEnv deserAnns (ann_name_env env)
    )
  where deserAnns = mapMaybe (fromSerialized deserialize)


-- |
-- Support for source code annotation feature of GHC. That is the ANN pragma.
--
-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
--
module Annotations (
        -- * Main Annotation data types
        Annotation(..), AnnPayload,
        AnnTarget(..), CoreAnnTarget,
        getAnnTargetName_maybe,

        -- * AnnEnv for collecting and querying Annotations
        AnnEnv,
        mkAnnEnv, extendAnnEnvList, plusAnnEnv, emptyAnnEnv, findAnns,
        deserializeAnns
    ) where

import Binary
import Module           ( Module )
import Name
import Outputable
import Serialized
import UniqFM
import Unique

import Control.Monad
import Data.Maybe
import Data.Typeable
import Data.Word        ( Word8 )


-- | Represents an annotation after it has been sufficiently desugared from
-- it's initial form of 'HsDecls.AnnDecl'
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

-- | The kind of annotation target found in the middle end of the compiler
type CoreAnnTarget = AnnTarget Name

instance Functor AnnTarget where
    fmap f (NamedTarget nm) = NamedTarget (f nm)
    fmap _ (ModuleTarget mod) = ModuleTarget mod

-- | Get the 'name' of an annotation target if it exists.
getAnnTargetName_maybe :: AnnTarget name -> Maybe name
getAnnTargetName_maybe (NamedTarget nm) = Just nm
getAnnTargetName_maybe _                = Nothing

instance Uniquable name => Uniquable (AnnTarget name) where
    getUnique (NamedTarget nm) = getUnique nm
    getUnique (ModuleTarget mod) = deriveUnique (getUnique mod) 0
    -- deriveUnique prevents OccName uniques clashing with NamedTarget

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
-- Can't use a type synonym or we hit bug #2412 due to source import
newtype AnnEnv = MkAnnEnv (UniqFM [AnnPayload])

-- | An empty annotation environment.
emptyAnnEnv :: AnnEnv
emptyAnnEnv = MkAnnEnv emptyUFM

-- | Construct a new annotation environment that contains the list of
-- annotations provided.
mkAnnEnv :: [Annotation] -> AnnEnv
mkAnnEnv = extendAnnEnvList emptyAnnEnv

-- | Add the given annotation to the environment.
extendAnnEnvList :: AnnEnv -> [Annotation] -> AnnEnv
extendAnnEnvList (MkAnnEnv env) anns 
  = MkAnnEnv $ addListToUFM_C (++) env $
    map (\ann -> (getUnique (ann_target ann), [ann_value ann])) anns

-- | Union two annotation environments.
plusAnnEnv :: AnnEnv -> AnnEnv -> AnnEnv
plusAnnEnv (MkAnnEnv env1) (MkAnnEnv env2) = MkAnnEnv $ plusUFM_C (++) env1 env2

-- | Find the annotations attached to the given target as 'Typeable' 
--   values of your choice. If no deserializer is specified, 
--   only transient annotations will be returned.
findAnns :: Typeable a => ([Word8] -> a) -> AnnEnv -> CoreAnnTarget -> [a]
findAnns deserialize (MkAnnEnv ann_env) 
  = (mapMaybe (fromSerialized deserialize))
    . (lookupWithDefaultUFM ann_env [])

-- | Deserialize all annotations of a given type. This happens lazily, that is
--   no deserialization will take place until the [a] is actually demanded and
--   the [a] can also be empty (the UniqFM is not filtered).
deserializeAnns :: Typeable a => ([Word8] -> a) -> AnnEnv -> UniqFM [a]
deserializeAnns deserialize (MkAnnEnv ann_env)
  = mapUFM (mapMaybe (fromSerialized deserialize)) ann_env


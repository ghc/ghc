{-
(c) The AQUA Project, Glasgow University, 1993-1998

-}

-- | Dealing with annotations
module GHC.Driver.Core.Opt.Annotations (
    getAnnotationsFromHscEnv, getFirstAnnotationsFromHscEnv,
  ) where

import GHC.Prelude

import GHC.Driver.Env ( HscEnv, prepareAnnotations )

import GHC.Types.Annotations
import GHC.Types.Name.Env

import GHC.Unit.Module
import GHC.Unit.Module.ModGuts

import Data.Bifunctor ( bimap )
import Data.Typeable ( Typeable )
import Data.Word ( Word8 )

-- | Get all annotations of a given type. This happens lazily, that is
-- no deserialization will take place until the [a] is actually demanded and
-- the [a] can also be empty (the UniqFM is not filtered).
--
-- This should be done once at the start of a Core-to-Core pass that uses
-- annotations.
--
-- See Note [Annotations]
getAnnotationsFromHscEnv :: Typeable a => HscEnv -> ([Word8] -> a) -> ModGuts -> IO (ModuleEnv [a], NameEnv [a])
getAnnotationsFromHscEnv hsc_env deserialize guts = do
   ann_env <- prepareAnnotations hsc_env (Just guts)
   return (deserializeAnns deserialize ann_env)

-- | Get at most one annotation of a given type per annotatable item.
getFirstAnnotationsFromHscEnv :: Typeable a => HscEnv -> ([Word8] -> a) -> ModGuts -> IO (ModuleEnv a, NameEnv a)
getFirstAnnotationsFromHscEnv hsc_env deserialize guts
  = bimap mod name <$> getAnnotationsFromHscEnv hsc_env deserialize guts
  where
    mod = mapModuleEnv head . filterModuleEnv (const $ not . null)
    name = mapNameEnv head . filterNameEnv (not . null)

{-
Note [Annotations]
~~~~~~~~~~~~~~~~~~
A Core-to-Core pass that wants to make use of annotations calls
getAnnotationsFromHscEnv or getFirstAnnotationsFromHscEnv at the beginning to
obtain a UniqFM with annotations of a specific type. This produces all
annotations from interface files read so far. However, annotations from
interface files read during the pass will not be visible until
getAnnotationsFromHscEnv is called again. This is similar to how rules work and
probably isn't too bad.

The current implementation could be optimised a bit: when looking up
annotations for a thing from the HomePackageTable, we could search directly in
the module where the thing is defined rather than building one UniqFM which
contains all annotations we know of. This would work because annotations can
only be given to things defined in the same module. However, since we would
only want to deserialise every annotation once, we would have to build a cache
for every module in the HTP. In the end, it's probably not worth it as long as
we aren't using annotations heavily.
-}

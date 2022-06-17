{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Shims
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- A small DSL to handle Shim files in the JS backend, mostly used in
-- 'GHC.StgToJS.Linker.Linker'
--
-----------------------------------------------------------------------------
module GHC.StgToJS.Linker.Shims
  ( Shim()
  , withShim
  , parseShim
  , readShimFiles
  , tryReadShimFile
  , streamShims
  ) where

import           GHC.StgToJS.Linker.Utils

import           System.FilePath
import           GHC.Driver.Session
import           GHC.Driver.Pipeline.Execute (doCpp)

import           GHC.Unit.Env
import           GHC.Utils.TmpFs
import           GHC.Utils.Logger
import           GHC.Utils.Panic

import           Data.Foldable (toList)
import qualified Data.ByteString as B
import           Data.Set  (Set)
import qualified Data.Set  as Set

import           Prelude

-- | A sum type to represent shims. Its sole purpose is to guarentee the
-- ordering of shims during link time. This makes it much harder to compile the
-- js-backend if the link ordering of shims is wrong. Each data constructor
-- represents a single shim file located in either @JS_RTS_PATH@ or
-- @JS_BASE_PATH@. See 'link'' for the call site.
--
-- ** Invariant: The order of data constructors in 'Shim'' determines the shim
--    link time ordering and there is a one-to-one correspondance between each
--    shim file and each data constructor
type Shim = Shim' B.ByteString

newtype Shim' a = Shim' { unShim :: (ShimLbl, a) }
    deriving newtype Functor

-- | projections from Shims; project the shim label tag
-- shimLabel :: Shim -> ShimLbl
-- shimLabel = fst . unShim

-- | projections from Shims, project the payload
shimPayload :: Shim' a -> a
shimPayload = snd . unShim

-- | Take a shim and modify the payload, note that the shim label cannot change
-- as this might invalidate the shim invariant
withShim :: Shim' a -> (a -> IO b) -> IO (Shim' b)
withShim (Shim' (lbl, payload)) f = Shim' . (lbl,) <$> f payload


instance Eq (Shim' a) where
  (Shim' (l,_)) == (Shim' (r,_)) = l == r

instance Ord (Shim' a) where
  (Shim' (l,_)) `compare` (Shim' (r,_)) = l `compare` r

-- | A tag to label shim payloads, the ordering dictates the ordering shim files
-- are linked.
data ShimLbl = ShStructs
             | ShProfiling
             | ShRts
             | ShGc
             | ShArith
             | ShCompact
             | ShDebug
             | ShEnum
             | ShEnvironment
             | ShErrno
             | ShGoog
             | ShHsCore
             | ShMd5
             | ShMem
             | ShNodeExports
             | ShObject
             | ShPlatform
             | ShStablePtr
             | ShStaticPtr
             | ShStm
             | ShString
             | ShThread
             | ShUnicode
             | ShVerify
             | ShWeak
             | ShBase
             deriving (Eq, Ord)

-- | Given a file path, check that the file is a shim file and construct a Shim
-- value if so. This is the sole exported constructor for a Shim type.
parseShim :: FilePath -> IO Shim
parseShim f = let shimFn = takeFileName f
              in case shimFn of
                   "rts.js.pp"           ->  (Shim' . (ShRts,))          <$> B.readFile f
                   "gc.js.pp"            ->  (Shim' . (ShGc,))           <$> B.readFile f
                   "arith.js.pp"         ->  (Shim' . (ShArith,))        <$> B.readFile f
                   "compact.js.pp"       ->  (Shim' . (ShCompact,))      <$> B.readFile f
                   "debug.js.pp"         ->  (Shim' . (ShDebug,))        <$> B.readFile f
                   "enum.js.pp"          ->  (Shim' . (ShEnum,))         <$> B.readFile f
                   "environment.js.pp"   ->  (Shim' . (ShEnvironment,))  <$> B.readFile f
                   "errno.js.pp"         ->  (Shim' . (ShErrno,))        <$> B.readFile f
                   "goog.js"             ->  (Shim' . (ShGoog,))         <$> B.readFile f
                   "hscore.js.pp"        ->  (Shim' . (ShHsCore,))       <$> B.readFile f
                   "md5.js"              ->  (Shim' . (ShMd5,))          <$> B.readFile f
                   "mem.js.pp"           ->  (Shim' . (ShMem,))          <$> B.readFile f
                   "node-exports.js"     ->  (Shim' . (ShNodeExports,))  <$> B.readFile f
                   "object.js.pp"        ->  (Shim' . (ShObject,))       <$> B.readFile f
                   "platform.js.pp"      ->  (Shim' . (ShPlatform,))     <$> B.readFile f
                   "profiling.js.pp"     ->  (Shim' . (ShProfiling,))    <$> B.readFile f
                   "stableptr.js.pp"     ->  (Shim' . (ShStablePtr,))    <$> B.readFile f
                   "staticpointer.js.pp" ->  (Shim' . (ShStaticPtr,))    <$> B.readFile f
                   "stm.js.pp"           ->  (Shim' . (ShStm,))          <$> B.readFile f
                   "string.js.pp"        ->  (Shim' . (ShString,))       <$> B.readFile f
                   "structs.js.pp"       ->  (Shim' . (ShStructs,))      <$> B.readFile f
                   "thread.js.pp"        ->  (Shim' . (ShThread,))       <$> B.readFile f
                   "unicode.js"          ->  (Shim' . (ShUnicode,))      <$> B.readFile f
                   "verify.js.pp"        ->  (Shim' . (ShVerify,))       <$> B.readFile f
                   "weak.js.pp"          ->  (Shim' . (ShWeak,))         <$> B.readFile f
                   "base.js.pp"          ->  (Shim' . (ShBase,))         <$> B.readFile f
                   other                 -> panic $
                                            "parseShim: unrecognized shim file: " ++ show other

-- | Convert any Foldable thing that holds shims into a stream of shim payloads.
-- This function frequently used in concert with 'Data.ByteString.fromChunks' in
-- 'GHC.StgToJS.Linker.Linker'
streamShims :: Foldable f => f Shim -> [B.ByteString]
streamShims = fmap shimPayload . toList

-- | read a sequence of possible shim files into a Set of shims. The set is
-- purposefully chosen to ensure a stable and order preserving container.
readShimFiles :: Logger -> TmpFs -> DynFlags -> UnitEnv -> [FilePath] -> IO (Set Shim)
readShimFiles logger tmpfs dflags unit_env =
  fmap Set.fromList . mapM (tryReadShimFile logger tmpfs dflags unit_env)

-- | Attempt to read a shim file. This function may panic if the shim file
-- cannot be parsed to an expected shim file as defined by the 'Shim' type.
tryReadShimFile :: Logger -> TmpFs -> DynFlags -> UnitEnv -> FilePath -> IO Shim
tryReadShimFile logger tmpfs dflags unit_env file = do
  if needsCpp file
  then do
    infile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "jsppx"
    -- FIXME (Sylvain 2022-06): we should get profiling from the codegen options
    -- (was GHCJS_PROF CPP define)
    let profiling = False
        use_cpp_and_not_cc_dash_E = False
        extra_opts = []
    -- append common CPP definitions to the .pp file.
    B.writeFile infile (commonCppDefs profiling)
    shim <- parseShim file

    withShim shim $ \payload ->
        do B.appendFile infile payload
           outfile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "jspp"
           doCpp logger tmpfs dflags unit_env use_cpp_and_not_cc_dash_E extra_opts infile outfile
           B.readFile outfile
  else parseShim file

-- readShimsArchive :: DynFlags -> FilePath -> IO B.ByteString
-- readShimsArchive = panic "readShimsArchive: Shims not yet implemented!"

-- | Test if file has ".pp" extension
--
-- running the C preprocessor on JS files is a bit fragile
-- and breaks in some situations. Therefore we only preprocess
-- files with .pp extension, for example lib.js.pp
needsCpp :: FilePath -> Bool
needsCpp file = "pp" `isExtensionOf` file

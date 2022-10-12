{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- for Ident's Binary instance

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Types
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Types where

import GHC.JS.Syntax
import GHC.StgToJS.Object
import GHC.StgToJS.Types (ClosureInfo, StaticInfo)

import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Types.Unique.Map
import GHC.Utils.Outputable (hsep,Outputable(..),text,ppr)

import Control.Monad

import Data.Array
import Data.ByteString      (ByteString)
import Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M
import Data.Set             (Set)

import Control.Concurrent.MVar

import System.IO

import Prelude

-- | Return a list of fresh local @Ident@
--
-- Prefix them with 'h$$' such that these will be compacted by the compactor.
newLocals :: [Ident]
newLocals = mkIdents 0
  where
    mkIdent s  = TxtI (mkFastString ("h$$" <> s))
    mkIdents n = [mkIdent (c0:cs) | c0 <- chars, cs <- replicateM n chars] ++ mkIdents (n+1)
    chars      = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

--------------------------------------------------------------------------------
-- CompactorState
--------------------------------------------------------------------------------

data CompactorState = CompactorState
  { csIdentSupply   :: ![Ident]                    -- ^ ident supply for new names
  , csNameMap       :: !(UniqMap FastString Ident) -- ^ renaming mapping for internal names
  , csEntries       :: !(UniqMap FastString Int)   -- ^ entry functions (these get listed in the metadata init
                                                   -- array)
  , csNumEntries    :: !Int
  , csStatics       :: !(UniqMap FastString Int)   -- ^ mapping of global closure -> index in current block,
                                                   -- for static initialisation
  , csNumStatics    :: !Int                        -- ^ number of static entries
  , csLabels        :: !(UniqMap FastString Int)   -- ^ non-Haskell JS labels
  , csNumLabels     :: !Int                        -- ^ number of labels
  , csParentEntries :: !(UniqMap FastString Int)   -- ^ entry functions we're not linking, offset where parent
                                                   -- gets [0..n], grandparent [n+1..k] etc
  , csParentStatics :: !(UniqMap FastString Int)   -- ^ objects we're not linking in base bundle
  , csParentLabels  :: !(UniqMap FastString Int)   -- ^ non-Haskell JS labels in parent
  , csStringTable   :: !StringTable
  }

-- | A Table of Strings representing @Ident@s and their payloads in
-- @CompactorState@
data StringTable = StringTable
  { stTableIdents :: !(Array Int FastString)                -- ^ An array of table identifiers, used in the compactor
  , stOffsets     :: !(M.Map ByteString (Int, Int))         -- ^ content of the table
  , stIdents      :: !(UniqMap FastString (Either Int Int)) -- ^ identifiers in the table
  }

-- | The empty @CompactorState@
emptyCompactorState :: CompactorState
emptyCompactorState = CompactorState newLocals
                                     mempty
                                     mempty
                                     0
                                     mempty
                                     0
                                     mempty
                                     0
                                     mempty
                                     mempty
                                     mempty
                                     emptyStringTable

-- | The empty @StringTable@
emptyStringTable :: StringTable
emptyStringTable = StringTable (listArray (0,-1) []) M.empty emptyUniqMap


--------------------------------------------------------------------------------
-- CompactorState helper functors
--------------------------------------------------------------------------------

-- | Update @csEntries@ in @CompactorState@
entries :: Functor f
        => (UniqMap FastString Int -> f (UniqMap FastString Int))
        -> CompactorState
        -> f CompactorState
entries f cs = fmap (\x -> cs { csEntries = x }) (f $ csEntries cs)
{-# INLINE entries #-}

-- | Update @csLabels@ in @CompactorState@
labels :: Functor f
       => (UniqMap FastString Int -> f (UniqMap FastString Int))
       -> CompactorState
       -> f CompactorState
labels f cs = fmap (\x -> cs { csLabels = x }) (f $ csLabels cs)
{-# INLINE labels #-}

-- | Update @csNameMap@ in @CompactorState@
nameMap :: Functor f
        => (UniqMap FastString Ident -> f (UniqMap FastString Ident))
        -> CompactorState
        -> f CompactorState
nameMap f cs = fmap (\x -> cs { csNameMap = x }) (f $ csNameMap cs)
{-# INLINE nameMap #-}

-- | Update @csNumEntries@ in @CompactorState@
numEntries :: Functor f
           => (Int -> f Int)
           -> CompactorState
           -> f CompactorState
numEntries f cs = fmap (\x -> cs { csNumEntries = x }) (f $ csNumEntries cs)
{-# INLINE numEntries #-}

-- | Update @csNumLabels@ in @CompactorState@
numLabels :: Functor f
          => (Int -> f Int)
          -> CompactorState
          -> f CompactorState
numLabels f cs = fmap (\x -> cs { csNumLabels = x }) (f $ csNumLabels cs)
{-# INLINE numLabels #-}

-- | Update @csNumStatics@ in @CompactorState@
numStatics :: Functor f
           => (Int -> f Int)
           -> CompactorState
           -> f CompactorState
numStatics f cs = fmap (\x -> cs { csNumStatics = x }) (f $ csNumStatics cs)
{-# INLINE numStatics #-}

-- | Update @csParentEntries@ in @CompactorState@
parentEntries :: Functor f
              => (UniqMap FastString Int -> f (UniqMap FastString Int))
              -> CompactorState
              -> f CompactorState
parentEntries f cs = fmap (\x -> cs { csParentEntries = x }) (f $ csParentEntries cs)
{-# INLINE parentEntries #-}

-- | Update @csParentLabels@ in @CompactorState@
parentLabels :: Functor f
             => (UniqMap FastString Int -> f (UniqMap FastString Int))
             -> CompactorState
             -> f CompactorState
parentLabels f cs = fmap (\x -> cs { csParentLabels = x }) (f $ csParentLabels cs)
{-# INLINE parentLabels #-}

-- | Update @csParentStatics@ in @CompactorState@
parentStatics :: Functor f
              => (UniqMap FastString Int -> f (UniqMap FastString Int))
              -> CompactorState
              -> f CompactorState
parentStatics f cs = fmap (\x -> cs { csParentStatics = x }) (f $ csParentStatics cs)
{-# INLINE parentStatics #-}

-- | Update @csStatics@ in @CompactorState@
statics :: Functor f
        => (UniqMap FastString Int -> f (UniqMap FastString Int))
        -> CompactorState
        -> f CompactorState
statics f cs = fmap (\x -> cs { csStatics = x }) (f $ csStatics cs)
{-# INLINE statics #-}

-- | Update @csStringTable@ in @CompactorState@
stringTable :: Functor f
            => (StringTable -> f StringTable)
            -> CompactorState
            -> f CompactorState
stringTable f cs = fmap (\x -> cs { csStringTable = x }) (f $ csStringTable cs)
{-# INLINE stringTable #-}


--------------------------------------------------------------------------------
-- CompactorState Insertions
--------------------------------------------------------------------------------

-- | Given a static entry, add the entry to @CompactorState@
addStaticEntry :: FastString        -- ^ The static entry to add
               -> CompactorState    -- ^ the old state
               -> CompactorState    -- ^ the new state
addStaticEntry new cs =
  -- check if we have seen new before
  let cur_statics = csStatics cs
      go          = lookupUniqMap cur_statics new >> lookupUniqMap (csParentStatics cs) new
  in case go of
    Just _  -> cs                      -- we have so return
    Nothing -> let cnt = csNumStatics cs -- we haven't so do the business
                   newStatics = addToUniqMap cur_statics new cnt
                   newCnt = cnt + 1
               in cs {csStatics = newStatics, csNumStatics = newCnt}

-- | Given an entry function, add the entry function to @CompactorState@
addEntry :: FastString        -- ^ The entry function to add
         -> CompactorState    -- ^ the old state
         -> CompactorState    -- ^ the new state
addEntry new cs =
  let cur_entries = csEntries cs
      go          = lookupUniqMap cur_entries new >> lookupUniqMap (csParentEntries cs) new
  in case go of
    Just _  -> cs
    Nothing -> let cnt = csNumEntries cs
                   newEntries = addToUniqMap cur_entries new cnt
                   newCnt = cnt + 1
               in cs {csEntries = newEntries, csNumEntries = newCnt}

-- | Given a label, add the label to @CompactorState@
addLabel :: FastString        -- ^ The label to add
         -> CompactorState    -- ^ the old state
         -> CompactorState    -- ^ the new state
addLabel new cs =
  let cur_lbls = csLabels cs
      go       = lookupUniqMap cur_lbls new >> lookupUniqMap (csParentLabels cs) new
  in case go of
    Just _  -> cs
    Nothing -> let cnt = csNumLabels cs
                   newLabels = addToUniqMap cur_lbls new cnt
                   newCnt = cnt + 1
               in cs {csEntries = newLabels, csNumLabels = newCnt}

--------------------------------------------------------------------------------
-- Linker Config
--------------------------------------------------------------------------------

data JSLinkConfig =
  JSLinkConfig { lcNativeExecutables  :: Bool
               , lcNativeToo          :: Bool
               , lcBuildRunner        :: Bool
               , lcNoJSExecutables    :: Bool
               , lcNoHsMain           :: Bool
               , lcStripProgram       :: Maybe FilePath
               , lcLogCommandLine     :: Maybe FilePath
               , lcGhc                :: Maybe FilePath
               , lcOnlyOut            :: Bool
               , lcNoRts              :: Bool
               , lcNoStats            :: Bool
               , lcLinkJsLib          :: Maybe String
               , lcJsLibOutputDir     :: Maybe FilePath
               , lcJsLibSrcs          :: [FilePath]
               , lcDedupe             :: Bool
               }

-- | we generate a runnable all.js only if we link a complete application,
--   no incremental linking and no skipped parts
generateAllJs :: JSLinkConfig -> Bool
generateAllJs s = not (lcOnlyOut s) && not (lcNoRts s)

instance Monoid JSLinkConfig where
  mempty = JSLinkConfig
            { lcNativeExecutables  = False
            , lcNativeToo          = False
            , lcBuildRunner        = False
            , lcNoJSExecutables    = False
            , lcNoHsMain           = False
            , lcStripProgram       = Nothing
            , lcLogCommandLine     = Nothing
            , lcGhc                = Nothing
            , lcOnlyOut            = False
            , lcNoRts              = False
            , lcNoStats            = False
            , lcLinkJsLib          = Nothing
            , lcJsLibOutputDir     = Nothing
            , lcJsLibSrcs          = mempty
            , lcDedupe             = False
            }

instance Semigroup JSLinkConfig where
  (<>) c1 c2 =
    let comb :: (a -> a -> a) -> (JSLinkConfig -> a) -> a
        comb f a = f (a c1) (a c2)
    in JSLinkConfig
            { lcNativeExecutables  = comb (||) lcNativeExecutables
            , lcNativeToo          = comb (||) lcNativeToo
            , lcBuildRunner        = comb (||) lcBuildRunner
            , lcNoJSExecutables    = comb (||) lcNoJSExecutables
            , lcNoHsMain           = comb (||) lcNoHsMain
            , lcStripProgram       = comb mplus lcStripProgram
            , lcLogCommandLine     = comb mplus lcLogCommandLine
            , lcGhc                = comb mplus lcGhc
            , lcOnlyOut            = comb (||) lcOnlyOut
            , lcNoRts              = comb (||) lcNoRts
            , lcNoStats            = comb (||) lcNoStats
            , lcLinkJsLib          = comb (<>) lcLinkJsLib
            , lcJsLibOutputDir     = comb (<>) lcJsLibOutputDir
            , lcJsLibSrcs          = comb (<>) lcJsLibSrcs
            , lcDedupe             = comb (||) lcDedupe
            }

--------------------------------------------------------------------------------
-- Linker Environment
--------------------------------------------------------------------------------

-- | A @LinkableUnit@ is a pair of a module and the index of the block in the
-- object file
type LinkableUnit = (Module, Int)

-- | A @LinkedUnit@ is a payload of JS code with its closures and any static info.
data LinkedUnit = LinkedUnit
  { lu_js_code  :: !JStat
  , lu_closures :: ![ClosureInfo]
  , lu_statics  :: ![StaticInfo]
  }

-- | An object file that's either already in memory (with name) or on disk
data LinkedObj
  = ObjFile   FilePath      -- ^ load from this file
  | ObjLoaded String Object -- ^ already loaded: description and payload

instance Outputable LinkedObj where
  ppr = \case
    ObjFile fp    -> hsep [text "ObjFile", text fp]
    ObjLoaded s o -> hsep [text "ObjLoaded", text s, ppr (objModuleName o)]

data GhcjsEnv = GhcjsEnv
  { linkerArchiveDeps :: MVar (Map (Set FilePath)
                                   (Map Module (Deps, DepsLocation)
                                   , [LinkableUnit]
                                   )
                              )
  }

-- | return a fresh @GhcjsEnv@
newGhcjsEnv :: IO GhcjsEnv
newGhcjsEnv = GhcjsEnv <$> newMVar M.empty

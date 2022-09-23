{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

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
--  A base bundle is used for incremental linking. it contains information about
--  the symbols that have already been linked. These symbols are not included
--  again in the incrementally linked program.
--
-- The Base data structure contains the information we need to do incremental
-- linking against a base bundle.
--
--  base file format:
--  - GHCJSBASE
--  - [renamer state]
--  - [linkedPackages]
--  - [packages]
--  - [modules]
--  - [symbols]
--
--  The base contains a CompactorState for consistent renaming of private names
--  and packed initialization of info tables and static closures.
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Types where

import           GHC.JS.Syntax
import           GHC.StgToJS.Object
import           GHC.StgToJS.Types (ClosureInfo, StaticInfo)

import           GHC.Unit.Types
import           GHC.Utils.Outputable hiding ((<>))
import           GHC.Data.FastString
import           GHC.Driver.Env.Types (HscEnv)
import           GHC.Types.Error      (Messages)
import           GHC.Types.Unique.Map

import           Control.Monad

import           Data.Array
import           Data.ByteString      (ByteString)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import qualified Data.IntMap          as I

import           Control.Concurrent.MVar
import qualified Control.Exception as E

import           System.IO
import           System.Process

import           Prelude

-- | return a list of fresh @Ident@
newLocals :: [Ident]
newLocals = filter (not . isJsKeyword) $
            map (TxtI . mkFastString) $
            map (:[]) chars0 ++ concatMap mkIdents [1..]
  where
    mkIdents n = [c0:cs | c0 <- chars0, cs <- replicateM n chars]
    chars0 = ['a'..'z']++['A'..'Z']
    chars = chars0++['0'..'9']

-- | Rename @newLocals@ to 'h$$' such that these will be compacted by the
-- compactor.
renamedVars :: [Ident]
renamedVars = map (\(TxtI xs) -> TxtI ("h$$"<>xs)) newLocals


--------------------------------------------------------------------------------
-- CompactorState
--------------------------------------------------------------------------------

data CompactorState = CompactorState
  { csIdentSupply   :: [Ident]                     -- ^ ident supply for new names
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
emptyCompactorState = CompactorState renamedVars
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

-- | Update @csIdentSupply@ in @CompactorState@
identSupply :: Functor f
            => ([Ident] -> f [Ident])
            -> CompactorState
            -> f CompactorState
identSupply f cs = fmap (\x -> cs { csIdentSupply = x }) (f $ csIdentSupply cs)
{-# INLINE identSupply #-}

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
-- Base
--------------------------------------------------------------------------------

-- | The Base bundle. Used for incremental linking it maintains the compactor
-- state the base packages and units.
data Base = Base { baseCompactorState :: CompactorState
                 , basePkgs           :: [UnitId]
                 , baseUnits          :: Set (Module, Int)
                 }

-- | Custom Show for the @Base@ bundle
showBase :: Base -> String
showBase b = unlines
  [ "Base:"
  , "  packages: " ++ showSDocUnsafe (ppr (basePkgs b))
  , "  number of units: " ++ show (S.size $ baseUnits b)
  , "  renaming table size: " ++
    show (sizeUniqMap . csNameMap . baseCompactorState $ b)
  ]

-- | The empty @Base@ bundle
emptyBase :: Base
emptyBase = Base emptyCompactorState [] S.empty

-- | make a @Base@ state from a @CompactorState@: empty the current symbols
--   sets, move everything to the parent
makeCompactorParent :: CompactorState -> CompactorState
makeCompactorParent (CompactorState is nm es nes ss nss ls nls pes pss pls sts)
  = CompactorState is
                   nm
                   emptyUniqMap 0
                   emptyUniqMap 0
                   emptyUniqMap 0
                   (plusUniqMap (fmap (+nes) pes) es)
                   (plusUniqMap (fmap (+nss) pss) ss)
                   (plusUniqMap (fmap (+nls) pls) ls)
                   sts

-- | There are 3 ways the linker can use @Base@. We can not use it, and thus not
-- do any incremental linking. We can load it from a file, where we assume that
-- the symbols from the bundle and their dependencies have already been loaded.
-- In this case We must save the CompactorState so that we can do consistent
-- renaming. Or we can use a Base that is already in memory.
--
-- Incremental linking greatly improves link time and can also be used in
-- multi-page or repl-type applications to serve most of the code from a static
-- location, reloading only the small parts that are actually different.
data UseBase = NoBase             -- ^ don't use incremental linking
             | BaseFile  FilePath -- ^ load base from file
             | BaseState Base     -- ^ use this base

instance Show UseBase where
  show NoBase       = "NoBase"
  show BaseFile {}  = "BaseFile"
  show BaseState {} = "BaseState"

instance Monoid UseBase where
  mempty             = NoBase

instance Semigroup UseBase where
  x <> NoBase = x
  _ <> x      = x


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
               , lcGenBase            :: Maybe Module   -- ^ base module
               , lcUseBase            :: UseBase
               , lcLinkJsLib          :: Maybe String
               , lcJsLibOutputDir     :: Maybe FilePath
               , lcJsLibSrcs          :: [FilePath]
               , lcDedupe             :: Bool
               }

-- | Check if we are using the @Base@ bundle, or not.
usingBase :: JSLinkConfig -> Bool
usingBase s | NoBase <- lcUseBase s = False
            | otherwise             = True

-- | we generate a runnable all.js only if we link a complete application,
--   no incremental linking and no skipped parts
generateAllJs :: JSLinkConfig -> Bool
generateAllJs s
  | NoBase <- lcUseBase s = not (lcOnlyOut s) && not (lcNoRts s)
  | otherwise             = False

instance Monoid JSLinkConfig where
  mempty = JSLinkConfig False   False   False   False False
                        Nothing Nothing Nothing False
                        False   False   Nothing NoBase
                        Nothing Nothing mempty  False

instance Semigroup JSLinkConfig where
  (<>) (JSLinkConfig ne1 nn1 bc1 nj1 noHs1 sp1 lc1 gh1 oo1 nr1 ns1 gb1 ub1 ljsl1 jslo1 jslsrc1 dd1)
       (JSLinkConfig ne2 nn2 bc2 nj2 noHs2 sp2 lc2 gh2 oo2 nr2 ns2 gb2 ub2 ljsl2 jslo2 jslsrc2 dd2) =
          JSLinkConfig (ne1 || ne2)
                        (nn1 || nn2)
                        (bc1 || bc2)
                        (nj1 || nj2)
                        (noHs1 || noHs2)
                        (sp1 `mplus` sp2)
                        (lc1 `mplus` lc2)
                        (gh1 `mplus` gh2)
                        (oo1 || oo2)
                        (nr1 || nr2)
                        (ns1 || ns2)
                        (gb1 `mplus` gb2)
                        (ub1 <> ub2)
                        (ljsl1 <> ljsl2)
                        (jslo1 <> jslo2)
                        (jslsrc1 <> jslsrc2)
                        (dd1 || dd2)


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

data GhcjsEnv = GhcjsEnv
  { compiledModules   :: MVar (Map Module ByteString)  -- ^ keep track of already compiled modules so we don't compile twice for dynamic-too
  , thRunners         :: MVar THRunnerState            -- ^ template haskell runners
  , thSplice          :: MVar Int
  , linkerArchiveDeps :: MVar (Map (Set FilePath)
                                   (Map Module (Deps, DepsLocation)
                                   , [LinkableUnit]
                                   )
                              )
  , pluginState       :: MVar (Maybe HscEnv)
  }

-- | return a fresh @GhcjsEnv@
newGhcjsEnv :: IO GhcjsEnv
newGhcjsEnv = GhcjsEnv <$> newMVar M.empty
                       <*> newMVar emptyTHRunnerState
                       <*> newMVar 0
                       <*> newMVar M.empty
                       <*> newMVar Nothing


--------------------------------------------------------------------------------
-- Template Haskell
--------------------------------------------------------------------------------

data THRunnerState = THRunnerState
  { activeRunners :: Map String THRunner
  , idleRunners   :: [THRunner]
  }

data THRunner =
  THRunner { thrProcess        :: ProcessHandle
           , thrHandleIn       :: Handle
           , thrHandleErr      :: Handle
           , thrBase           :: MVar Base
           , thrRecover        :: MVar [Messages String]
           , thrExceptions     :: MVar (I.IntMap E.SomeException)
           }

emptyTHRunnerState :: THRunnerState
emptyTHRunnerState = THRunnerState mempty mempty


--------------------------------------------------------------------------------
-- Template Haskell helpers
--------------------------------------------------------------------------------

-- | Add an idle runner to the set of @idleRunners@ in @THRunnerState@
consIdleRunner :: THRunner -> THRunnerState -> THRunnerState
consIdleRunner r s = s { idleRunners = r : idleRunners s }

-- | Remove an idle runner from the set of @idleRunners@ in @THRunnerState@
unconsIdleRunner :: THRunnerState -> Maybe (THRunner, THRunnerState)
unconsIdleRunner s
  | (r:xs) <- idleRunners s = Just (r, s { idleRunners = xs })
  | otherwise               = Nothing

-- | Remove an active runner from the set of @activeRunners@ in @THRunnerState@
deleteActiveRunner :: String -> THRunnerState -> THRunnerState
deleteActiveRunner m s =
  s { activeRunners = M.delete m (activeRunners s) }

-- | Add an active runner to the set of @activeRunners@ in @THRunnerState@
insertActiveRunner :: String -> THRunner -> THRunnerState -> THRunnerState
insertActiveRunner m runner s =
  s { activeRunners = M.insert m runner (activeRunners s) }

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
import           GHC.Utils.Panic
import           GHC.Utils.Outputable hiding ((<>))
import           GHC.Data.FastString
import           GHC.Driver.Env.Types (HscEnv)
import           GHC.Types.Error      (Messages)
import           GHC.Types.Unique.Map

import           Control.Monad

import           Data.Array
import qualified Data.Binary          as DB
import qualified Data.Binary.Get      as DB
import qualified Data.Binary.Put      as DB
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.List            (sortOn)
import           Data.Set             (Set)
import qualified Data.Set             as S
import qualified Data.IntMap          as I

import           Control.Concurrent.MVar
import qualified Control.Exception as E

import           System.IO
import           System.Process

import           Prelude

newLocals :: [Ident]
newLocals = filter (not . isJsKeyword) $
            map (TxtI . mkFastString) $
            map (:[]) chars0 ++ concatMap mkIdents [1..]
  where
    mkIdents n = [c0:cs | c0 <- chars0, cs <- replicateM n chars]
    chars0 = ['a'..'z']++['A'..'Z']
    chars = chars0++['0'..'9']

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

data StringTable = StringTable
  { stTableIdents :: !(Array Int FastString)
  , stOffsets     :: !(M.Map ByteString (Int, Int))         -- ^ content of the table
  , stIdents      :: !(UniqMap FastString (Either Int Int)) -- ^ identifiers in the table
  }

instance DB.Binary Ident where
  put (TxtI s) = DB.put $ unpackFS s
  get = TxtI . mkFastString <$> DB.get

instance DB.Binary StringTable where
  put (StringTable tids offs idents) = do
    DB.put tids
    DB.put (M.toList offs)
    -- The lexical sorting allows us to use nonDetEltsUniqMap without introducing non-determinism
    DB.put (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap idents)
  get = StringTable <$> DB.get
                    <*> fmap M.fromList DB.get
                    <*> fmap listToUniqMap DB.get

emptyStringTable :: StringTable
emptyStringTable = StringTable (listArray (0,-1) []) M.empty emptyUniqMap

entries :: Functor f
        => (UniqMap FastString Int -> f (UniqMap FastString Int))
        -> CompactorState
        -> f CompactorState
entries f cs = fmap (\x -> cs { csEntries = x }) (f $ csEntries cs)
{-# INLINE entries #-}

identSupply :: Functor f
            => ([Ident] -> f [Ident])
            -> CompactorState
            -> f CompactorState
identSupply f cs = fmap (\x -> cs { csIdentSupply = x }) (f $ csIdentSupply cs)
{-# INLINE identSupply #-}

labels :: Functor f
       => (UniqMap FastString Int -> f (UniqMap FastString Int))
       -> CompactorState
       -> f CompactorState
labels f cs = fmap (\x -> cs { csLabels = x }) (f $ csLabels cs)
{-# INLINE labels #-}

nameMap :: Functor f
        => (UniqMap FastString Ident -> f (UniqMap FastString Ident))
        -> CompactorState
        -> f CompactorState
nameMap f cs = fmap (\x -> cs { csNameMap = x }) (f $ csNameMap cs)
{-# INLINE nameMap #-}

numEntries :: Functor f
           => (Int -> f Int)
           -> CompactorState
           -> f CompactorState
numEntries f cs = fmap (\x -> cs { csNumEntries = x }) (f $ csNumEntries cs)
{-# INLINE numEntries #-}

numLabels :: Functor f
          => (Int -> f Int)
          -> CompactorState
          -> f CompactorState
numLabels f cs = fmap (\x -> cs { csNumLabels = x }) (f $ csNumLabels cs)
{-# INLINE numLabels #-}

numStatics :: Functor f
           => (Int -> f Int)
           -> CompactorState
           -> f CompactorState
numStatics f cs = fmap (\x -> cs { csNumStatics = x }) (f $ csNumStatics cs)
{-# INLINE numStatics #-}

parentEntries :: Functor f
              => (UniqMap FastString Int -> f (UniqMap FastString Int))
              -> CompactorState
              -> f CompactorState
parentEntries f cs = fmap (\x -> cs { csParentEntries = x }) (f $ csParentEntries cs)
{-# INLINE parentEntries #-}

parentLabels :: Functor f
             => (UniqMap FastString Int -> f (UniqMap FastString Int))
             -> CompactorState
             -> f CompactorState
parentLabels f cs = fmap (\x -> cs { csParentLabels = x }) (f $ csParentLabels cs)
{-# INLINE parentLabels #-}

parentStatics :: Functor f
              => (UniqMap FastString Int -> f (UniqMap FastString Int))
              -> CompactorState
              -> f CompactorState
parentStatics f cs = fmap (\x -> cs { csParentStatics = x }) (f $ csParentStatics cs)
{-# INLINE parentStatics #-}

statics :: Functor f
        => (UniqMap FastString Int -> f (UniqMap FastString Int))
        -> CompactorState
        -> f CompactorState
statics f cs = fmap (\x -> cs { csStatics = x }) (f $ csStatics cs)
{-# INLINE statics #-}

stringTable :: Functor f
            => (StringTable -> f StringTable)
            -> CompactorState
            -> f CompactorState
stringTable f cs = fmap (\x -> cs { csStringTable = x }) (f $ csStringTable cs)
{-# INLINE stringTable #-}

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

-- | make a base state from a CompactorState: empty the current symbols sets,
--   move everything to the parent
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

-- Helper functions used in Linker.Compactor. We live with some redundant code
-- to avoid the lens mayhem in Gen2 GHCJS. TODO: refactor to avoid redundant
-- code
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

instance DB.Binary Base where
  get = getBase "<unknown file>"
  put = putBase

showBase :: Base -> String
showBase b = unlines
  [ "Base:"
  , "  packages: " ++ showSDocUnsafe (ppr (basePkgs b))
  , "  number of units: " ++ show (S.size $ baseUnits b)
  , "  renaming table size: " ++
    show (sizeUniqMap . csNameMap . baseCompactorState $ b)
  ]

emptyBase :: Base
emptyBase = Base emptyCompactorState [] S.empty

putBase :: Base -> DB.Put
putBase (Base cs packages funs) = do
  DB.putByteString "GHCJSBASE"
  DB.putLazyByteString versionTag
  putCs cs
  putList DB.put packages
  -- putList putPkg pkgs
  putList DB.put mods
  putList putFun (S.toList funs)
  where
    pi :: Int -> DB.Put
    pi = DB.putWord32le . fromIntegral
    uniq :: Ord a => [a] -> [a]
    uniq  = S.toList . S.fromList
    -- pkgs  = uniq (map fst $ S.toList funs)
    -- pkgsM = M.fromList (zip pkgs [(0::Int)..])
    mods  = uniq (map fst $ S.toList funs)
    modsM = M.fromList (zip mods [(0::Int)..])
    putList f xs = pi (length xs) >> mapM_ f xs
    -- serialise the compactor state
    putCs (CompactorState [] _ _ _ _ _ _ _ _ _ _ _) =
      panic "putBase: putCs exhausted renamer symbol names"
    putCs (CompactorState (ns:_) nm es _ ss _ ls _ pes pss pls sts) = do
      DB.put ns
      -- We can use nonDetEltsUniqMap without introducing non-determinism by sorting lexically
      DB.put (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap nm)
      DB.put (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap es)
      DB.put (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap ss)
      DB.put (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap ls)
      DB.put (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap pes)
      DB.put (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap pss)
      DB.put (sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap pls)
      DB.put sts
    -- putPkg mod = DB.put mod
    -- fixme group things first
    putFun (m,s) = --pi (pkgsM M.! p) >>
                   pi (modsM M.! m) >> DB.put s

getBase :: FilePath -> DB.Get Base
getBase file = getBase'
  where
    gi :: DB.Get Int
    gi = fromIntegral <$> DB.getWord32le
    getList f = DB.getWord32le >>= \n -> replicateM (fromIntegral n) f
    getFun ms = (,) <$>
                   -- ((ps!) <$> gi) <*>
                   ((ms!) <$> gi) <*> DB.get
    la xs = listArray (0, length xs - 1) xs
    -- getPkg = DB.get
    getCs = do
      n   <- DB.get
      nm  <- listToUniqMap <$> DB.get
      es  <- listToUniqMap <$> DB.get
      ss  <- listToUniqMap <$> DB.get
      ls  <- listToUniqMap <$> DB.get
      pes <- listToUniqMap <$> DB.get
      pss <- listToUniqMap <$> DB.get
      pls <- listToUniqMap <$> DB.get
      CompactorState (dropWhile (/=n) renamedVars)
                             nm
                             es
                             (sizeUniqMap es)
                             ss
                             (sizeUniqMap ss)
                             ls
                             (sizeUniqMap ls)
                             pes
                             pss
                             pls <$> DB.get
    getBase' = do
      hdr <- DB.getByteString 9
      when (hdr /= "GHCJSBASE")
           (panic $ "getBase: invalid base file: " <> file)
      vt  <- DB.getLazyByteString (fromIntegral versionTagLength)
      when (vt /= versionTag)
           (panic $ "getBase: incorrect version: " <> file)
      cs <- makeCompactorParent <$> getCs
      linkedPackages <- getList DB.get
      -- pkgs <- la <$> getList getPkg
      mods <- la <$> getList DB.get
      funs <- getList (getFun mods)
      return (Base cs linkedPackages $ S.fromList funs)

-- | lazily render the base metadata into a bytestring
renderBase :: Base -> BL.ByteString
renderBase = DB.runPut . putBase

-- | lazily load base metadata from a file, see @UseBase@.
loadBase :: FilePath -> IO Base
loadBase file = DB.runGet (getBase file) <$> BL.readFile file

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
-- TODO: Jeff: (2022,03): Move to separate module? Linker.Config? Or Merge with StgToJSConfig?
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
-- | A LinkableUnit is a pair of a module and the index of the block in the
-- object file
type LinkableUnit = (Module, Int)

data LinkedUnit = LinkedUnit
  { lu_js_code  :: !JStat
  , lu_closures :: ![ClosureInfo]
  , lu_statics  :: ![StaticInfo]
  }

-- | An object file that's either already in memory (with name) or on disk
data LinkedObj = ObjFile   FilePath             -- ^ load from this file
               | ObjLoaded String BL.ByteString -- ^ already loaded: description and payload
               deriving (Eq, Ord, Show)

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

newGhcjsEnv :: IO GhcjsEnv
newGhcjsEnv = GhcjsEnv <$> newMVar M.empty
                       <*> newMVar emptyTHRunnerState
                       <*> newMVar 0
                       <*> newMVar M.empty
                       <*> newMVar Nothing

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

consIdleRunner :: THRunner -> THRunnerState -> THRunnerState
consIdleRunner r s = s { idleRunners = r : idleRunners s }

unconsIdleRunner :: THRunnerState -> Maybe (THRunner, THRunnerState)
unconsIdleRunner s
  | (r:xs) <- idleRunners s = Just (r, s { idleRunners = xs })
  | otherwise               = Nothing

deleteActiveRunner :: String -> THRunnerState -> THRunnerState
deleteActiveRunner m s =
  s { activeRunners = M.delete m (activeRunners s) }

insertActiveRunner :: String -> THRunner -> THRunnerState -> THRunnerState
insertActiveRunner m runner s =
  s { activeRunners = M.insert m runner (activeRunners s) }

emptyTHRunnerState :: THRunnerState
emptyTHRunnerState = THRunnerState mempty mempty

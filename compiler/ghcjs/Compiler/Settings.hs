{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Compiler.Settings where

import           Gen2.Base
import qualified Gen2.Object as Object

import qualified Control.Exception as E
import           Control.Concurrent.MVar
import           Control.Monad

import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Data.IntMap            (IntMap)
import qualified Data.List              as L
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import           Data.Text              (Text)
import qualified Data.Text as T
import           Data.Typeable

import           GHC.Generics

import           System.Environment     (getEnvironment)
import           System.IO
import           System.Process
import           System.FilePath        ((</>), searchPathSeparator
                                        ,splitSearchPath)

import           DynFlags
import           Module
import           ErrUtils
import           HscTypes
import           Panic
import Prelude

import qualified Compiler.Info as Info

{- | We can link incrementally against a base bundle, where we assume
     that the symbols from the bundle and their dependencies have already
     been loaded. We need to save the CompactorState so that we can
     do consistent renaming.

     Incremental linking greatly improves link time and can also
     be used in multi-page or repl-type applications to serve
     most of the code from a static location, reloading only the
     small parts that are actually different.
 -}
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

data GhcjsSettings =
  GhcjsSettings { gsNativeExecutables  :: Bool
                , gsNativeToo          :: Bool
                , gsBuildRunner        :: Bool
                , gsNoJSExecutables    :: Bool
                , gsStripProgram       :: Maybe FilePath
                , gsLogCommandLine     :: Maybe FilePath
                , gsGhc                :: Maybe FilePath
                , gsOnlyOut            :: Bool
                , gsNoRts              :: Bool
                , gsNoStats            :: Bool
                , gsGenBase            :: Maybe String   -- ^ module name
                , gsUseBase            :: UseBase
                , gsLinkJsLib          :: Maybe String
                , gsJsLibOutputDir     :: Maybe FilePath
                , gsJsLibSrcs          :: [FilePath]
                , gsDedupe             :: Bool
                } deriving Show

usingBase :: GhcjsSettings -> Bool
usingBase s | NoBase <- gsUseBase s = False
            | otherwise             = True

-- | we generate a runnable all.js only if we link a complete application,
--   no incremental linking and no skipped parts
generateAllJs :: GhcjsSettings -> Bool
generateAllJs s
  | NoBase <- gsUseBase s = not (gsOnlyOut s) && not (gsNoRts s)
  | otherwise             = False

{-
  this instance is supposed to capture overriding settings, where one group
  comes from the environment (env vars, config files) and the other
  from the command line. (env `mappend` cmdLine) should give the combined
  settings, but it doesn't work very well. find something better.
 -}
instance Monoid GhcjsSettings where
  mempty = GhcjsSettings False False False False Nothing Nothing Nothing False False False Nothing NoBase Nothing Nothing [] False

instance Semigroup GhcjsSettings where
  (<>) (GhcjsSettings ne1 nn1 bc1 nj1 sp1 lc1 gh1 oo1 nr1 ns1 gb1 ub1 ljsl1 jslo1 jslsrc1 dd1)
       (GhcjsSettings ne2 nn2 bc2 nj2 sp2 lc2 gh2 oo2 nr2 ns2 gb2 ub2 ljsl2 jslo2 jslsrc2 dd2) =
          GhcjsSettings (ne1 || ne2)
                        (nn1 || nn2)
                        (bc1 || bc2)
                        (nj1 || nj2)
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

data THRunner =
  THRunner { thrProcess        :: ProcessHandle
           , thrHandleIn       :: Handle
           , thrHandleErr      :: Handle
           , thrBase           :: MVar Base
           , thrRecover        :: MVar [Messages]
           , thrExceptions     :: MVar (IntMap E.SomeException)
           }

data DepsLocation = ObjectFile  FilePath
                  | ArchiveFile FilePath
                  | InMemory    String ByteString
                  deriving (Eq, Show)

data THRunnerState = THRunnerState
  { activeRunners :: Map String THRunner
  , idleRunners   :: [THRunner]
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

data GhcjsEnv = GhcjsEnv
  { compiledModules   :: MVar (Map Module ByteString) -- ^ keep track of already compiled modules so we don't compile twice for dynamic-too
  , thRunners         :: MVar THRunnerState -- (Map String ThRunner)   -- ^ template haskell runners
  , thSplice          :: MVar Int
  , linkerArchiveDeps :: MVar (Map (Set FilePath)
                                   (Map (Object.Package, Text)
                                        (Object.Deps, DepsLocation), [(Object.Package, Text, Int)]
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

-- an object file that's either already in memory (with name) or on disk
data LinkedObj = ObjFile   FilePath          -- load from this file
               | ObjLoaded String ByteString -- already loaded, description
               deriving (Eq, Ord, Show)

data NodeSettings = NodeSettings
  { nodeProgram         :: FilePath    -- ^ location of node.js program
  , nodePath            :: Maybe Text  -- ^ value of NODE_PATH environment variable
  , nodeExtraArgs       :: [Text]      -- ^ extra arguments to pass to node.js
  , nodeKeepAliveMaxMem :: Integer     -- ^ keep node.js (TH, GHCJSi) processes alive if they don't use more than this
  } deriving (Eq, Ord, Show, Typeable, Generic)

instance FromJSON NodeSettings
instance ToJSON NodeSettings

readNodeSettings :: DynFlags -> IO NodeSettings
readNodeSettings dflags = do
  contents <- BL.readFile (Info.getLibDir dflags </> "nodeSettings.json")
  either panic pure (eitherDecode' contents)

runNodeInteractive :: DynFlags
                   -> Maybe FilePath
                   -> FilePath
                   -> IO (Handle, Handle, Handle, ProcessHandle)
runNodeInteractive dflags mbWorkingDir src = do
  nodeSettings <- readNodeSettings dflags
  env0 <- getEnvironment
  let ghcjsNodePath = topDir dflags </> "ghcjs-node" </> "node_modules"
      addGhcjsNodePath origNodePath =
        L.intercalate (searchPathSeparator:[])
                      (ghcjsNodePath:splitSearchPath origNodePath)
      nodePath = maybe ghcjsNodePath
                       addGhcjsNodePath
                       (L.lookup "NODE_PATH" env0)
      env1 = [("NODE_PATH", nodePath)] ++ filter ((/="NODE_PATH") . fst) env0
  runInteractiveProcess
    (nodeProgram nodeSettings)
    (map T.unpack (nodeExtraArgs nodeSettings) ++ [src])
    mbWorkingDir
    (Just env1)

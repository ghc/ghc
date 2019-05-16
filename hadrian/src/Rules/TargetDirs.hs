{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{- | Provides utilities for figuring out the right target to build when
     Hadrian is invoked from a particular package or test directory,
     to only rebuild the said package or run the tests from the said
     directory.
-}
module Rules.TargetDirs
  (getTestDir, getDirAction, performDirAction, targetsTrie)
where

import CommandLine (cmdTargetDir)
import Data.List (isPrefixOf, intercalate)
import Data.Map (Map)
import Development.Shake
import Development.Shake.FilePath
import Hadrian.Package
import Oracles.Setting
import Settings (knownPackages)
import Stage
import UserSettings

import qualified Data.Map as Map

data DirAction
  = BuildPackage Package -- ^ Called from a package's path, only
                         --   rebuild that package.
  | RunTestDir FilePath  -- ^ Called from a test directory, only
                         --   run tests from that directory.
  deriving (Eq, Show)

actionTarget :: DirAction -> String
actionTarget (RunTestDir _) = "test"
actionTarget (BuildPackage p) = intercalate ":"
  [stagestr, typ, pkgname]

  where stagestr = stageString finalStage
        typ      = if isLibrary p then "lib" else "exe"
        pkgname  = pkgName p

getDirAction :: FilePath -> Trie FilePath DirAction -> Action (Maybe DirAction)
getDirAction dir trie = do
  top <- topDirectory
  verifyPrefix top dir

  where verifyPrefix top d
          | not (top `isPrefixOf` d) = return Nothing
          | otherwise = return $ targetForPath (makeRelative top d) trie

getTestDir :: Action (Maybe FilePath)
getTestDir = do
  mdir <- cmdTargetDir
  trie <- liftIO targetsTrie
  mact <- maybe (return Nothing) (flip getDirAction trie) mdir
  return $ case mact of
      Just (RunTestDir d) -> Just d
      _                   -> Nothing

performDirAction :: DirAction -> Action ()
performDirAction a = need [ actionTarget a ]

allTestDirs :: IO [FilePath]
allTestDirs = map takeDirectory <$> getDirectoryFilesIO "." ["**/all.T"]

targetsTrie :: IO (Trie FilePath DirAction)
targetsTrie = do
  testdirs <- allTestDirs
  return $ fromList $
    [ (pathComponents p,     BuildPackage p) | p <- knownPackages ] ++
    [ (splitDirectories dir, RunTestDir dir) | dir <- testdirs ]

  where pathComponents p = splitDirectories (pkgPath p)

targetForPath :: FilePath -> Trie FilePath DirAction -> Maybe DirAction
targetForPath dir trie = fst <$> lookupPath (splitDirectories dir) trie

-- Trie implementation

data Trie k a
  = Node (Maybe a) (Map k (Trie k a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

empty :: Trie k a
empty = Node Nothing Map.empty

value :: a -> Trie k a
value a = Node (Just a) Map.empty

cons :: k -> Trie k a -> Trie k a
cons k trie = Node Nothing (Map.singleton k trie)

at :: Ord k => [k] -> (Maybe a -> a) -> Trie k a -> Trie k a
at ks f trie = case foldr go (tweakNodeValue, value (f Nothing)) ks of
  (k, _) -> k trie

  where tweakNodeValue (Node mval ts) = Node (Just (f mval)) ts

        go :: Ord k
           => k
           -> (Trie k a -> Trie k a, Trie k a)
           -> (Trie k a -> Trie k a, Trie k a)
        go k (f, trie) =
          let f' (Node ma ts) =
                Node ma (Map.insertWith (const f) k trie ts)
          in (f', cons k trie)

insert :: Ord k => [k] -> a -> Trie k a -> Trie k a
insert ks a = at ks (const a)

fromList :: Ord k => [([k], a)] -> Trie k a
fromList = foldr (\(ks, a) trie -> insert ks a trie) empty

data Walk k a = Walk
  { walkWalked   :: [k]
  , walkValue    :: Maybe a
  , walkLeftover :: [k]
  , walkTrie     :: Trie k a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

walk :: Ord k => [k] -> Trie k a -> Walk k a
walk = go []

  where go walked [] t@(Node ma _) =
          mkWalk walked [] ma t
        go walked keys@(k:ks) t@(Node ma ts) =
          case Map.lookup k ts of
            Nothing   -> mkWalk walked keys ma t
            Just trie -> go (k:walked) ks trie

        mkWalk walked leftover ma trie =
          Walk (reverse walked) ma leftover trie

lookupPath :: Ord k => [k] -> Trie k a -> Maybe (a, [k])
lookupPath ks trie = (, walkLeftover w) <$> walkValue w

  where w = walk ks trie

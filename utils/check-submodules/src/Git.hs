{-# LANGUAGE ImportQualifiedPost #-}

module Git
    ( GitRepo(..)
    , submoduleIn

    , Ref
    , describeRef
    , submoduleCommit
    , Tag
    , reachableTags
    , changedFiles
    ) where

import System.Process.Typed
import Data.ByteString.Lazy.Char8 qualified as BSL
import System.FilePath ((</>))

newtype GitRepo = GitRepo { gitRepoPath :: FilePath }

submoduleIn :: GitRepo -> FilePath -> GitRepo
submoduleIn (GitRepo path) submod =
    GitRepo $ path </> submod

type Ref = String
type Tag = String

runGit :: GitRepo -> [String] -> IO BSL.ByteString
runGit (GitRepo path) args = do
    readProcessStdout_ $ setWorkingDir path (proc "git" args)

describeRef :: GitRepo -> Ref -> IO String
describeRef repo ref =
    head . lines . BSL.unpack <$> runGit repo ["describe", "--always", ref]

-- | Get the commit of the given submodule.
submoduleCommit :: GitRepo -> FilePath -> IO Ref
submoduleCommit repo submodule = do
    out <- runGit repo ["submodule", "status", submodule]
    case BSL.words $ BSL.drop 1 out of
      commit:_ -> return $ BSL.unpack commit
      _ -> fail "Unrecognized output from `git submodule status`"

-- | Get the most recent tags reacheable from the given commit.
reachableTags :: GitRepo -> Ref -> IO [Tag]
reachableTags repo ref =
    reverse . map BSL.unpack . BSL.lines <$> runGit repo ["tag", "--sort=taggerdate", "--merged", ref]

changedFiles :: GitRepo -> Ref -> Ref -> IO [FilePath]
changedFiles repo a b = do
    map BSL.unpack . BSL.lines <$> runGit repo ["diff", "--name-only", a, b]


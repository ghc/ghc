{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module CheckTags
    ( checkTags
    ) where

import Data.List (isPrefixOf, isSuffixOf)
import Git qualified
import Package (Package(..))
import Packages (packages)
import Pretty
import Control.Monad (unless)

findReleaseTag :: Git.GitRepo -> Package -> IO (Maybe Git.Tag)
findReleaseTag repo pkg = do
    allTags <- Git.reachableTags repo "HEAD"
    case filter (\tag -> pkgIsReleaseTag pkg tag || isGhcTag tag) allTags of
      [] -> return Nothing
      tag:_ -> return (Just tag)

isGhcTag :: Git.Tag -> Bool
isGhcTag tag = "-ghc" `isSuffixOf` tag

checkTag :: Git.GitRepo -> Package -> IO (Maybe Doc)
checkTag repo pkg = do
    mb_tag <- findReleaseTag repo pkg
    case mb_tag of
      Nothing -> return $ Just "No release tags found"
      Just tag -> checkChanges repo tag

-- | Check whether the tag only deviates from HEAD in trivial ways.
checkChanges :: Git.GitRepo -> Git.Ref -> IO (Maybe Doc)
checkChanges repo tag = do
    files <- Git.changedFiles repo tag "HEAD"
    case filter (not . okayChange) files of
      [] -> return Nothing
      badFiles  -> do
          described <- Git.describeRef repo "HEAD"
          let msg = vsep
                [ "Tag" <+> ppCommit (pretty tag) <+> "differs from" <+> ppCommit (pretty described) <+> "in:"
                , bulletList fileList
                ]
              maxFiles = 5
              fileList
                | n > 0 =
                    take maxFiles (map pretty badFiles) ++
                    ["... and" <+> pretty n <+> "other" <+> plural "file" "files" n]
                | otherwise = map pretty badFiles
                where n = length badFiles - maxFiles
          return $ Just msg

okayChange :: FilePath -> Bool
okayChange path
  | "." `isPrefixOf` path = True
  | ".gitignore" `isSuffixOf` path = True
  | otherwise = False

checkTags :: IO ()
checkTags = do
    let ghcRepo = Git.GitRepo "."
    errs <- mapM (\pkg -> (pkg,) <$> checkTag (Git.submoduleIn ghcRepo (pkgPath pkg)) pkg) packages
    putDoc $ bulletList
      [ severityIcon Error <+> ppPackage pkg <> ":" <+> err
      | (pkg, Just err) <- errs
      ]
    unless (null errs) $ fail "Tag issues above"

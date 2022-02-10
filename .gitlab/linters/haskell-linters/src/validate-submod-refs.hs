#!/opt/ghc/7.8.1/bin/runghc

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Shelly
import           System.Environment

import Common

main :: IO ()
main = do
    dir0:refs <- getArgs >>= \case
        [] -> fail "usage: submodchecker <git-repo> [<commit-id>+]"
        x  -> return x

    let dir = fromText (T.pack dir0)

    shelly $ forM_ (map T.pack refs) $ \ref -> do
      (cid,deltas) <- gitDiffTree dir ref

      let smDeltas = [ (smPath, smCid) | (_, (GitTypeGitLink, smCid), smPath) <- deltas ]

      unless (null smDeltas) $ do
          echo $ "Submodule update(s) detected in " <> cid <> ":"

          (_, msg) <- gitCatCommit dir cid

          unless ("submodule" `T.isInfixOf` msg) $ do
              echo "*FAIL* commit message does not contain magic 'submodule' word."
              echo "This lint avoids accidental changes to git submodules."
              echo "Include the word 'submodule' in your commit message to silence this warning, e.g. 'Update submodule'."
              quietExit 1

          bad <- fmap or $ forM smDeltas $ \(smPath,smCid) -> do
              echo $ " - " <> smPath <> " => " <> smCid

              let smAbsPath = dir </> smPath
              remoteBranches <- gitBranchesContain smAbsPath smCid

              let (wip, nonWip) = partition ("wip/" `T.isPrefixOf`) originBranches
                  originBranches = mapMaybe isOriginTracking remoteBranches
                  isOriginTracking = T.stripPrefix "origin/"
              let bad = null nonWip
              when bad $ do
                  echo $     "   *FAIL* commit not found in submodule repo"
                  echo       "          or not reachable from persistent branches"
                  echo       ""
                  when (not $ null wip) $ do
                    echo     "     Found the following non-mirrored WIP branches:"
                    forM_ wip $ \branch -> do
                      commit <- gitNormCid smAbsPath branch
                      echo $ "      - " <> branch <> " -> " <> commit
                    echo   ""
              pure bad

          if bad
            then quietExit 1
            else echo " OK"

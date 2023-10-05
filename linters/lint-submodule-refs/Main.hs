{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- base
import           Control.Monad
  ( forM, forM_, unless, when )
import           Data.List
  ( partition )
import           Data.Maybe
  ( mapMaybe )
import           System.Environment
  ( getArgs )
import           System.Exit
  ( ExitCode(..), exitWith )

-- text
import qualified Data.Text    as T
import qualified Data.Text.IO as T
  ( putStrLn, putStr )

-- linters-common
import           Linters.Common
  ( GitType(..)
  , gitBranchesContain, gitIsTagged, gitCatCommit, gitDiffTree, gitNormCid
  )

--------------------------------------------------------------------------------

main :: IO ()
main = do
    dir:refs <- getArgs >>= \case
        [] -> fail "usage: lint-submodule-refs <git-repo> [<commit-id>+]"
        x  -> return x

    forM_ (map T.pack refs) $ \ref -> do
      (cid,deltas) <- gitDiffTree dir ref

      let smDeltas = [ (smPath, smCid) | (_, (GitTypeGitLink, smCid), smPath) <- deltas ]

      unless (null smDeltas) $ do
          T.putStrLn $ "Submodule update(s) detected in " <> cid <> ":"

          (_, msg) <- gitCatCommit dir cid

          unless ("submodule" `T.isInfixOf` msg) $ do
              T.putStrLn "*FAIL* commit message does not contain magic 'submodule' word."
              T.putStrLn "This lint avoids accidental changes to git submodules."
              T.putStrLn "Include the word 'submodule' in your commit message to silence this warning, e.g. 'Update submodule'."
              exitWith (ExitFailure 1)

          bad <- fmap or $ forM smDeltas $ \(smPath,smCid) -> do
              T.putStr $ " - " <> smPath <> " => " <> smCid

              let smAbsPath = dir ++ "/" ++ T.unpack smPath
              remoteBranches <- gitBranchesContain smAbsPath smCid
              isTagged <- gitIsTagged smAbsPath smCid

              let (wip, nonWip) = partition ("wip/" `T.isPrefixOf`) originBranches
                  originBranches = mapMaybe isOriginTracking remoteBranches
                  isOriginTracking = T.stripPrefix "origin/"
              case (nonWip ++ isTagged) of
                [] -> do
                  T.putStrLn " ... BAD"
                  T.putStrLn $     "   *FAIL* commit not found in submodule repo"
                  T.putStrLn       "          or not reachable from persistent branches"
                  T.putStrLn       ""
                  when (not $ null wip) $ do
                    T.putStrLn     "     Found the following non-mirrored WIP branches:"
                    forM_ wip $ \branch -> do
                      commit <- gitNormCid smAbsPath ("origin/" <> branch)
                      T.putStrLn $ "      - " <> branch <> " -> " <> commit
                    T.putStrLn ""
                  return True
                (b:bs) -> do
                  let more = case bs of
                                [] -> ")"
                                rest -> " and " <> T.pack (show (length rest)) <> " more)"
                  T.putStrLn $ "... OK (" <>  b <> more
                  return False

          if bad
            then exitWith (ExitFailure 1)
            else T.putStrLn "OK"


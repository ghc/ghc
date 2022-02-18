{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Main where

-- base
import           Control.Monad
  ( forM, forM_, unless, when )
import           Data.Maybe
  ( isJust )
import           System.Environment
  ( getArgs )
import           System.Exit
  ( ExitCode(..), exitWith )

-- mtl
import           Control.Monad.Writer
  ( liftIO, execWriter, tell )

-- text
import           Data.Text
  ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as T
  ( putStrLn )

-- linters-common
import           Linters.Common
  ( LintMsg(..), LintLvl(..)
  , gitCatCommit, gitNormCid, tshow
  )

--------------------------------------------------------------------------------

main :: IO ()
main = do
    dir:refs <- getArgs >>= \case
        [] -> fail "usage: lint-commit-msg <git-repo> [<commit-id>+]"
        x  -> return x

    stats <- forM (map T.pack refs) $ \ref -> do
        cid <- gitNormCid dir ref
        (_, msg) <- gitCatCommit dir cid

        let cmsgs = lintMsg msg

        liftIO $ do
            -- putStrLn (T.unpack cid)
            -- forM_ (zip [1::Int ..] (T.lines msg)) $ \(lno,l) -> do
            --     putStrLn (show lno <> "\t" <> show l)
            -- putStrLn "--"

            let status = maximum (Nothing : [ Just lvl | LintMsg lvl _ _ _ <- cmsgs ])
                ok     = status < Just LintLvlErr

            unless (null cmsgs) $ do
                putStrLn "====================================================================================="
                putStrLn ("commit " <> T.unpack cid <> " has linter issues:")
                putStrLn ""
                forM_ cmsgs $ \(LintMsg lvl lno l m) -> do
                    let lvls = case lvl of
                            LintLvlErr  -> "*ERROR*"
                            LintLvlWarn -> "Warning"
                    putStrLn (" " <> lvls <> " on line " <> show lno <> ": " <> T.unpack m)
                    putStrLn (" > " <> show l)
                    putStrLn ""
                    return ()

            unless ok $
                putStrLn ("Validation FAILED for " <> T.unpack cid)

            return status

    unless (null $ filter isJust stats) $
        T.putStrLn "====================================================================================="

    let stats1 = maximum (Nothing : stats)

    unless (stats1 == Nothing) $ do
        T.putStrLn "There were commit message linter issues! For more information see"
        T.putStrLn " http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html"
        T.putStrLn ""

    unless (stats1 < Just LintLvlErr) $ do
        T.putStrLn "Validation FAILED because at least one commit had linter errors!"
        exitWith (ExitFailure 1)

    T.putStrLn "Commit message validation passed!"

-- | Commit message linter
lintMsg :: Text -> [LintMsg]
lintMsg msg0 = execWriter $ do
    -- subject-line validations
    if | T.null (T.strip subj) -> errSubj "empty subject line"
       | otherwise -> do
           when (T.stripStart subj /= subj) $
               errSubj "subject line with leading whitespace"

           when (T.stripEnd subj /= subj) $
               warnSubj "subject line with trailing whitespace"

           when (T.any (== '\t') subj) $
               errSubj "subject line contains TAB"

           if | slen > 80 -> errSubj  ("subject line longer than 80 characters (was " <> tshow slen <> " characters)"
                                       <> " -- , ideally subject line is at most 50 characters long")
              | slen > 50 -> warnSubj ("subject line longer than 50 characters (was " <> tshow slen <> " characters)")
              | slen < 8  -> errSubj  ("subject line shorter than 8 characters (was " <> tshow slen <> " characters)")
              | otherwise -> return ()

    -- 2nd-line & body validations
    case lns of
        []  -> return () -- empty commit msg -- will have caused already an LintLvlErr
        [_] -> return () -- single-line commit msg
        (_:line2:body) -> do
            -- 2nd line validations
            if | not (T.null line2)
                   -> tell [LintMsg LintLvlErr  2 line2 "2nd line must be empty"]
               | null body
                   -> tell [LintMsg LintLvlWarn 2 line2 "2nd line exists, but no commit msg body found"]
               | otherwise -> return ()

            -- body validations
            forM_ (zip [3..] body) $ \(lineno,l) -> do
                let llen = T.length l
                    warnBody m = tell [LintMsg LintLvlWarn lineno l m]
                    errBody  m = tell [LintMsg LintLvlErr  lineno l m]

                when (T.stripEnd l /= l) $ warnBody "trailing whitespace"

                when (T.any (== '\t') l) $ warnBody "contains TAB character"

                when (T.isPrefixOf "Summary:" l) $
                    warnBody "redundant Phabricator 'Summary:' tag detected -- please trim your commit message"

                when (T.isPrefixOf "Summary: Signed-off-by:" l) $
                    errBody "'Signed-Off-by:'-marker not starting on first column"

                if | llen > 100 -> errBody  ("body line longer than 100 characters (was "
                                             <> tshow llen <> " characters) -- "
                                             <> "ideally body lines are at most 72 characters long")
                   | llen > 72  -> warnBody ("body line longer than 72 characters (was "
                                             <> tshow llen <> " characters)")
                   | otherwise  -> return ()

    return ()
  where
    warnSubj m = tell [LintMsg LintLvlWarn 1 subj m]
    errSubj  m = tell [LintMsg LintLvlErr  1 subj m]

    lns = T.lines msg0

    subj | (l0:_) <- lns = l0
         | otherwise     = ""

    slen = T.length subj

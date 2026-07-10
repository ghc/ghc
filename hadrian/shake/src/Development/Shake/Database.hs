{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Lower-level primitives to drive Shake, which are wrapped into the
--   'Development.Shake.shake' function. Useful if you want to perform multiple Shake
--   runs in a row without reloading from the database.
--   Sometimes used in conjunction with @'shakeFiles'=\"\/dev\/null\"@.
--   Using these functions you can approximate the 'Development.Shake.shake' experience with:
--
-- @
-- shake opts rules = do
--     (_, after) \<- 'shakeWithDatabase' opts rules $ \\db -> do
--         'shakeOneShotDatabase' db
--         'shakeRunDatabase' db []
--     'shakeRunAfter' opts after
-- @
module Development.Shake.Database(
    ShakeDatabase,
    shakeOpenDatabase,
    shakeWithDatabase,
    shakeOneShotDatabase,
    shakeRunDatabase,
    shakeLiveFilesDatabase,
    shakeProfileDatabase,
    shakeErrorsDatabase,
    shakeRunAfter
    ) where

import Control.Concurrent.Extra
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import General.Cleanup
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Rules.Default


data UseState
    = Closed
    | Using String
    | Open {openOneShot :: Bool, openRequiresReset :: Bool}

-- | The type of an open Shake database. Created with
--   'shakeOpenDatabase' or 'shakeWithDatabase'. Used with
--   'shakeRunDatabase'. You may not execute simultaneous calls using 'ShakeDatabase'
--   on separate threads (it will raise an error).
data ShakeDatabase = ShakeDatabase (Var UseState) RunState

-- | Given some options and rules, return a pair. The first component opens the database,
--   the second cleans it up. The creation /does not/ need to be run masked, because the
--   cleanup is able to run at any point. Most users should prefer 'shakeWithDatabase'
--   which handles exceptions duration creation properly.
shakeOpenDatabase :: ShakeOptions -> Rules () -> IO (IO ShakeDatabase, IO ())
shakeOpenDatabase opts rules = do
    (cleanup, clean) <- newCleanup
    use <- newVar $ Open False False
    let alloc =
            withOpen use "shakeOpenDatabase" id $ \_ ->
                ShakeDatabase use <$> open cleanup opts (rules >> defaultRules)
    let free = do
            modifyVar_ use $ \case
                    Using s -> throwM $ errorStructured "Error when calling shakeOpenDatabase close function, currently running" [("Existing call", Just s)] ""
                    _ -> pure Closed
            clean
    pure (alloc, free)

withOpen :: Var UseState -> String -> (UseState -> UseState) -> (UseState -> IO a) -> IO a
withOpen var name final act = mask $ \restore -> do
    o <- modifyVar var $ \case
        Using s -> throwM $ errorStructured ("Error when calling " ++ name ++ ", currently running") [("Existing call", Just s)] ""
        Closed -> throwM $ errorStructured ("Error when calling " ++ name ++ ", already closed") [] ""
        o@Open{} -> pure (Using name, o)
    let clean = writeVar var $ final o
    res <- restore (act o) `onException` clean
    clean
    pure res

-- | Declare that a just-openned database will be used to call 'shakeRunDatabase' at most once.
--   If so, an optimisation can be applied to retain less memory.
shakeOneShotDatabase :: ShakeDatabase -> IO ()
shakeOneShotDatabase (ShakeDatabase use _) =
    withOpen use "shakeOneShotDatabase" (\o -> o{openOneShot=True}) $ \_ -> pure ()

-- | Given some options and rules, create a 'ShakeDatabase' that can be used to run
--   executions.
shakeWithDatabase :: ShakeOptions -> Rules () -> (ShakeDatabase -> IO a) -> IO a
shakeWithDatabase opts rules act = do
    (db, clean) <- shakeOpenDatabase opts rules
    (act =<< db) `finally` clean

-- | Given a 'ShakeDatabase', what files did the execution ensure were up-to-date
--   in the previous call to 'shakeRunDatabase'. Corresponds to the list of files
--   written out to 'shakeLiveFiles'.
shakeLiveFilesDatabase :: ShakeDatabase -> IO [FilePath]
shakeLiveFilesDatabase (ShakeDatabase use s) =
    withOpen use "shakeLiveFilesDatabase" id $ \_ ->
        liveFilesState s

-- | Given a 'ShakeDatabase', generate profile information to the given file about the latest run.
--   See 'shakeReport' for the types of file that can be generated.
shakeProfileDatabase :: ShakeDatabase -> FilePath -> IO ()
shakeProfileDatabase (ShakeDatabase use s) file =
    withOpen use "shakeProfileDatabase" id $ \_ ->
        profileState s file

-- | Given a 'ShakeDatabase', what files did the execution reach an error on last time.
--   Some special considerations when using this function:
--
-- * The presence of an error does not mean the build will fail, specifically if a
--   previously required dependency was run and raised an error, then the thing that previously
--   required it will be run. If the build system has changed in an untracked manner,
--   the build may succeed this time round.
--
-- * If the previous run actually failed then 'shakeRunDatabase' will have thrown an exception.
--   You probably want to catch that exception so you can make the call to 'shakeErrorsDatabase'.
--
-- * You may see a single failure reported multiple times, with increasingly large call stacks, showing
--   the ways in which the error lead to further errors throughout.
--
-- * The 'SomeException' values are highly likely to be of type 'ShakeException'.
--
-- * If you want as many errors as possible in one run set @'shakeStaunch'=True@.
shakeErrorsDatabase :: ShakeDatabase -> IO [(String, SomeException)]
shakeErrorsDatabase (ShakeDatabase use s) =
    withOpen use "shakeErrorsDatabase" id $ \_ ->
        errorsState s

-- | Given an open 'ShakeDatabase', run both whatever actions were added to the 'Rules',
--   plus the list of 'Action' given here. Returns the results from the explicitly passed
--   actions along with a list of actions to run after the database was closed, as added with
--   'Development.Shake.runAfter' and 'Development.Shake.removeFilesAfter'.
shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO ([a], [IO ()])
shakeRunDatabase (ShakeDatabase use s) as =
    withOpen use "shakeRunDatabase" (\o -> o{openRequiresReset=True}) $ \Open{..} -> do
        when openRequiresReset $ do
            when openOneShot $
                throwM $ errorStructured "Error when calling shakeRunDatabase twice, after calling shakeOneShotDatabase" [] ""
            reset s
        (refs, as) <- fmap unzip $ forM as $ \a -> do
            ref <- newIORef Nothing
            pure (ref, liftIO . writeIORef ref . Just =<< a)
        after <- run s openOneShot $ map void as
        results <- mapM readIORef refs
        case sequence results of
            Just result -> pure (result, after)
            Nothing -> throwM $ errorInternal "Expected all results were written, but some where not"

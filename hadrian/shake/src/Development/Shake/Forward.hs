{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, Rank2Types, ScopedTypeVariables, ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | A module for producing forward-defined build systems, in contrast to standard backwards-defined
--   build systems such as shake. Based around ideas from <https://code.google.com/p/fabricate/ fabricate>.
--   As an example:
--
-- @
-- import "Development.Shake"
-- import "Development.Shake.Forward"
-- import "Development.Shake.FilePath"
--
-- main = 'shakeArgsForward' 'shakeOptions' $ do
--     contents <- 'readFileLines' \"result.txt\"
--     'cache' $ 'cmd' \"tar -cf result.tar\" contents
-- @
--
--   Compared to backward-defined build systems (such as normal Shake), forward-defined build
--   systems tend to be simpler for simple systems (less boilerplate, more direct style), but more
--   complex for larger build systems (requires explicit parallelism, explicit sharing of build products,
--   no automatic command line targets). As a general approach for writing forward-defined systems:
--
-- * Figure out the sequence of system commands that will build your project.
--
-- * Write a simple 'Action' that builds your project.
--
-- * Insert 'cache' in front of most system commands.
--
-- * Replace most loops with 'forP', where they can be executed in parallel.
--
-- * Where Haskell performs real computation, if zero-build performance is insufficient, use 'cacheAction'.
--
--   All forward-defined systems use 'AutoDeps', which requires @fsatrace@ to be on the @$PATH@.
--   You can obtain @fsatrace@ from <https://github.com/jacereda/fsatrace>. You must set
--   'shakeLintInside' to specify where 'AutoDeps' will look for dependencies - if you want all dependencies
--   everywhere use @[\"\"]@.
--
--   This module is considered experimental - it has not been battle tested. There are now a few possible
--   alternatives in this space:
--
-- * Pier <http://hackage.haskell.org/package/pier/docs/Pier-Core-Artifact.html> (built on Shake).
--
-- * Rattle <https://github.com/ndmitchell/rattle> (by the same author as Shake).
--
-- * Stroll <https://github.com/snowleopard/stroll>.
module Development.Shake.Forward(
    shakeForward, shakeArgsForward,
    forwardOptions, forwardRule,
    cache, cacheAction, cacheActionWith,
    ) where

import Control.Monad
import Development.Shake
import Development.Shake.Rule
import Development.Shake.Command
import Development.Shake.Classes
import Development.Shake.FilePath
import Data.IORef.Extra
import Data.Either
import Data.Typeable
import Data.List.Extra
import Control.Exception.Extra
import Numeric
import System.IO.Unsafe
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as Map


{-# NOINLINE forwards #-}
forwards :: IORef (Map.HashMap Forward (Action Forward))
forwards = unsafePerformIO $ newIORef Map.empty

-- I'd like to use TypeRep, but it doesn't have any instances in older versions
newtype Forward = Forward (String, String, BS.ByteString) -- the type, the Show, the payload
    deriving (Hashable,Typeable,Eq,NFData,Binary)

mkForward :: (Typeable a, Show a, Binary a) => a -> Forward
mkForward x = Forward (show $ typeOf x, show x, encode' x)

unForward :: forall a . (Typeable a, Binary a) => Forward -> a
unForward (Forward (got,_,x))
    | got /= want = error $ "Failed to match forward type, wanted " ++ show want ++ ", got " ++ show got
    | otherwise = decode' x
    where want = show $ typeRep (Proxy :: Proxy a)

encode' :: Binary a => a -> BS.ByteString
encode' = BS.concat . LBS.toChunks . encode

decode' :: Binary a => BS.ByteString -> a
decode' = decode . LBS.fromChunks . pure

type instance RuleResult Forward = Forward

instance Show Forward where
    show (Forward (_,x,_)) = x

-- | Run a forward-defined build system.
shakeForward :: ShakeOptions -> Action () -> IO ()
shakeForward opts act = shake (forwardOptions opts) (forwardRule act)

-- | Run a forward-defined build system, interpreting command-line arguments.
shakeArgsForward :: ShakeOptions -> Action () -> IO ()
shakeArgsForward opts act = shakeArgs (forwardOptions opts) (forwardRule act)

-- | Given an 'Action', turn it into a 'Rules' structure which runs in forward mode.
forwardRule :: Action () -> Rules ()
forwardRule act = do
    opts <- getShakeOptionsRules
    when (null $ shakeLintInside opts) $
        fail "When running in forward mode you must set shakeLintInside to specify where to detect dependencies"
    addBuiltinRule noLint noIdentity $ \k old mode ->
        case old of
            Just old | mode == RunDependenciesSame -> pure $ RunResult ChangedNothing old (decode' old)
            _ -> do
                res <- liftIO $ atomicModifyIORef forwards $ \mp -> (Map.delete k mp, Map.lookup k mp)
                case res of
                    Nothing -> liftIO $ errorIO $ "Failed to find action name, " ++ show k
                    Just act -> do
                        new <- act
                        pure $ RunResult ChangedRecomputeSame (encode' new) new
    action act

-- | Given a 'ShakeOptions', set the options necessary to execute in forward mode.
forwardOptions :: ShakeOptions -> ShakeOptions
forwardOptions opts = opts{shakeCommandOptions=[AutoDeps]}


-- | Cache an action, given a key and an 'Action'. Each call in your program should specify a different
--   key, but the key should remain consistent between runs. Ideally, the 'Action' will gather all its dependencies
--   with tracked operations, e.g. 'readFile\''. However, if information is accessed from the environment
--   (e.g. the action is a closure), you should call 'cacheActionWith' being explicit about what is captured.
cacheAction :: (Typeable a, Binary a, Show a, Typeable b, Binary b, Show b) => a -> Action b -> Action b
cacheAction (mkForward -> key) (action :: Action b) = do
    liftIO $ atomicModifyIORef_ forwards $ Map.insert key (mkForward <$> action)
    res <- apply1 key
    liftIO $ atomicModifyIORef_ forwards $ Map.delete key
    pure $ unForward res

newtype With a = With a
    deriving (Typeable, Binary, Show)

-- | Like 'cacheAction', but also specify which information is captured by the closure of the 'Action'. If that
--   information changes, the 'Action' will be rerun.
cacheActionWith :: (Typeable a, Binary a, Show a, Typeable b, Binary b, Show b, Typeable c, Binary c, Show c) => a -> b ->  Action c -> Action c
cacheActionWith key argument action = do
    cacheAction (With argument) $ do
        alwaysRerun
        pure argument
    cacheAction key $ do
        apply1 $ mkForward $ With argument
        action

-- | Apply caching to an external command using the same arguments as 'cmd'.
--
-- > cache $ cmd "gcc -c" ["foo.c"] "-o" ["foo.o"]
--
--   This command will be cached, with the inputs/outputs traced. If any of the
--   files used by this command (e.g. @foo.c@ or header files it imports) then
--   the command will rerun.
cache :: (forall r . CmdArguments r => r) -> Action ()
cache cmd = do
    let CmdArgument args = cmd
    let isDull ['-',_] = True; isDull _ = False
    let name = headDef "unknown" $ filter (not . isDull) $ drop1 $ rights args
    cacheAction (Command $ toStandard name ++ " #" ++ upper (showHex (abs $ hash $ show args) "")) cmd

newtype Command = Command String
    deriving (Typeable, Binary)

instance Show Command where
    show (Command x) = "command " ++ x

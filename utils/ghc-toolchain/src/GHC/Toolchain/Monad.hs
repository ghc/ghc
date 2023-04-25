{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module GHC.Toolchain.Monad
    ( Env(..)
    , M
    , runM
    , getEnv
    , throwE
    , ifCrossCompiling

      -- * File I/O
    , readFile
    , writeFile
    , createFile

      -- * Logging
    , logInfo
    , logDebug
    , checking
    , withLogContext
    ) where

import Prelude hiding (readFile, writeFile)
import qualified Prelude

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Except as Except
import System.IO hiding (readFile, writeFile)

data Env = Env { verbosity    :: Int
               , targetPrefix :: Maybe String
               , keepTemp     :: Bool
               , logContexts  :: [String]
               }

newtype M a = M (Except.ExceptT [Error] (Reader.ReaderT Env IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, Alternative,
              -- TODO: Eliminate these instances
              MC.MonadThrow, MC.MonadCatch, MC.MonadMask)

runM :: Env -> M a -> IO (Either [Error] a)
runM env (M k) =
    Reader.runReaderT (Except.runExceptT k) env

getEnv :: M Env
getEnv = M $ lift Reader.ask

data Error = Error { errorMessage :: String
                   , errorLogContexts :: [String]
                   }
    deriving (Show)

throwE :: String -> M a
throwE msg = do
    e <- getEnv
    let err = Error { errorMessage = msg
                    , errorLogContexts = logContexts e
                    }
    M (Except.throwE [err])

withLogContext :: String -> M a -> M a
withLogContext ctxt k = do
    env <- getEnv
    let env' = env { logContexts = ctxt : logContexts env }
    logDebug $ "Entering: " ++ ctxt
    r <- liftIO $ runM env' k
    either (M . Except.throwE) return r

checking :: Show a => String -> M a -> M a
checking what k = do
    logInfo $ "checking " ++ what ++ "..."
    r <- withLogContext ("checking " ++ what) k
    logInfo $ "found " ++ what ++ ": " ++ show r
    return r

logDebug :: String -> M ()
logDebug = logMsg 2

logInfo :: String -> M ()
logInfo = logMsg 1

logMsg :: Int -> String -> M ()
logMsg v msg = do
    e <- getEnv
    let n = length $ logContexts e
        indent = concat $ replicate n "  "
    when (verbosity e >= v) (liftIO $ hPutStrLn stderr $ indent ++ msg)

readFile :: FilePath -> M String
readFile path = liftIO $ Prelude.readFile path

writeFile :: FilePath -> String -> M ()
writeFile path s = liftIO $ Prelude.writeFile path s

-- | Create an empty file.
createFile :: FilePath -> M ()
createFile path = writeFile path ""

ifCrossCompiling
    :: M a  -- ^ what to do when cross-compiling
    -> M a  -- ^ what to do otherwise
    -> M a
ifCrossCompiling cross other = other -- TODO

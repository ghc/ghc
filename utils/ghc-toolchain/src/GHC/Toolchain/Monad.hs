{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module GHC.Toolchain.Monad
    ( Env(..)
    , M
    , runM
    , getEnv
    , makeM
    , throwE, throwEs
    , ifCrossCompiling

      -- * File I/O
    , readFile
    , writeFile
    , appendFile
    , createFile

      -- * Logging
    , logInfo
    , logDebug
    , checking
    , withLogContext
    ) where

import Prelude hiding (readFile, writeFile, appendFile)
import qualified Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Except as Except
import System.IO hiding (readFile, writeFile, appendFile)
-- import qualified System.Directory
import qualified Data.Text    as T
import qualified Data.Text.IO as T


data Env = Env { verbosity    :: Int
               , targetPrefix :: Maybe String
               , keepTemp     :: Bool
               , canLocallyExecute :: Bool
               , logContexts  :: [String]
               }

newtype M a = M (Except.ExceptT [Error] (Reader.ReaderT Env IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, Alternative)

runM :: Env -> M a -> IO (Either [Error] a)
runM env (M k) =
    Reader.runReaderT (Except.runExceptT k) env

getEnv :: M Env
getEnv = M $ lift Reader.ask

makeM :: IO (Either [Error] a) -> M a
makeM io = M (Except.ExceptT (Reader.ReaderT (\_env -> io)))

data Error = Error { errorMessage :: String
                   , errorLogContexts :: [String]
                   }
    deriving (Show)

throwE :: String -> M a
throwE msg = throwEs [msg]

-- | Throw an error with multiple lines.
-- This should be used rather than `throwE . unlines` to preserve proper
-- logging indentation.
throwEs :: [String] -> M a
throwEs msgs = do
    e <- getEnv
    forM_ msgs $ \msg -> do
      logInfo msg
    let err = Error { errorMessage = unlines msgs
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
readFile path = liftIO $ T.unpack <$> T.readFile path
              -- Use T.readfile to read the file strictly, or otherwise run
              -- into file locking bugs on Windows

writeFile :: FilePath -> String -> M ()
writeFile path s = liftIO $ Prelude.writeFile path s

appendFile :: FilePath -> String -> M ()
appendFile path s = liftIO $ Prelude.appendFile path s

-- | Create an empty file.
createFile :: FilePath -> M ()
createFile path = writeFile path ""

-- | Branch on whether we can execute target code locally.
ifCrossCompiling
    :: M a  -- ^ what to do when cross-compiling
    -> M a  -- ^ what to do otherwise
    -> M a
ifCrossCompiling cross other = do
  canExec <- canLocallyExecute <$> getEnv
  if not canExec then cross -- can't execute, this is a cross target
                 else other -- can execute, run the other action

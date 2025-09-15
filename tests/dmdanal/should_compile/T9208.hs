{-# LANGUAGE LambdaCase, BangPatterns, MagicHash, TupleSections, ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}  -- Suppress warnings for unimplemented methods

------------- WARNING ---------------------
--
-- This program is utterly bogus. It takes a value of type ()
-- and unsafe-coerces it to a function, and applies it.
-- This is caught by an ASSERT with a debug compiler.
--
-- See #9208 for discussion
--
--------------------------------------------

{- | Evaluate Template Haskell splices on node.js,
     using pipes to communicate with GHCJS
 -}

-- module GHCJS.Prim.TH.Eval
module Eval (
         runTHServer
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail (MonadFail(fail))
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL

import           GHC.Base                 (Any)

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import           Unsafe.Coerce

data THResultType = THExp | THPat | THType | THDec

data Message
  -- | GHCJS compiler to node.js requests
  = RunTH THResultType ByteString TH.Loc
  -- | node.js to GHCJS compiler responses
  | RunTH' THResultType ByteString [TH.Dec] -- ^ serialized AST and additional toplevel declarations

instance Binary THResultType where
  put _ = return ()
  get   = return undefined

instance Binary Message where
  put _ = return ()
  get   = return undefined

data QState = QState

data GHCJSQ a = GHCJSQ { runGHCJSQ :: QState -> IO (a, QState) }

instance Functor GHCJSQ where
  fmap f (GHCJSQ s) = GHCJSQ $ fmap (\(x,s') -> (f x,s')) . s

instance Applicative GHCJSQ where
  f <*> a = GHCJSQ $ \s ->
    do (f',s')   <- runGHCJSQ f s
       (a', s'') <- runGHCJSQ a s'
       return (f' a', s'')
  pure x = GHCJSQ (\s -> return (x,s))

instance Monad GHCJSQ where
  (>>=) m f = GHCJSQ $ \s ->
    do (m', s')  <- runGHCJSQ m s
       (a,  s'') <- runGHCJSQ (f m') s'
       return (a, s'')
  return    = pure

instance MonadFail GHCJSQ where
  fail = undefined

instance MonadIO GHCJSQ where liftIO m = GHCJSQ $ \s -> fmap (,s) m
instance TH.Quasi GHCJSQ

-- | the Template Haskell server
runTHServer :: IO ()
runTHServer = void $ runGHCJSQ server QState
  where
    server = TH.qRunIO awaitMessage >>= \case
      RunTH t code loc -> do
        a <- TH.qRunIO $ loadTHData code
        runTH t a loc
      _ -> TH.qRunIO (putStrLn "warning: ignoring unexpected message type")

runTH :: THResultType -> Any -> TH.Loc -> GHCJSQ ()
runTH rt obj loc = do
  res <- case rt of
           THExp  -> runTHCode (unsafeCoerce obj :: TH.Q TH.Exp)
           THPat  -> runTHCode (unsafeCoerce obj :: TH.Q TH.Pat)
           THType -> runTHCode (unsafeCoerce obj :: TH.Q TH.Type)
           THDec  -> runTHCode (unsafeCoerce obj :: TH.Q [TH.Dec])
  TH.qRunIO (sendResult $ RunTH' rt res [])

runTHCode :: {- Binary a => -} TH.Q a -> GHCJSQ ByteString
runTHCode c = TH.runQ c >> return B.empty

loadTHData :: ByteString -> IO Any
loadTHData bs = return (unsafeCoerce ())

awaitMessage :: IO Message
awaitMessage = fmap (runGet get) (return BL.empty)

-- | send result back
sendResult :: Message -> IO ()
sendResult msg = return ()

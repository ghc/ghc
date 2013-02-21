-- |The Vectorisation monad.

module Vectorise.Monad.Base (
  -- * The Vectorisation Monad
  VResult(..),
  VM(..),

  -- * Lifting
  liftDs,

  -- * Error Handling
  cantVectorise,
  maybeCantVectorise,
  maybeCantVectoriseM,
  
  -- * Debugging
  emitVt, traceVt, dumpOptVt, dumpVt,
  
  -- * Control
  noV, traceNoV,
  ensureV, traceEnsureV,
  onlyIfV,
  tryV, tryErrV,
  maybeV,  traceMaybeV,
  orElseV, orElseErrV,
  fixV,
) where

import Vectorise.Builtins
import Vectorise.Env

import DsMonad
import TcRnMonad
import ErrUtils
import Outputable
import DynFlags

import Control.Monad


-- The Vectorisation Monad ----------------------------------------------------

-- |Vectorisation can either succeed with new envionment and a value, or return with failure
-- (including a description of the reason for failure).
--
data VResult a 
  = Yes GlobalEnv LocalEnv a 
  | No  SDoc

newtype VM a 
  = VM { runVM :: Builtins -> GlobalEnv -> LocalEnv -> DsM (VResult a) }

instance Monad VM where
  return x   = VM $ \_  genv lenv -> return (Yes genv lenv x)
  VM p >>= f = VM $ \bi genv lenv -> do
                                       r <- p bi genv lenv
                                       case r of
                                         Yes genv' lenv' x -> runVM (f x) bi genv' lenv'
                                         No reason         -> return $ No reason

instance Applicative VM where
  pure  = return
  (<*>) = ap
  
instance Functor VM where
  fmap = liftM
  
instance MonadIO VM where
  liftIO = liftDs . liftIO

instance HasDynFlags VM where
    getDynFlags = liftDs getDynFlags

-- Lifting --------------------------------------------------------------------

-- |Lift a desugaring computation into the vectorisation monad.
--
liftDs :: DsM a -> VM a
liftDs p = VM $ \_ genv lenv -> do { x <- p; return (Yes genv lenv x) }


-- Error Handling -------------------------------------------------------------

-- |Throw a `pgmError` saying we can't vectorise something.
--
cantVectorise :: DynFlags -> String -> SDoc -> a
cantVectorise dflags s d = pgmError
                  . showSDoc dflags
                  $ vcat [text "*** Vectorisation error ***",
                          nest 4 $ sep [text s, nest 4 d]]

-- |Like `fromJust`, but `pgmError` on Nothing.
--
maybeCantVectorise :: DynFlags -> String -> SDoc -> Maybe a -> a
maybeCantVectorise dflags s d Nothing  = cantVectorise dflags s d
maybeCantVectorise _ _ _ (Just x) = x

-- |Like `maybeCantVectorise` but in a `Monad`.
--
maybeCantVectoriseM :: (Monad m, HasDynFlags m)
                    => String -> SDoc -> m (Maybe a) -> m a
maybeCantVectoriseM s d p
  = do
      r <- p
      case r of
        Just x  -> return x
        Nothing ->
            do dflags <- getDynFlags
               cantVectorise dflags s d


-- Debugging ------------------------------------------------------------------

-- |Output a trace message if -ddump-vt-trace is active.
--
emitVt :: String -> SDoc -> VM () 
emitVt herald doc
  = liftDs $ do
      dflags <- getDynFlags
      liftIO . printInfoForUser dflags alwaysQualify $
        hang (text herald) 2 doc

-- |Output a trace message if -ddump-vt-trace is active.
--
traceVt :: String -> SDoc -> VM () 
traceVt herald doc
  = do dflags <- getDynFlags
       when (1 <= traceLevel dflags) $
           liftDs $ traceOptIf Opt_D_dump_vt_trace $ hang (text herald) 2 doc

-- |Dump the given program conditionally.
--
dumpOptVt :: DumpFlag -> String -> SDoc -> VM ()
dumpOptVt flag header doc 
  = do { b <- liftDs $ doptM flag
       ; if b 
         then dumpVt header doc 
         else return () 
       }

-- |Dump the given program unconditionally.
--
dumpVt :: String -> SDoc -> VM ()
dumpVt header doc 
  = do { unqual <- liftDs mkPrintUnqualifiedDs
       ; dflags <- liftDs getDynFlags
       ; liftIO $ printInfoForUser dflags unqual (mkDumpDoc header doc)
       }


-- Control --------------------------------------------------------------------

-- |Return some result saying we've failed.
--
noV :: SDoc -> VM a
noV reason = VM $ \_ _ _ -> return $ No reason

-- |Like `traceNoV` but also emit some trace message to stderr.
--
traceNoV :: String -> SDoc -> VM a
traceNoV s d = pprTrace s d $ noV d

-- |If `True` then carry on, otherwise fail.
--
ensureV :: SDoc -> Bool -> VM ()
ensureV reason  False = noV reason
ensureV _reason True  = return ()

-- |Like `ensureV` but if we fail then emit some trace message to stderr.
--
traceEnsureV :: String -> SDoc -> Bool -> VM ()
traceEnsureV s d False = traceNoV s d
traceEnsureV _ _ True  = return ()

-- |If `True` then return the first argument, otherwise fail.
--
onlyIfV :: SDoc -> Bool -> VM a -> VM a
onlyIfV reason b p = ensureV reason b >> p

-- |Try some vectorisation computaton.
--
-- If it succeeds then return `Just` the result; otherwise, return `Nothing` after emitting a
-- failure message.
--
tryErrV :: VM a -> VM (Maybe a)
tryErrV (VM p) = VM $ \bi genv lenv ->
  do
    r <- p bi genv lenv
    case r of
      Yes genv' lenv' x -> return (Yes genv' lenv' (Just x))
      No reason         -> do { unqual <- mkPrintUnqualifiedDs
                              ; dflags <- getDynFlags
                              ; liftIO $ 
                                  printInfoForUser dflags unqual $
                                    text "Warning: vectorisation failure:" <+> reason
                              ; return (Yes genv  lenv  Nothing)
                              }

-- |Try some vectorisation computaton.
--
-- If it succeeds then return `Just` the result; otherwise, return `Nothing` without emitting a
-- failure message.
--
tryV :: VM a -> VM (Maybe a)
tryV (VM p) = VM $ \bi genv lenv ->
  do
    r <- p bi genv lenv
    case r of
      Yes genv' lenv' x -> return (Yes genv' lenv' (Just x))
      No _reason        -> return (Yes genv  lenv  Nothing)

-- |If `Just` then return the value, otherwise fail.
--
maybeV :: SDoc -> VM (Maybe a) -> VM a
maybeV reason p = maybe (noV reason) return =<< p

-- |Like `maybeV` but emit a message to stderr if we fail.
--
traceMaybeV :: String -> SDoc -> VM (Maybe a) -> VM a
traceMaybeV s d p = maybe (traceNoV s d) return =<< p

-- |Try the first computation,
--
--   * if it succeeds then take the returned value,
--   * if it fails then run the second computation instead while emitting a failure message.
--
orElseErrV :: VM a -> VM a -> VM a
orElseErrV p q = maybe q return =<< tryErrV p

-- |Try the first computation,
--
--   * if it succeeds then take the returned value,
--   * if it fails then run the second computation instead without emitting a failure message.
--
orElseV :: VM a -> VM a -> VM a
orElseV p q = maybe q return =<< tryV p

-- |Fixpoint in the vectorisation monad.
--
fixV :: (a -> VM a) -> VM a
fixV f = VM (\bi genv lenv -> fixDs $ \r -> runVM (f (unYes r)) bi genv lenv )
  where
    -- NOTE: It is essential that we are lazy in r above so do not replace
    --       calls to this function by an explicit case.
    unYes (Yes _ _ x) = x
    unYes (No reason) = pprPanic "Vectorise.Monad.Base.fixV: no result" reason

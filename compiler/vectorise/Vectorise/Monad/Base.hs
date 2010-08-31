
-- | The Vectorisation monad.
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
	
	-- * Control
	noV,	 traceNoV,
	ensureV, traceEnsureV,
	onlyIfV,
	tryV,
	maybeV,  traceMaybeV,
	orElseV,
	fixV,
) where
import Vectorise.Builtins
import Vectorise.Env

import DsMonad
import Outputable
	

-- The Vectorisation Monad ----------------------------------------------------
-- | Vectorisation can either succeed with new envionment and a value,
--   or return with failure.
data VResult a 
	= Yes GlobalEnv LocalEnv a | No

newtype VM a 
	= VM { runVM :: Builtins -> GlobalEnv -> LocalEnv -> DsM (VResult a) }

instance Monad VM where
  return x   = VM $ \_  genv lenv -> return (Yes genv lenv x)
  VM p >>= f = VM $ \bi genv lenv -> do
                                      r <- p bi genv lenv
                                      case r of
                                        Yes genv' lenv' x -> runVM (f x) bi genv' lenv'
                                        No                -> return No


-- Lifting --------------------------------------------------------------------
-- | Lift a desugaring computation into the vectorisation monad.
liftDs :: DsM a -> VM a
liftDs p = VM $ \_ genv lenv -> do { x <- p; return (Yes genv lenv x) }


-- Error Handling -------------------------------------------------------------
-- | Throw a `pgmError` saying we can't vectorise something.
cantVectorise :: String -> SDoc -> a
cantVectorise s d = pgmError
                  . showSDocDump
                  $ vcat [text "*** Vectorisation error ***",
                          nest 4 $ sep [text s, nest 4 d]]


-- | Like `fromJust`, but `pgmError` on Nothing.
maybeCantVectorise :: String -> SDoc -> Maybe a -> a
maybeCantVectorise s d Nothing  = cantVectorise s d
maybeCantVectorise _ _ (Just x) = x


-- | Like `maybeCantVectorise` but in a `Monad`.
maybeCantVectoriseM :: Monad m => String -> SDoc -> m (Maybe a) -> m a
maybeCantVectoriseM s d p
  = do
      r <- p
      case r of
        Just x  -> return x
        Nothing -> cantVectorise s d


-- Control --------------------------------------------------------------------
-- | Return some result saying we've failed.
noV :: VM a
noV	= VM $ \_ _ _ -> return No


-- | Like `traceNoV` but also emit some trace message to stderr.
traceNoV :: String -> SDoc -> VM a
traceNoV s d  = pprTrace s d noV


-- | If `True` then carry on, otherwise fail.
ensureV :: Bool -> VM ()
ensureV False = noV
ensureV True  = return ()


-- | Like `ensureV` but if we fail then emit some trace message to stderr.
traceEnsureV :: String -> SDoc -> Bool -> VM ()
traceEnsureV s d False = traceNoV s d
traceEnsureV _ _ True  = return ()


-- | If `True` then return the first argument, otherwise fail.
onlyIfV :: Bool -> VM a -> VM a
onlyIfV b p = ensureV b >> p


-- | Try some vectorisation computaton.
--	If it succeeds then return `Just` the result,
--	otherwise return `Nothing`.
tryV :: VM a -> VM (Maybe a)
tryV (VM p) = VM $ \bi genv lenv ->
  do
    r <- p bi genv lenv
    case r of
      Yes genv' lenv' x -> return (Yes genv' lenv' (Just x))
      No                -> return (Yes genv  lenv  Nothing)


-- | If `Just` then return the value, otherwise fail.
maybeV :: VM (Maybe a) -> VM a
maybeV p = maybe noV return =<< p


-- | Like `maybeV` but emit a message to stderr if we fail.
traceMaybeV :: String -> SDoc -> VM (Maybe a) -> VM a
traceMaybeV s d p = maybe (traceNoV s d) return =<< p


-- | Try the first computation,
--	if it succeeds then take the returned value,
--	if it fails then run the second computation instead.
orElseV :: VM a -> VM a -> VM a
orElseV p q = maybe q return =<< tryV p


-- | Fixpoint in the vectorisation monad.
fixV :: (a -> VM a) -> VM a
fixV f = VM (\bi genv lenv -> fixDs $ \r -> runVM (f (unYes r)) bi genv lenv )
  where
    -- NOTE: It is essential that we are lazy in r above so do not replace
    --       calls to this function by an explicit case.
    unYes (Yes _ _ x) = x
    unYes No          = panic "Vectorise.Monad.Base.fixV: no result"


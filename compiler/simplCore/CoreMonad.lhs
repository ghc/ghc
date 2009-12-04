%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[CoreMonad]{The core pipeline monad}

\begin{code}
{-# LANGUAGE UndecidableInstances #-}

module CoreMonad (
    -- * The monad
    CoreM, runCoreM,
    
    -- ** Reading from the monad
    getHscEnv, getRuleBase, getModule,
    getDynFlags, getOrigNameCache,
    
    -- ** Writing to the monad
    addSimplCount,
    
    -- ** Lifting into the monad
    liftIO, liftIOWithCount,
    liftIO1, liftIO2, liftIO3, liftIO4,
    
    -- ** Dealing with annotations
    getAnnotations, getFirstAnnotations,
    
    -- ** Debug output
    endPass, endPassIf, endIteration,

    -- ** Screen output
    putMsg, putMsgS, errorMsg, errorMsgS, 
    fatalErrorMsg, fatalErrorMsgS, 
    debugTraceMsg, debugTraceMsgS,
    dumpIfSet_dyn,

#ifdef GHCI
    -- * Getting 'Name's
    thNameToGhcName
#endif
  ) where

#ifdef GHCI
import Name( Name )
#endif
import CoreSyn
import PprCore
import CoreUtils
import CoreLint		( lintCoreBindings )
import PrelNames        ( iNTERACTIVE )
import HscTypes
import Module           ( Module )
import DynFlags         ( DynFlags, DynFlag )
import SimplMonad       ( SimplCount, plusSimplCount, zeroSimplCount )
import Rules            ( RuleBase )
import Annotations

import IOEnv hiding     ( liftIO, failM, failWithM )
import qualified IOEnv  ( liftIO )
import TcEnv            ( tcLookupGlobal )
import TcRnMonad        ( TcM, initTc )

import Outputable
import FastString
import qualified ErrUtils as Err
import Maybes
import UniqSupply
import LazyUniqFM       ( UniqFM, mapUFM, filterUFM )

import Data.Dynamic
import Data.IORef
import Data.Word
import Control.Monad

import Prelude hiding   ( read )

#ifdef GHCI
import {-# SOURCE #-} TcSplice ( lookupThName_maybe )
import qualified Language.Haskell.TH as TH
#endif
\end{code}

%************************************************************************
%*									*
                       Debug output
%*									*
%************************************************************************

These functions are not CoreM monad stuff, but they probably ought to
be, and it makes a conveneint place.  place for them.  They print out
stuff before and after core passes, and do Core Lint when necessary.

\begin{code}
endPass :: DynFlags -> String -> DynFlag -> [CoreBind] -> [CoreRule] -> IO ()
endPass = dumpAndLint Err.dumpIfSet_core

endPassIf :: Bool -> DynFlags -> String -> DynFlag -> [CoreBind] -> [CoreRule] -> IO ()
endPassIf cond = dumpAndLint (Err.dumpIf_core cond)

-- Same as endPass but doesn't dump Core even with -dverbose-core2core
endIteration :: DynFlags -> String -> DynFlag -> [CoreBind] -> [CoreRule] -> IO ()
endIteration = dumpAndLint Err.dumpIfSet_dyn

dumpAndLint :: (DynFlags -> DynFlag -> String -> SDoc -> IO ())
            -> DynFlags -> String -> DynFlag 
            -> [CoreBind] -> [CoreRule] -> IO ()
dumpAndLint dump dflags pass_name dump_flag binds rules
  = do {  -- Report result size if required
	  -- This has the side effect of forcing the intermediate to be evaluated
       ; Err.debugTraceMsg dflags 2 $
		(text "    Result size =" <+> int (coreBindsSize binds))

	-- Report verbosely, if required
       ; dump dflags dump_flag pass_name
              (pprCoreBindings binds $$ ppUnless (null rules) pp_rules)

	-- Type check
       ; lintCoreBindings dflags pass_name binds }
  where
    pp_rules = vcat [ blankLine
                    , ptext (sLit "------ Local rules for imported ids --------")
                    , pprRules rules ]
\end{code}


%************************************************************************
%*									*
             Monad and carried data structure definitions
%*									*
%************************************************************************

\begin{code}
newtype CoreState = CoreState {
        cs_uniq_supply :: UniqSupply
}

data CoreReader = CoreReader {
        cr_hsc_env :: HscEnv,
        cr_rule_base :: RuleBase,
        cr_module :: Module
}

data CoreWriter = CoreWriter {
        cw_simpl_count :: SimplCount
}

emptyWriter :: DynFlags -> CoreWriter
emptyWriter dflags = CoreWriter {
        cw_simpl_count = zeroSimplCount dflags
    }

plusWriter :: CoreWriter -> CoreWriter -> CoreWriter
plusWriter w1 w2 = CoreWriter {
        cw_simpl_count = (cw_simpl_count w1) `plusSimplCount` (cw_simpl_count w2)
    }

type CoreIOEnv = IOEnv CoreReader

-- | The monad used by Core-to-Core passes to access common state, register simplification
-- statistics and so on
newtype CoreM a = CoreM { unCoreM :: CoreState -> CoreIOEnv (a, CoreState, CoreWriter) }

instance Functor CoreM where
    fmap f ma = do
        a <- ma
        return (f a)

instance Monad CoreM where
    return x = CoreM (\s -> nop s x)
    mx >>= f = CoreM $ \s -> do
            (x, s', w1) <- unCoreM mx s
            (y, s'', w2) <- unCoreM (f x) s'
            return (y, s'', w1 `plusWriter` w2)

instance Applicative CoreM where
    pure = return
    (<*>) = ap

-- For use if the user has imported Control.Monad.Error from MTL
-- Requires UndecidableInstances
instance MonadPlus IO => MonadPlus CoreM where
    mzero = CoreM (const mzero)
    m `mplus` n = CoreM (\rs -> unCoreM m rs `mplus` unCoreM n rs)

instance MonadUnique CoreM where
    getUniqueSupplyM = do
        us <- getS cs_uniq_supply
        let (us1, us2) = splitUniqSupply us
        modifyS (\s -> s { cs_uniq_supply = us2 })
        return us1

runCoreM :: HscEnv
         -> RuleBase
         -> UniqSupply
         -> Module
         -> CoreM a
         -> IO (a, SimplCount)
runCoreM hsc_env rule_base us mod m =
        liftM extract $ runIOEnv reader $ unCoreM m state
  where
    reader = CoreReader {
            cr_hsc_env = hsc_env,
            cr_rule_base = rule_base,
            cr_module = mod
        }
    state = CoreState { 
            cs_uniq_supply = us
        }

    extract :: (a, CoreState, CoreWriter) -> (a, SimplCount)
    extract (value, _, writer) = (value, cw_simpl_count writer)

\end{code}


%************************************************************************
%*									*
             Core combinators, not exported
%*									*
%************************************************************************

\begin{code}

nop :: CoreState -> a -> CoreIOEnv (a, CoreState, CoreWriter)
nop s x = do
    r <- getEnv
    return (x, s, emptyWriter $ (hsc_dflags . cr_hsc_env) r)

read :: (CoreReader -> a) -> CoreM a
read f = CoreM (\s -> getEnv >>= (\r -> nop s (f r)))

getS :: (CoreState -> a) -> CoreM a
getS f = CoreM (\s -> nop s (f s))

modifyS :: (CoreState -> CoreState) -> CoreM ()
modifyS f = CoreM (\s -> nop (f s) ())

write :: CoreWriter -> CoreM ()
write w = CoreM (\s -> return ((), s, w))

\end{code}

\subsection{Lifting IO into the monad}

\begin{code}

-- | Lift an 'IOEnv' operation into 'CoreM'
liftIOEnv :: CoreIOEnv a -> CoreM a
liftIOEnv mx = CoreM (\s -> mx >>= (\x -> nop s x))

instance MonadIO CoreM where
    liftIO = liftIOEnv . IOEnv.liftIO

-- | Lift an 'IO' operation into 'CoreM' while consuming its 'SimplCount'
liftIOWithCount :: IO (SimplCount, a) -> CoreM a
liftIOWithCount what = liftIO what >>= (\(count, x) -> addSimplCount count >> return x)

\end{code}


%************************************************************************
%*									*
             Reader, writer and state accessors
%*									*
%************************************************************************

\begin{code}

getHscEnv :: CoreM HscEnv
getHscEnv = read cr_hsc_env

getRuleBase :: CoreM RuleBase
getRuleBase = read cr_rule_base

getModule :: CoreM Module
getModule = read cr_module

addSimplCount :: SimplCount -> CoreM ()
addSimplCount count = write (CoreWriter { cw_simpl_count = count })

-- Convenience accessors for useful fields of HscEnv

getDynFlags :: CoreM DynFlags
getDynFlags = fmap hsc_dflags getHscEnv

-- | The original name cache is the current mapping from 'Module' and
-- 'OccName' to a compiler-wide unique 'Name'
getOrigNameCache :: CoreM OrigNameCache
getOrigNameCache = do
    nameCacheRef <- fmap hsc_NC getHscEnv
    liftIO $ fmap nsNames $ readIORef nameCacheRef

\end{code}


%************************************************************************
%*									*
             Dealing with annotations
%*									*
%************************************************************************

\begin{code}
-- | Get all annotations of a given type. This happens lazily, that is
-- no deserialization will take place until the [a] is actually demanded and
-- the [a] can also be empty (the UniqFM is not filtered).
--
-- This should be done once at the start of a Core-to-Core pass that uses
-- annotations.
--
-- See Note [Annotations]
getAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (UniqFM [a])
getAnnotations deserialize guts = do
     hsc_env <- getHscEnv
     ann_env <- liftIO $ prepareAnnotations hsc_env (Just guts)
     return (deserializeAnns deserialize ann_env)

-- | Get at most one annotation of a given type per Unique.
getFirstAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (UniqFM a)
getFirstAnnotations deserialize guts
  = liftM (mapUFM head . filterUFM (not . null))
  $ getAnnotations deserialize guts
  
\end{code}

Note [Annotations]
~~~~~~~~~~~~~~~~~~
A Core-to-Core pass that wants to make use of annotations calls
getAnnotations or getFirstAnnotations at the beginning to obtain a UniqFM with
annotations of a specific type. This produces all annotations from interface
files read so far. However, annotations from interface files read during the
pass will not be visible until getAnnotations is called again. This is similar
to how rules work and probably isn't too bad.

The current implementation could be optimised a bit: when looking up
annotations for a thing from the HomePackageTable, we could search directly in
the module where the thing is defined rather than building one UniqFM which
contains all annotations we know of. This would work because annotations can
only be given to things defined in the same module. However, since we would
only want to deserialise every annotation once, we would have to build a cache
for every module in the HTP. In the end, it's probably not worth it as long as
we aren't using annotations heavily.

%************************************************************************
%*									*
                Direct screen output
%*									*
%************************************************************************

\begin{code}

msg :: (DynFlags -> SDoc -> IO ()) -> SDoc -> CoreM ()
msg how doc = do
        dflags <- getDynFlags
        liftIO $ how dflags doc

-- | Output a String message to the screen
putMsgS :: String -> CoreM ()
putMsgS = putMsg . text

-- | Output a message to the screen
putMsg :: SDoc -> CoreM ()
putMsg = msg Err.putMsg

-- | Output a string error to the screen
errorMsgS :: String -> CoreM ()
errorMsgS = errorMsg . text

-- | Output an error to the screen
errorMsg :: SDoc -> CoreM ()
errorMsg = msg Err.errorMsg

-- | Output a fatal string error to the screen. Note this does not by itself cause the compiler to die
fatalErrorMsgS :: String -> CoreM ()
fatalErrorMsgS = fatalErrorMsg . text

-- | Output a fatal error to the screen. Note this does not by itself cause the compiler to die
fatalErrorMsg :: SDoc -> CoreM ()
fatalErrorMsg = msg Err.fatalErrorMsg

-- | Output a string debugging message at verbosity level of @-v@ or higher
debugTraceMsgS :: String -> CoreM ()
debugTraceMsgS = debugTraceMsg . text

-- | Outputs a debugging message at verbosity level of @-v@ or higher
debugTraceMsg :: SDoc -> CoreM ()
debugTraceMsg = msg (flip Err.debugTraceMsg 3)

-- | Show some labelled 'SDoc' if a particular flag is set or at a verbosity level of @-v -ddump-most@ or higher
dumpIfSet_dyn :: DynFlag -> String -> SDoc -> CoreM ()
dumpIfSet_dyn flag str = msg (\dflags -> Err.dumpIfSet_dyn dflags flag str)
\end{code}

\begin{code}

initTcForLookup :: HscEnv -> TcM a -> IO a
initTcForLookup hsc_env = liftM (expectJust "initTcInteractive" . snd) . initTc hsc_env HsSrcFile False iNTERACTIVE

\end{code}


%************************************************************************
%*									*
               Finding TyThings
%*									*
%************************************************************************

\begin{code}
instance MonadThings CoreM where
    lookupThing name = do
        hsc_env <- getHscEnv
        liftIO $ initTcForLookup hsc_env (tcLookupGlobal name)
\end{code}

%************************************************************************
%*									*
               Template Haskell interoperability
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
-- | Attempt to convert a Template Haskell name to one that GHC can
-- understand. Original TH names such as those you get when you use
-- the @'foo@ syntax will be translated to their equivalent GHC name
-- exactly. Qualified or unqualifed TH names will be dynamically bound
-- to names in the module being compiled, if possible. Exact TH names
-- will be bound to the name they represent, exactly.
thNameToGhcName :: TH.Name -> CoreM (Maybe Name)
thNameToGhcName th_name = do
    hsc_env <- getHscEnv
    liftIO $ initTcForLookup hsc_env (lookupThName_maybe th_name)
#endif
\end{code}

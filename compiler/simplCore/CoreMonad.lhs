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
    getHscEnv, getAnnEnv, getRuleBase, getModule,
    getDynFlags, getOrigNameCache,
    
    -- ** Writing to the monad
    addSimplCount,
    
    -- ** Lifting into the monad
    liftIO, liftIOWithCount,
    liftIO1, liftIO2, liftIO3, liftIO4,
    
    -- ** Dealing with annotations
    findAnnotations, addAnnotation,
    
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
import PrelNames        ( iNTERACTIVE )
import HscTypes
import Module           ( Module )
import DynFlags         ( DynFlags, DynFlag )
import SimplMonad       ( SimplCount, plusSimplCount, zeroSimplCount )
import Rules            ( RuleBase )
import Annotations
import Serialized

import IOEnv hiding     ( liftIO, failM, failWithM )
import qualified IOEnv  ( liftIO )
import TcEnv            ( tcLookupGlobal )
import TcRnMonad        ( TcM, initTc )

import Outputable
import qualified ErrUtils as Err
import Maybes
import UniqSupply

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

\subsection{Monad and carried data structure definitions}

\begin{code}
data CoreState = CoreState {
        cs_uniq_supply :: UniqSupply,
        cs_ann_env :: AnnEnv
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
         -> AnnEnv
         -> RuleBase
         -> UniqSupply
         -> Module
         -> CoreM a
         -> IO (a, SimplCount)
runCoreM hsc_env ann_env rule_base us mod m =
        liftM extract $ runIOEnv reader $ unCoreM m state
  where
    reader = CoreReader {
            cr_hsc_env = hsc_env,
            cr_rule_base = rule_base,
            cr_module = mod
        }
    state = CoreState { 
            cs_uniq_supply = us,
            cs_ann_env = ann_env
        }

    extract :: (a, CoreState, CoreWriter) -> (a, SimplCount)
    extract (value, _, writer) = (value, cw_simpl_count writer)

\end{code}

\subsection{Core combinators, not exported}

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

\subsection{Reader, writer and state accessors}

\begin{code}

getHscEnv :: CoreM HscEnv
getHscEnv = read cr_hsc_env

getAnnEnv :: CoreM AnnEnv
getAnnEnv = getS cs_ann_env

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

\subsection{Dealing with annotations}

\begin{code}

-- | Find all the annotations we currently know about for the given target. Note that no
-- annotations will be returned if we haven't loaded information about the particular target
-- you are inquiring about: by default, only those modules that have been imported by the
-- program being compiled will have been loaded in this way.
--
-- To load the information from additional modules, you can use the functions 'DynamicLoading.forceLoadModuleInterfaces'
-- and 'DynamicLoading.forceLoadNameModuleInterface', but be aware that doing this indiscriminantly
-- will impose a performance penalty.
--
-- If no deserialization function is supplied, only transient annotations will be returned.
findAnnotations :: Typeable a => ([Word8] -> a) -> CoreAnnTarget -> CoreM [a]
findAnnotations deserialize target = do
     ann_env <- getAnnEnv
     return (findAnns deserialize ann_env target)

addAnnotation :: Typeable a => (a -> [Word8]) -> CoreAnnTarget -> a -> CoreM ()
addAnnotation serialize target what = addAnnotationToEnv $ Annotation { ann_target = target, ann_value = toSerialized serialize what }

addAnnotationToEnv :: Annotation -> CoreM ()
addAnnotationToEnv annotation = modifyS (\state -> state { cs_ann_env = extendAnnEnvList (cs_ann_env state) [annotation] })

\end{code}

\subsection{Direct screen output}

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

\subsection{Finding TyThings}

\begin{code}

instance MonadThings CoreM where
    lookupThing name = do
        hsc_env <- getHscEnv
        liftIO $ initTcForLookup hsc_env (tcLookupGlobal name)

\end{code}

\subsection{Template Haskell interoperability}

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

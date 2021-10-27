{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

-- | Do not use global variables!
--
-- Global variables are a hack. Do not use them if you can help it.
module GHC.Utils.GlobalVars
   ( v_unsafeHasPprDebug
   , v_unsafeHasNoDebugOutput
   , v_unsafeHasNoStateHack
   , unsafeHasPprDebug
   , unsafeHasNoDebugOutput
   , unsafeHasNoStateHack

   , global
   , consIORef
   , globalM
   , sharedGlobal
   , sharedGlobalM
   )
where

import GHC.Prelude

import GHC.Conc.Sync ( sharedCAF )

import System.IO.Unsafe
import Data.IORef
import Foreign (Ptr)

#define GLOBAL_VAR(name,value,ty)  \
{-# NOINLINE name #-};             \
name :: IORef (ty);                \
name = global (value);

#define GLOBAL_VAR_M(name,value,ty) \
{-# NOINLINE name #-};              \
name :: IORef (ty);                 \
name = globalM (value);


#define SHARED_GLOBAL_VAR(name,accessor,saccessor,value,ty) \
{-# NOINLINE name #-};                                      \
name :: IORef (ty);                                         \
name = sharedGlobal (value) (accessor);                     \
foreign import ccall unsafe saccessor                       \
  accessor :: Ptr (IORef a) -> IO (Ptr (IORef a));

#define SHARED_GLOBAL_VAR_M(name,accessor,saccessor,value,ty)  \
{-# NOINLINE name #-};                                         \
name :: IORef (ty);                                            \
name = sharedGlobalM (value) (accessor);                       \
foreign import ccall unsafe saccessor                          \
  accessor :: Ptr (IORef a) -> IO (Ptr (IORef a));



#if !MIN_VERSION_GLASGOW_HASKELL(9,3,0,0)

GLOBAL_VAR(v_unsafeHasPprDebug,      False, Bool)
GLOBAL_VAR(v_unsafeHasNoDebugOutput, False, Bool)
GLOBAL_VAR(v_unsafeHasNoStateHack,   False, Bool)

#else
SHARED_GLOBAL_VAR( v_unsafeHasPprDebug
                 , getOrSetLibHSghcGlobalHasPprDebug
                 , "getOrSetLibHSghcGlobalHasPprDebug"
                 , False
                 , Bool )
SHARED_GLOBAL_VAR( v_unsafeHasNoDebugOutput
                 , getOrSetLibHSghcGlobalHasNoDebugOutput
                 , "getOrSetLibHSghcGlobalHasNoDebugOutput"
                 , False
                 , Bool )
SHARED_GLOBAL_VAR( v_unsafeHasNoStateHack
                 , getOrSetLibHSghcGlobalHasNoStateHack
                 , "getOrSetLibHSghcGlobalHasNoStateHack"
                 , False
                 , Bool )
#endif

unsafeHasPprDebug :: Bool
unsafeHasPprDebug = unsafePerformIO $ readIORef v_unsafeHasPprDebug

unsafeHasNoDebugOutput :: Bool
unsafeHasNoDebugOutput = unsafePerformIO $ readIORef v_unsafeHasNoDebugOutput

unsafeHasNoStateHack :: Bool
unsafeHasNoStateHack = unsafePerformIO $ readIORef v_unsafeHasNoStateHack

{-
************************************************************************
*                                                                      *
                        Globals and the RTS
*                                                                      *
************************************************************************

When a plugin is loaded, it currently gets linked against a *newly
loaded* copy of the GHC package. This would not be a problem, except
that the new copy has its own mutable state that is not shared with
that state that has already been initialized by the original GHC
package.

(Note that if the GHC executable was dynamically linked this
wouldn't be a problem, because we could share the GHC library it
links to; this is only a problem if DYNAMIC_GHC_PROGRAMS=NO.)

The solution is to make use of @sharedCAF@ through @sharedGlobal@
for globals that are shared between multiple copies of ghc packages.
-}

-- Global variables:

global :: a -> IORef a
global a = unsafePerformIO (newIORef a)

consIORef :: IORef [a] -> a -> IO ()
consIORef var x =
  atomicModifyIORef' var (\xs -> (x:xs,()))

globalM :: IO a -> IORef a
globalM ma = unsafePerformIO (ma >>= newIORef)

-- Shared global variables:

sharedGlobal :: a -> (Ptr (IORef a) -> IO (Ptr (IORef a))) -> IORef a
sharedGlobal a get_or_set = unsafePerformIO $
  newIORef a >>= flip sharedCAF get_or_set

sharedGlobalM :: IO a -> (Ptr (IORef a) -> IO (Ptr (IORef a))) -> IORef a
sharedGlobalM ma get_or_set = unsafePerformIO $
  ma >>= newIORef >>= flip sharedCAF get_or_set

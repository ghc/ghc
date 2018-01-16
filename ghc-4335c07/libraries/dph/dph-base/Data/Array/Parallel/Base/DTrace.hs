{-# LANGUAGE ForeignFunctionInterface, CPP #-}

-- | Harness for DTrace.
module Data.Array.Parallel.Base.DTrace (
  traceLoopEntry, traceLoopExit,

  traceLoopST, traceLoopEntryST, traceLoopExitST,
  traceLoopIO, traceLoopEntryIO, traceLoopExitIO,

  traceFn, traceArg, traceF
) where

#ifdef DPH_ENABLE_DTRACE
import Foreign
import Foreign.C.Types
import Foreign.C.String
#endif

import GHC.ST ( ST )
import GHC.IO ( unsafeIOToST )

import Debug.Trace ( trace )

traceLoopST :: String -> ST s a -> ST s a
{-# INLINE traceLoopST #-}
traceLoopST s p = do
                    traceLoopEntryST s
                    x <- p
                    traceLoopExitST s
                    return x

traceLoopIO :: String -> IO a -> IO a
{-# INLINE traceLoopIO #-}
traceLoopIO s p = do
                    traceLoopEntryIO s
                    x <- p
                    traceLoopExitIO s
                    return x


traceLoopEntryST :: String -> ST s ()
traceLoopExitST  :: String -> ST s ()

traceLoopEntryIO :: String -> IO ()
traceLoopExitIO  :: String -> IO ()

traceLoopEntry :: String -> a -> a
traceLoopExit  :: String -> a -> a

#ifdef DPH_ENABLE_DTRACE

traceLoopEntry s x = unsafePerformIO (traceLoopEntryIO s >> return x)
traceLoopExit  s x = unsafePerformIO (traceLoopExitIO  s >> return x)

traceLoopEntryST s = unsafeIOToST (traceLoopEntryIO s)
traceLoopExitST  s = unsafeIOToST (traceLoopExitIO  s)

traceLoopEntryIO s = withCString s dph_loop_entry
traceLoopExitIO  s = withCString s dph_loop_exit

foreign import ccall safe dph_loop_entry :: Ptr CChar -> IO ()
foreign import ccall safe dph_loop_exit  :: Ptr CChar -> IO () 

#else

traceLoopEntry s x = x
traceLoopExit  s x = x

traceLoopEntryST s = return ()
traceLoopExitST  s = return ()

traceLoopEntryIO s = return ()
traceLoopExitIO  s = return ()

#endif


-- FIXME: make these use DTrace as well
traceFn :: String -> String -> a -> a
-- traceFn fn ty x = trace (fn ++ "<" ++ ty ++ ">") x `seq` trace ("DONE " ++ fn ++ "<" ++ ty ++ ">") x
traceFn _ _ x = x

traceArg :: Show a => String -> a -> b -> b
-- traceArg name arg x = trace ("    " ++ name ++ " = " ++ show arg) x
traceArg _ _ x = x

traceF :: String -> a -> a
-- traceF f x = trace f x `seq` trace ("DONE " ++ f) x
traceF _ x = x


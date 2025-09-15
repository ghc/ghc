{-# LANGUAGE ForeignFunctionInterface, MagicHash #-}
import GHC.ForeignPtr
import GHC.Ptr
import System.Mem

-- Don't use debugBelch() directly, because we cannot call varargs functions
-- using the FFI (doing so produces a segfault on 64-bit Linux, for example).
-- See Debug.Trace.traceIO, which also uses debugBelch2.
foreign import ccall "&debugBelch2" fun :: FunPtr (Ptr () -> Ptr () -> IO ())

new name = do
    p <- newForeignPtr_ (Ptr name)
    addForeignPtrFinalizerEnv fun (Ptr "finalizer 1 (%s)\n"#) p
    addForeignPtrFinalizerEnv fun (Ptr "finalizer 2 (%s)\n"#) p
    return p

main = do
    p <- new "p"#
    q <- new "q"#
    r <- new "r"#
    performGC -- collect p. finalizer order: 2, then 1.
--    print q
    touchForeignPtr q
    performGC -- collect q. finalizer order: 1, then 2.
              -- expected order: 2, then 1.
--    print r
    touchForeignPtr r
    performGC -- collect r. finalizer order: 2, then 1.

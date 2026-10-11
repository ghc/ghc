{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHC.Internal.Wasm.Prim.Exports (
  mkJSCallback,
  runIO,
  runNonIO,
  js_promiseResolveUnit,
  js_promiseResolveJSVal,
  js_promiseResolveChar,
  js_promiseResolveInt,
  js_promiseResolveInt8,
  js_promiseResolveInt16,
  js_promiseResolveInt32,
  js_promiseResolveInt64,
  js_promiseResolveWord,
  js_promiseResolveWord8,
  js_promiseResolveWord16,
  js_promiseResolveWord32,
  js_promiseResolveWord64,
  js_promiseResolvePtr,
  js_promiseResolveFunPtr,
  js_promiseResolveFloat,
  js_promiseResolveDouble,
  js_promiseResolveStablePtr,
  js_promiseResolveBool,
  js_promiseReject
) where

import GHC.Internal.Base
import GHC.Internal.Exception.Type
import GHC.Internal.IO
import GHC.Internal.IORef
import GHC.Internal.Int
import GHC.Internal.Prim (
    RealWorld, StablePtr#, State#, fork#, makeStablePtr#, mkWeakNoFinalizer#,
  )
import GHC.Internal.Stable
import GHC.Internal.TopHandler (flushStdHandles)
import GHC.Internal.Wasm.Prim.Types
import GHC.Internal.Word
import GHC.Internal.Unsafe.Coerce ( unsafeCoerce# )
import GHC.Internal.Ptr

mkJSCallback :: (StablePtr a -> IO JSVal) -> a -> IO JSVal
mkJSCallback adjustor f = do
  sp@(StablePtr sp#) <- newStablePtr f
  v@(JSVal p) <- adjustor sp
  IO $ \s0 -> case stg_setJSVALsp p sp# s0 of
    (# s1 #) -> (# s1, () #)
  fastcall js_callback_register v sp
  pure v

foreign import prim "stg_setJSVALsp"
  stg_setJSVALsp :: JSVal# -> StablePtr# a -> State# RealWorld -> (# State# RealWorld #)

foreign import javascript unsafe "__ghc_wasm_jsffi_finalization_registry.register($1, $2, $1)"
  js_callback_register :: JSVal -> StablePtr a -> IO ()

runIO :: (JSVal -> a -> IO ()) -> IO a -> IO JSVal
runIO res m = do
  p <- fastcall js_promiseWithResolvers
  let topHandler :: SomeException -> IO ()
      topHandler err = catch (realHandler err) topHandler
      realHandler :: SomeException -> IO ()
      realHandler (SomeException err) = do
        let tmp@(JSString tmp_v) = toJSString $ displayException err
        js_promiseReject p tmp
        freeJSVal tmp_v
  post_action_ref <- newIORef $ pure ()
  IO $ \s0 -> case fork# (unIO $ catch (res p =<< m) topHandler *> flushStdHandles *> join (readIORef post_action_ref)) s0 of
    (# s1, tso# #) -> case mkWeakNoFinalizer# tso# () s1 of
      (# s2, w# #) -> case makeStablePtr# w# s2 of
        (# s3, sp# #) -> case unIO (writeIORef post_action_ref $ fastcall js_promiseDelThrowTo p *> freeStablePtr (StablePtr $ unsafeCoerce# sp#)) s3 of
          (# s4, _ #) -> case unIO (fastcall js_promiseAddThrowTo p $ StablePtr $ unsafeCoerce# sp#) s4 of
            (# s5, _ #) -> case stg_scheduler_loop# s5 of
              (# s6, _ #) -> (# s6, p #)

runNonIO :: (JSVal -> a -> IO ()) -> a -> IO JSVal
runNonIO res a = runIO res $ pure a

foreign import javascript unsafe "let res, rej; const p = new Promise((resolve, reject) => { res = resolve; rej = reject; }); p.resolve = res; p.reject = rej; return p;"
  js_promiseWithResolvers :: IO JSVal

foreign import javascript unsafe "$1.throwTo = (err) => __exports.rts_promiseThrowTo($2, err);"
  js_promiseAddThrowTo :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.throwTo = () => {};"
  js_promiseDelThrowTo :: JSVal -> IO ()

foreign import prim "stg_scheduler_loopzh"
  stg_scheduler_loop# :: State# RealWorld -> (# State# RealWorld, () #)

-- Generated async export stubs pass these exported closures to runIO, so
-- select fastcall in the wrappers rather than at their generated call sites.
js_promiseResolveUnit :: JSVal -> () -> IO ()
js_promiseResolveUnit p _ = fastcall js_promiseResolveUnit' p

foreign import javascript unsafe "$1.resolve()"
  js_promiseResolveUnit' :: JSVal -> IO ()

-- Resolving a JSVal can synchronously invoke a user-defined @then@ getter,
-- so this import must retain the re-entrant calling convention. Promise
-- reactions for unit and primitive results, and for rejection, run later.
foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveJSVal :: JSVal -> JSVal -> IO ()

js_promiseResolveChar :: JSVal -> Char -> IO ()
js_promiseResolveChar = fastcall js_promiseResolveChar'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveChar' :: JSVal -> Char -> IO ()

js_promiseResolveInt :: JSVal -> Int -> IO ()
js_promiseResolveInt = fastcall js_promiseResolveInt'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt' :: JSVal -> Int -> IO ()

js_promiseResolveInt8 :: JSVal -> Int8 -> IO ()
js_promiseResolveInt8 = fastcall js_promiseResolveInt8'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt8' :: JSVal -> Int8 -> IO ()

js_promiseResolveInt16 :: JSVal -> Int16 -> IO ()
js_promiseResolveInt16 = fastcall js_promiseResolveInt16'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt16' :: JSVal -> Int16 -> IO ()

js_promiseResolveInt32 :: JSVal -> Int32 -> IO ()
js_promiseResolveInt32 = fastcall js_promiseResolveInt32'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt32' :: JSVal -> Int32 -> IO ()

js_promiseResolveInt64 :: JSVal -> Int64 -> IO ()
js_promiseResolveInt64 = fastcall js_promiseResolveInt64'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveInt64' :: JSVal -> Int64 -> IO ()

js_promiseResolveWord :: JSVal -> Word -> IO ()
js_promiseResolveWord = fastcall js_promiseResolveWord'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord' :: JSVal -> Word -> IO ()

js_promiseResolveWord8 :: JSVal -> Word8 -> IO ()
js_promiseResolveWord8 = fastcall js_promiseResolveWord8'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord8' :: JSVal -> Word8 -> IO ()

js_promiseResolveWord16 :: JSVal -> Word16 -> IO ()
js_promiseResolveWord16 = fastcall js_promiseResolveWord16'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord16' :: JSVal -> Word16 -> IO ()

js_promiseResolveWord32 :: JSVal -> Word32 -> IO ()
js_promiseResolveWord32 = fastcall js_promiseResolveWord32'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord32' :: JSVal -> Word32 -> IO ()

js_promiseResolveWord64 :: JSVal -> Word64 -> IO ()
js_promiseResolveWord64 = fastcall js_promiseResolveWord64'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveWord64' :: JSVal -> Word64 -> IO ()

js_promiseResolvePtr :: JSVal -> Ptr a -> IO ()
js_promiseResolvePtr = fastcall js_promiseResolvePtr'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolvePtr' :: JSVal -> Ptr a -> IO ()

js_promiseResolveFunPtr :: JSVal -> FunPtr a -> IO ()
js_promiseResolveFunPtr = fastcall js_promiseResolveFunPtr'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveFunPtr' :: JSVal -> FunPtr a -> IO ()

js_promiseResolveFloat :: JSVal -> Float -> IO ()
js_promiseResolveFloat = fastcall js_promiseResolveFloat'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveFloat' :: JSVal -> Float -> IO ()

js_promiseResolveDouble :: JSVal -> Double -> IO ()
js_promiseResolveDouble = fastcall js_promiseResolveDouble'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveDouble' :: JSVal -> Double -> IO ()

js_promiseResolveStablePtr :: JSVal -> StablePtr a -> IO ()
js_promiseResolveStablePtr = fastcall js_promiseResolveStablePtr'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveStablePtr' :: JSVal -> StablePtr a -> IO ()

js_promiseResolveBool :: JSVal -> Bool -> IO ()
js_promiseResolveBool = fastcall js_promiseResolveBool'

foreign import javascript unsafe "$1.resolve($2)"
  js_promiseResolveBool' :: JSVal -> Bool -> IO ()

js_promiseReject :: JSVal -> JSString -> IO ()
js_promiseReject = fastcall js_promiseReject'

foreign import javascript unsafe "$1.reject(new WebAssembly.RuntimeError($2))"
  js_promiseReject' :: JSVal -> JSString -> IO ()

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHC.Internal.Wasm.Prim.Imports (
  stg_blockPromise,
  stg_messagePromiseUnit,
  stg_messagePromiseJSVal,
  stg_messagePromiseChar,
  stg_messagePromiseInt,
  stg_messagePromiseInt8,
  stg_messagePromiseInt16,
  stg_messagePromiseInt32,
  stg_messagePromiseInt64,
  stg_messagePromiseWord,
  stg_messagePromiseWord8,
  stg_messagePromiseWord16,
  stg_messagePromiseWord32,
  stg_messagePromiseWord64,
  stg_messagePromisePtr,
  stg_messagePromiseFunPtr,
  stg_messagePromiseFloat,
  stg_messagePromiseDouble,
  stg_messagePromiseStablePtr,
  stg_messagePromiseBool
) where

import GHC.Internal.Base
import GHC.Internal.Exception
import GHC.Internal.Exts
import GHC.Internal.IO.Unsafe
import GHC.Internal.Stable
import GHC.Internal.Wasm.Prim.Types

{-# OPAQUE raiseJSException #-}
raiseJSException :: JSVal -> a
raiseJSException v = throw $ JSException v

{-

Note [stg_blockPromise]
~~~~~~~~~~~~~~~~~~~~~~~

When desugaring a JSFFI async import, we first emit a sync import: it
returns a JSVal that represents a Promise. Now we need to wrap it in a
thunk with the same return type as the user written import, so that
when the thunk is forced, the thread will be suspended and only
resumed later when the Promise fulfills. This is done by
stg_blockPromise.

stg_blockPromise takes two arguments: the Promise, and a "message
promise" function which is a JSFFI sync import that sends a message to
the Promise via invoking promise.then(). When the Promise resolves
with a result, the callback passed in .then() invokes RTS API which
needs to box the JavaScript result with the correct rts_mk* function,
so for each possible return type, we need to define a distinct
"message promise" function. This is an implementation detail, not end
user's concern, the desugar logic picks the right one to be passed to
stg_blockPromise.

Once the thunk is forced, we first check if we're inside a C FFI
export's main thread and if so, throw WouldBlockException. Then we pin
the current TSO via a stable pointer and call the "message promise"
function. At this point, the Promise fulfill logic that resumes the
thread in the future has been set up, we can drop the Promise eagerly,
then arrange the current thread to block.

Blocking is done by readMVar. stg_blockPromise allocates an empty MVar
and pins it under a stable pointer, then finally blocks by readMVar.
The stable pointer is captured in the promise.then callback. When the
Promise is settled in the future, the promise.then callback writes the
result (or exception) to the MVar and then resumes Haskell execution.

-}

stg_blockPromise :: String -> JSVal -> (JSVal -> StablePtr Any -> IO ()) -> r
stg_blockPromise err_msg p msg_p = unsafeDupablePerformIO $ IO $ \s0 ->
  case stg_jsffi_check (unsafeCoerce# $ toException $ WouldBlockException err_msg) s0 of
    (# s1 #) -> case newMVar# s1 of
      (# s2, mv# #) -> case makeStablePtr# mv# s2 of
        (# s3, sp #) ->
          case unIO (msg_p p $ StablePtr $ unsafeCoerce# sp) s3 of
            -- Since we eagerly free the Promise here, we must return
            -- an updatable thunk in stg_blockPromise that can't be
            -- evaluated more than once, regardless of optimization
            -- level, otherwise runtime panic may happen. This
            -- property holds because:
            -- 1. We're using the single threaded RTS
            -- 2. Once the thunk is evaluated the first time and that
            --    thread is paused, lazy blackholing does happen
            -- 3. unsafeDupablePerformIO applies lazy to the result
            --    and prevents dmdanal from being naughty
            (# s4, _ #) -> case unIO (freeJSVal p) s4 of
              (# s5, _ #) ->
                -- raiseJSException_closure is used by the RTS in case
                -- the Promise is rejected, and it is likely a CAF. So
                -- we need to keep it alive when we block waiting for
                -- the Promise to resolve or reject, and also mark it
                -- as OPAQUE just to be sure.
                keepAlive# raiseJSException s5 $
                  readMVar# mv#

foreign import prim "stg_jsffi_check"
  stg_jsffi_check :: Any -> State# RealWorld -> (# State# RealWorld #)

foreign import javascript unsafe "$1.then(() => __exports.rts_promiseResolveUnit($2), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseUnit :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveJSVal($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseJSVal :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveChar($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseChar :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveInt($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseInt :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveInt8($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseInt8 :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveInt16($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseInt16 :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveInt32($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseInt32 :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveInt64($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseInt64 :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveWord($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseWord :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveWord8($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseWord8 :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveWord16($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseWord16 :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveWord32($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseWord32 :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveWord64($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseWord64 :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolvePtr($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromisePtr :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveFunPtr($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseFunPtr :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveFloat($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseFloat :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveDouble($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseDouble :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveStablePtr($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseStablePtr :: JSVal -> StablePtr Any -> IO ()

foreign import javascript unsafe "$1.then(res => __exports.rts_promiseResolveBool($2, res), err => __exports.rts_promiseReject($2, err))"
  stg_messagePromiseBool :: JSVal -> StablePtr Any -> IO ()

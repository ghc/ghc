{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnliftedNewtypes #-}

module GHC.Internal.Wasm.Prim.Types (
  JSVal# (..),
  JSVal (..),
  freeJSVal,
  mkWeakJSVal,
  JSString (..),
  fromJSString,
  toJSString,
  JSException (..),
  WouldBlockException (..)
) where

import GHC.Internal.Base
import GHC.Internal.Exception.Type
import GHC.Internal.Exts
import GHC.Internal.Foreign.C.String.Encoding
import GHC.Internal.ForeignPtr
import GHC.Internal.IO
import GHC.Internal.IO.Encoding
import GHC.Internal.Num
import GHC.Internal.Show
import GHC.Internal.Weak

{-

Note [JSVal representation for wasm]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On wasm, the Haskell heap lives in the linear memory space, and it can
only contain bit patterns, not opaque references of the host
JavaScript heap. As long as we have two heaps that coexist in this
way, the best we can do is representing JavaScript references as
ids in the Haskell heap.

First, we have the unlifted JSVal# type, defined in jsval.cmm with the
following memory layout:

+--------------+-----+----+----------+
|stg_JSVAL_info|Weak#|Int#|StablePtr#|
+--------------+-----+----+----------+

The first non-pointer Int# field is a 32-bit id allocated and
returned by the JSValManager on the JavaScript side. The JSValManager
maintains a Map from ids to actual JavaScript values. This field is
immutable throughout a JSVal# closure's lifetime and is unique for
each JSVal# ever created.

The Weak# poiner sets the JSVal# closure as key and has a C finalizer
that drops the mapping in JSValManager. When the JSVal# closure is
garbage collected, the finalizer is invoked, but it can also be
eagerly invoked by freeJSVal, that's why we carry the Weak# in JSVal#
as a pointer field.

Normally, one JSVal# manage one kind of resource: the JavaScript value
retained in JSValManager. However, in case of JSFFI exports where we
convert Haskell functions to JavaScript callbacks, the JSVal# manages
not only the callback on the JavaScript side, but also a stable
pointer that pins the exported function on the Haskell side. That
StablePtr# is recorded in the JSVal# closure.

Even if the JSVal# closure is garbage collected, we don't know if the
JavaScript side still retains the callback somewhere other than
JSValManager, so the stable pointer will continue to pin the Haskell
function closure. We do a best effort cleanup on the JavaScript side
by using a FinalizationRegistry: if the JSVal# is automatically
collected, the callback is dropped in JSValManager and also not used
elsewhere, the FinalizationRegistry calls into the RTS to drop the
stable pointer as well.

However, JSVal# can be eagerly freed by freeJSVal. It'll deregister
the callback in the FinalizationRegistry, finalize the Weak# pointer
and also free the stable pointer. In order to make freeJSVal
idempotent, we must not free the stable pointer twice; therefore the
StablePtr# field is mutable and will be overwritten with NULL upon
first freeJSVal invocation; it's also NULL upon creation by
rts_mkJSVal and later overwritten with the StablePtr# upon the
callback creation.

On top of JSVal#, we have the user-facing lifted JSVal type, which
wraps the JSVal#. The lifted JSVal type is meant to be an abstract
type. Its creation and consumption is mainly handled by the RTS API
functions rts_mkJSVal and rts_getJSVal, which are used in C stub files
generated when desugaring JSFFI foreign imports/exports.

-}

newtype JSVal#
  = JSVal# (Any :: UnliftedType)

-- | A 'JSVal' is a first-class Haskell value on the Haskell heap that
-- represents a JavaScript value. You can use 'JSVal' or its @newtype@
-- as a supported argument or result type in JSFFI import & export
-- declarations, in addition to those lifted FFI types like 'Int' or
-- 'Ptr' that's already supported by C FFI. It is garbage collected by
-- the GHC RTS:
--
-- * There can be different 'JSVal's that point to the same JavaScript
--   value. As long as there's at least one 'JSVal' still alive on the
--   Haskell heap, that JavaScript value will still be alive on the
--   JavaScript heap.
-- * If there's no longer any live 'JSVal' that points to the
--   JavaScript value, after Haskell garbage collection, the
--   JavaScript runtime will be able to eventually garbage collect
--   that JavaScript value as well.
--
-- There's a special kind of 'JSVal' that represents a JavaScript
-- callback exported from a Haskell function like this:
--
-- > foreign import javascript "wrapper"
-- >   exportFibAsAsyncJSCallback :: (Int -> Int) -> IO JSVal
--
-- Such a 'JSVal' manages an additional kind of resource: the exported
-- Haskell function closure. Even if it is automatically garbage
-- collected, the Haskell function closure would still be retained
-- since the JavaScript callback might be retained elsewhere. We do a
-- best-effort collection here using JavaScript
-- [@FinalizationRegistry@](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/FinalizationRegistry),
-- so the Haskell function closure might be eventually dropped if the
-- JavaScript callback is garbage collected.
--
-- Note that even the @FinalizationRegistry@ logic can't break cyclic
-- references between the Haskell/JavaScript heap: when an exported
-- Haskell function closure retains a 'JSVal' that represents a
-- JavaScript callback. Though this can be solved by explicit
-- 'freeJSVal' calls.
data JSVal
  = JSVal JSVal#

-- | 'freeJSVal' eagerly frees a 'JSVal' in the runtime. It drops the
-- retained JavaScript value on the JavaScript side, and in case of a
-- 'JSVal' that represents a callback, also drops the retained Haskell
-- function closure. Once a 'JSVal' is freed by 'freeJSVal', later
-- attempts to pass it to the JavaScript side would result in runtime
-- crashes, so you should only call 'freeJSVal' when you're confident
-- that 'JSVal' won't be used again (and in case of callbacks, that
-- callback won't be invoked again).
--
-- 'freeJSVal' is idempotent: it's safe to call it more than once on
-- the same 'JSVal', subsequent invocations are no-ops. You are
-- strongly recommended to call 'freeJSVal' on short-lived
-- intermediate 'JSVal' values for timely release of resources!
{-# INLINE freeJSVal #-}
freeJSVal :: JSVal -> IO ()
freeJSVal v@(JSVal p) = do
  js_callback_unregister v
  IO $ \s0 -> case stg_freeJSVal# p s0 of
    (# s1, _, _ #) -> (# s1, () #)

-- | 'mkWeakJSVal' allows you to create a 'Weak' pointer that observes
-- the liveliness of a 'JSVal' closure on the Haskell heap and
-- optionally attach a finalizer.
--
-- Note that this liveliness is not affected by 'freeJSVal': even if
-- 'freeJSVal' is called, the 'JSVal' might still be alive on the
-- Haskell heap as a dangling reference and 'deRefWeak' might still be
-- able to retrieve the 'JSVal' before it is garbage collected.
mkWeakJSVal :: JSVal -> Maybe (IO ()) -> IO (Weak JSVal)
mkWeakJSVal v@(JSVal p) (Just (IO fin)) = IO $ \s0 ->
  case mkWeak# p v fin s0 of
    (# s1, w #) -> (# s1, Weak w #)
mkWeakJSVal v@(JSVal p) Nothing = IO $ \s0 ->
  case mkWeakNoFinalizer# p v s0 of
    (# s1, w #) -> (# s1, Weak w #)

foreign import prim "stg_freeJSVAL"
  stg_freeJSVal# :: JSVal# -> State# RealWorld -> (# State# RealWorld, Int#, State# RealWorld -> (# State# RealWorld, b #) #)

foreign import javascript unsafe "try { __ghc_wasm_jsffi_finalization_registry.unregister($1); } catch {}"
  js_callback_unregister :: JSVal -> IO ()

-- | A 'JSString' represents a JavaScript string.
newtype JSString
  = JSString JSVal

-- | Converts a 'JSString' to a Haskell 'String'. Conversion is done
-- eagerly once the resulting 'String' is forced, and the argument
-- 'JSString' may be explicitly freed if no longer used.
fromJSString :: JSString -> String
fromJSString s = unsafeDupablePerformIO $ do
  l <- js_stringLength s
  fp <- mallocPlainForeignPtrBytes $ l * 3
  withForeignPtr fp $ \buf -> do
    l' <- js_encodeInto s buf $ l * 3
    peekCStringLen utf8 (buf, l')

foreign import javascript unsafe "$1.length"
  js_stringLength :: JSString -> IO Int

foreign import javascript unsafe "(new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written"
  js_encodeInto :: JSString -> Ptr a -> Int -> IO Int

-- | Converts a Haskell 'String' to a 'JSString'.
toJSString :: String -> JSString
toJSString s = unsafeDupablePerformIO $ withCStringLen utf8 s $ \(buf, len) -> js_toJSString buf len

foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_toJSString :: Ptr a -> Int -> IO JSString

-- | A 'JSException' represents a JavaScript exception. It is likely
-- but not guaranteed to be an instance of the @Error@ class. When you
-- call an async JSFFI import and the result @Promise@ rejected, the
-- rejection value will be wrapped in a 'JSException' and re-thrown in
-- Haskell once you force the result.
newtype JSException
  = JSException JSVal

-- | If the
-- [@error.stack@](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/stack)
-- property is present, it will be used to render the 'Show' instance
-- output so you can see the JavaScript stack trace.
instance Show JSException where
  showsPrec p e =
    showParen (p >= 11) $ showString "JSException " . showsPrec 11 (jsErrorString e)

jsErrorString :: JSException -> String
jsErrorString e = unsafeDupablePerformIO $ do
  s@(JSString v) <- js_errorToString e
  r <- evaluate $ fromJSString s
  freeJSVal v
  pure r

foreign import javascript unsafe "`${$1.stack ? $1.stack : $1}`"
  js_errorToString :: JSException -> IO JSString

instance Exception JSException

-- | An async JSFFI import returns a thunk that represents a pending
-- JavaScript @Promise@:
--
-- > foreign import javascript "(await fetch($1)).text()"
-- >   js_fetch :: JSString -> IO JSString
--
-- Forcing that thunk blocks the current Haskell thread until the
-- @Promise@ is fulfilled, but that cannot happen if the Haskell
-- thread is a bound thread created by a JSFFI sync export or a C FFI
-- export! Those Haskell computations are meant to return
-- synchronously, but JavaScript asynchronocity is contagious and
-- there's no escape hatch like @unsafeAwaitPromise@.
--
-- In such cases, a 'WouldBlockException' exception would be thrown.
-- The 'WouldBlockException' is attached with a diagnostic message
-- generated at compile-time (currently just the JSFFI source snippet
-- of the corresponding async import) to help debugging the
-- exception's cause.
newtype WouldBlockException
  = WouldBlockException String
  deriving (Show)

instance Exception WouldBlockException

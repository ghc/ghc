{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
  WouldBlockException (..),
  PromisePendingException (..)
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
import GHC.Internal.Stable
import GHC.Internal.Weak

{-

Note [JSVal representation for wasm]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On wasm, the Haskell heap lives in the linear memory space, and it can
only contain bit patterns, not opaque references of the host
JavaScript heap. As long as we have two heaps that coexist in this
way, the best we can do is representing JavaScript references as
unique ids in the Haskell heap.

In JavaScript, we have a JSValManager which exposes some interfaces as
wasm imports. The JSValManager is in charge of allocating unique ids
and managing the mapping from ids to the actual JavaScript values. In
fact we can implement the entire JSValManager in wasm, using a wasm
table with externref elements to hold the JavaScript values and a
special allocator to manage free slots in the table. That'll take more
work to implement though, with one more caveat: browsers typically
limit max wasm table size to 10000000 which may not be large enough
for some use cases. We can workaround the table size restriction by
managing a pool or tree of wasm tables, but at this point we really
should ditch the idea of doing everything in wasm just because we can.

Next, we have the unlifted JSVal# type, defined in jsval.cmm and
contains one non-pointer word which is the id allocated by
JSValManager. On top of JSVal#, we have the user-facing lifted JSVal
type, which carries the JSVal#, as well as a weak pointer and a stable
pointer.

The weak pointer is used to garbage collect JSVals. Its key is the
JSVal# closure, and it has a C finalizer that tells the JSValManager
to drop the mapping when the JSVal# closure is collected. Since we
want to provide freeJSVal to allow eager freeing of JSVals, we need to
carry it as a field of JSVal.

The stable pointer field is NULL for normal JSVals created via foreign
import results or foreign export arguments. But for JSFFI dynamic
exports that wraps a Haskell function closure as a JavaScript callback
and returns that callback's JSVal, it is a stable pointer that pins
that Haskell function closure. If this JSVal is garbage collected,
then we can only rely on a JavaScript FinalizerRegistry to free the
stable pointer in the future, but if we eagerly free the callback with
freeJSVal, then we can eagerly free this stable pointer as well.

The lifted JSVal type is meant to be an abstract type. Its creation
and consumption is mainly handled by the RTS API functions rts_mkJSVal
and rts_getJSVal, which are used in C stub files generated when
desugaring JSFFI foreign imports/exports.

-}

newtype JSVal#
  = JSVal# (Any :: UnliftedType)

data JSVal
  = forall a . JSVal JSVal# (Weak# JSVal) (StablePtr# a)

freeJSVal :: JSVal -> IO ()
freeJSVal v@(JSVal _ w sp) = do
  case sp `eqStablePtr#` unsafeCoerce# nullAddr# of
    0# -> do
      js_callback_unregister v
      freeStablePtr $ StablePtr sp
    _ -> pure ()
  IO $ \s0 -> case finalizeWeak# w s0 of
    (# s1, _, _ #) -> (# s1, () #)

mkWeakJSVal :: JSVal -> Maybe (IO ()) -> IO (Weak JSVal)
mkWeakJSVal v@(JSVal k _ _) (Just (IO fin)) = IO $ \s0 ->
  case mkWeak# k v fin s0 of
    (# s1, w #) -> (# s1, Weak w #)
mkWeakJSVal (JSVal _ w _) Nothing = pure $ Weak w

foreign import javascript unsafe "if (!__ghc_wasm_jsffi_finalization_registry.unregister($1)) { throw new WebAssembly.RuntimeError('js_callback_unregister'); }"
  js_callback_unregister :: JSVal -> IO ()

newtype JSString
  = JSString JSVal

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

toJSString :: String -> JSString
toJSString s = unsafeDupablePerformIO $ withCStringLen utf8 s $ \(buf, len) -> js_toJSString buf len

foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_toJSString :: Ptr a -> Int -> IO JSString

newtype JSException
  = JSException JSVal

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

newtype WouldBlockException
  = WouldBlockException String
  deriving (Show)

instance Exception WouldBlockException

data PromisePendingException
  = PromisePendingException
  deriving (Show)

instance Exception PromisePendingException

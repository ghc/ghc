{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module GHC.JS.Prim
    ( JSVal(..), JSVal#
    , JSException(..)
    , WouldBlockException(..)
#if defined(javascript_HOST_ARCH)
    , toIO
    , resolve
    , resolveIO
    , mkJSException
    , fromJSString
    , toJSString
    , toJSArray
    , fromJSArray
    , fromJSInt
    , toJSInt
    , isNull
    , isUndefined
    , jsNull
    , getProp
    , getProp'
    , getProp#
    , unsafeGetProp
    , unsafeGetProp'
    , unsafeGetProp#
    , unpackJSString#
    , unpackJSStringUtf8#
    , unsafeUnpackJSString#
    , unsafeUnpackJSStringUtf8#
    , unpackJSStringUtf8##
    , unsafeUnpackJSStringUtf8##
#endif
    ) where

import GHC.Internal.JS.Prim


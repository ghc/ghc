{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module GHC.Utils.Json where

import GHC.Prelude

import GHC.Utils.Outputable
import Data.Char
import Numeric

-- | Simple data type to represent JSON documents.
data JsonDoc where
  JSNull :: JsonDoc
  JSBool :: Bool -> JsonDoc
  JSInt  :: Int  -> JsonDoc
  JSString :: String -> JsonDoc
  JSArray :: [JsonDoc] -> JsonDoc
  JSObject :: [(String, JsonDoc)] -> JsonDoc


-- This is simple and slow as it is only used for error reporting
renderJSON :: JsonDoc -> SDoc
renderJSON d =
  case d of
    JSNull -> text "null"
    JSBool b -> text $ if b then "true" else "false"
    JSInt    n -> ppr n
    JSString s -> doubleQuotes $ text $ escapeJsonString s
    JSArray as -> brackets $ pprList renderJSON as
    JSObject fs -> braces $ pprList renderField fs
  where
    renderField :: (String, JsonDoc) -> SDoc
    renderField (s, j) = doubleQuotes (text s) <>  colon <> renderJSON j

    pprList pp xs = hcat (punctuate comma (map pp xs))

escapeJsonString :: String -> String
escapeJsonString = concatMap escapeChar
  where
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar '"'  = "\\\""
    escapeChar '\\'  = "\\\\"
    escapeChar c | isControl c || fromEnum c >= 0x7f  = uni_esc c
    escapeChar c = [c]

    uni_esc c = "\\u" ++ (pad 4 (showHex (fromEnum c) ""))

    pad n cs  | len < n   = replicate (n-len) '0' ++ cs
                          | otherwise = cs
                                   where len = length cs

class ToJson a where
  json :: a -> JsonDoc

instance ToJson String where
  json = JSString . escapeJsonString

instance ToJson Int where
  json = JSInt

-- This test (#24984) made the constraint solver do lots of kick-out
-- which (because of the lack of sharing in constraints) eventually
-- build an exponentiallyy-sized coercion.

{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE ImpredicativeTypes #-}  -- ImpredicativeTypes is vital to trigger the perf problem
module Standalone where

import Data.Proxy
import Data.String ( IsString, fromString )
import Data.Kind  ( Type )

newtype B dst a = B { unB :: a }
  deriving (Eq, Show)

type family IsCustomSink dst where
  IsCustomSink _ = 'False

class IsCustomSink dst ~ flag => InterpSink (flag :: Bool) dst where
  type Builder flag dst :: Type

  ofString :: Proxy flag -> String -> B dst (Builder flag dst)
  build    :: Proxy flag -> B dst (Builder flag dst) -> B dst (Builder flag dst) -> B dst (Builder flag dst)
  finalize :: Proxy flag -> B dst (Builder flag dst) -> dst

instance (IsString str) => InterpSink 'False str where
  type Builder 'False str = ShowS

  ofString _ = B . showString
  build _ (B f) (B g) = B $ f . g
  finalize _ = fromString . ($ "") . unB


-- Testcase

x :: Bool
x = True

-- You can scale this test easy by deleting some of the
-- duplicate lines, and removing some trailing parens

simplest :: String
simplest =
    finalize Proxy
      (build Proxy (ofString Proxy "1")
      (build Proxy (ofString Proxy "2")
      (build Proxy (ofString Proxy "3")
      (build Proxy (ofString Proxy "4")
      (build Proxy (ofString Proxy "2")
      (build Proxy (ofString Proxy "3")
      (build Proxy (ofString Proxy "4")
      (build Proxy (ofString Proxy "5")
      (build Proxy (ofString Proxy "6")
      (build Proxy (ofString Proxy "7")
      (build Proxy (ofString Proxy "8")
      (build Proxy (ofString Proxy "9")
      (build Proxy (ofString Proxy "a")
      (build Proxy (ofString Proxy "b")
      (build Proxy (ofString Proxy "c")
      (build Proxy (ofString Proxy "d")   -- 300k coercions
      (build Proxy (ofString Proxy "e")   -- 600k coercions
      (build Proxy (ofString Proxy "f")   -- 1.3M coercions
                   (ofString Proxy "")))))))))))))))))))


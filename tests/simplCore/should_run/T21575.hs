{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- 0 => use unsafeCoerce
-- 1 => use withDict
#define WITH_DICT 1

module Main (main) where

import Control.Monad (unless)
import qualified Data.Map as M
import Data.Map (Map)

#if WITH_DICT
import GHC.Exts (withDict)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

main :: IO ()
main = do
  testCase (give Normal (toJSON (Foo Bar)))
           (Object (M.fromList [("Foo",String "Bar")]))
  testCase (give ViaShow (toJSON (Foo Bar)))
           (Object (M.fromList [("Foo",String "SHOWBAR")]))
  putStrLn "All tests passed!"

{-
toJSONBar :: Given Style => Bar -> Value

  give Normal (\gd -> toJSONBar gd e)
  --> withDict @(Given Style) @Style Normal (toJSON e)
  --> toJSONBar ((Normal |> co) :: Given Style) e

  give Normal (\gd -> toJSONBar gd e')
  --> toJSONBar ((ViaShow |> co) :: Given Style) e'

--------- With new cast ------------

  give Normal (\gd -> toJSONBar gd e)
  --> withDict @(Given Style) @Style Normal (\gd -> toJSONBar gd e)
  --> ((\gd -> toJSONBar gd e) |> co) Normal
  --> (\gd' -> toJSonBar (gd' |> sym (co[1])) e) Normal
  --> toJSONBar (Normal |> co') e   -- Boo!

-}

testCase :: (Eq a, Show a) => a -> a -> IO ()
testCase expected actual =
  unless (expected == actual) $
    error $ unlines
      [ ""
      , "Expected: " ++ show expected
      , "Actual:   " ++ show actual
      ]

class Given a where
  given :: a

give :: forall a r. a -> (Given a => r) -> r
#if WITH_DICT
give = withDict @(Given a) @a
#else
give a k = unsafeCoerce (Gift k :: Gift a r) a

newtype Gift a r = Gift (Given a => r)
#endif

data Foo = Foo Bar

instance Show Foo where
  show _ = "SHOWFOO"

data Bar = Bar | BarBar

instance Show Bar where
  show _ = "SHOWBAR"

----------------------------------------------------------------------------
-- ToJSON instances
----------------------------------------------------------------------------

instance Given Style => ToJSON Foo where
  toJSON (Foo x) = Object $ M.singleton "Foo" (toJSON x)

instance Given Style => ToJSON Bar where
  toJSON x = case given of
    Normal -> String $ case x of
                Bar    -> "Bar"
                BarBar -> "BarBar"
    ViaShow -> String $ show x

data Style = Normal | ViaShow

----------------------------------------------------------------------------
-- Minimized aeson
----------------------------------------------------------------------------

class ToJSON a where
  toJSON :: a -> Value

data Value
  = Object !(Map String Value)
  | String !String
  deriving (Eq, Show)

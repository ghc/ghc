{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.Map as M
import Data.Map (Map)
import T21575b_aux (Given(..), give, unsafeGive)

main :: IO ()
main = do
  testCase "Normal"
    (give Normal (toJSON (Foo Bar)))
    (unsafeGive Normal (toJSON (Foo Bar)))
    (Object (M.fromList [("Foo",String "Bar")]))
  testCase "ViaShow"
    (give ViaShow (toJSON (Foo Bar)))
    (unsafeGive ViaShow (toJSON (Foo Bar)))
    (Object (M.fromList [("Foo",String "SHOWBAR")]))

-----

testCase :: (Eq a, Show a) => String -> a -> a -> a -> IO ()
testCase str with_give with_unsafeGive expected =
  putStrLn $ unlines
      [ str ++ ":"
      , "    withDict: " ++ show with_give
      , "unsafeCoerce: " ++ show with_unsafeGive
      , "    expected: " ++ show expected
      , ""
      ]

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

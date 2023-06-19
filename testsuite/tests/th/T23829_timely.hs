{-# LANGUAGE TemplateHaskellQuotes, TypeApplications #-}
module T99999_timely (main) where

import Language.Haskell.TH
import Data.Proxy

timely :: IO Type
timely = [t| forall a. a |]

type Foo = Int

timely_top :: IO Type
timely_top = [t| Foo |]

main :: IO ()
main = do
    timely >>= print
    timely_top >>= print
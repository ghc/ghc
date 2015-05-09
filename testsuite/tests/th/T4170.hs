{-# LANGUAGE TemplateHaskell #-}
module T4170 where

import Language.Haskell.TH

class LOL a

lol :: Q [Dec]
lol = [d|
    instance LOL Int
    |]

instance LOL Int

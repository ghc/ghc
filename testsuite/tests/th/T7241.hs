{-# LANGUAGE TemplateHaskell #-}

module T7241 where

import Language.Haskell.TH

$(newName "Foo" >>= \o -> return [DataD [] o [] [RecC o []] []])

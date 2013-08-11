{-# LANGUAGE TemplateHaskell, TypeFamilies, PolyKinds, DataKinds #-}

module ClosedFam1 where

import Language.Haskell.TH

$(do { decl <- [d| type family Foo a (b :: k) where
                     Foo Int Bool = Int
                     Foo a Maybe = Bool
                     Foo b (x :: Bool) = Char |]
     ; reportWarning (pprint decl)
     ; return [] })


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T12387a where

import GHC.Generics
import Language.Haskell.TH.Lib

data Foo = Foo

$(do d <- instanceD (cxt []) (conT ''Eq `appT` conT ''Foo)
            [tySynInstD $ tySynEqn Nothing (conT ''Rep `appT` conT ''Foo) (conT ''Maybe)]
     return [d])

main :: IO ()
main = print $ Foo == Foo

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module TypeToName where

import Language.Haskell.TH.Syntax
import Data.Proxy

main :: IO $(return $ ConT $ typeConstructorName (Proxy @()) )
main = return ()

foo :: IO $(return $ ConT $ typeConstructorName (Proxy @Int) )
foo = return 0

qux :: IO ($(return $ ConT $ typeConstructorName (Proxy @(Int, Int) )) Int Int)
qux = return (3, 4)


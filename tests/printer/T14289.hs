{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}

import Language.Haskell.TH

class C a b

$([d| data Foo a = Foo a deriving (C a) |])

{-

Note: to debug

~/inplace/bin/ghc-stage2 --interactive
load the following
----------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}

import Language.Haskell.TH

class C a b

main :: IO ()
main = putStrLn $([d| data Foo a = Foo a deriving (C a) |] >>= stringE . show)

----------------------------------------

-}

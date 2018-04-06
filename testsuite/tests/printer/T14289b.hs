{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}

import Language.Haskell.TH

class (a `C` b) c

$([d| data Foo a = Foo a deriving (y `C` z) |])

{-

Note: to debug

~/inplace/bin/ghc-stage2 --interactive
load the following
----------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}

import Language.Haskell.TH

class (a `C` b) c

main :: IO ()
main
  = putStrLn $([d| data Foo a = Foo a deriving (y `C` z) |] >>= stringE . show)

----------------------------------------
Bceomes


[DataD [] Foo_0 [PlainTV a_2] Nothing
  [NormalC Foo_1 [(Bang NoSourceUnpackedness NoSourceStrictness,VarT a_2)]]
  [DerivClause Nothing
      [AppT (AppT (ConT Main.C) (VarT y_6989586621679027885))
            (VarT z_6989586621679027886)]]]

-}

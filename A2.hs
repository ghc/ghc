{-# LANGUAGE TemplateHaskell #-}
module Test where

import Language.Haskell.TH

type Code a = Q (TExp a)

data Foo = Foo Int

from :: LiftT r => Code Foo -> (Code Int -> Code r) -> Code r
from c k =
  [|| case $$c of Foo is -> $$(k [|| is ||]) ||]

test :: Code Foo -> Code Foo -> Code (Int, Int)
test c1 c2 =
  from c1 $ \ a1 -> from c2 $ \ a2 -> [|| ($$a1, $$a2) ||]

testUse :: (Int, Int)
testUse = $$(test [|| Foo 1 ||] [|| Foo 2 ||])

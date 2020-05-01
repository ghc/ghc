{-# LANGUAGE Arrows          #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

-- See #10162 and #11743 for details

module ShouldCompile where

import Control.Arrow
import Language.Haskell.TH

handle :: ArrowPlus a => a b c -> a (b,String) c -> a b c
handle f h = proc b -> (f -< b) <+> (h -< (b,"FAIL"))

f :: ArrowPlus a => a (Int,Int) String
f = proc (x,y) ->
  ⦇handle
    (returnA -< show y)
    (\s -> returnA -< s ++ show x)
  ⦈

g :: ArrowPlus a => a (Int,Int) Int
g = proc (x,y) ->
  (
    (\z -> returnA -< x + z)
    <+>
    (\z -> returnA -< y + z)
  ) (x*y)


matches :: PatQ -> ExpQ
matches pat = ⟦\x ->
  case x of
    $pat -> True
    _    -> False
  ⟧

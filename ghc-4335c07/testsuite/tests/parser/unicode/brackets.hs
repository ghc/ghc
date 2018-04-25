{-# LANGUAGE Arrows          #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

-- See Trac #10162 and #11743 for details

module ShouldCompile where

import Control.Arrow
import Language.Haskell.TH

handle :: ArrowPlus a => a (b,s) c -> a (b,(String,s)) c -> a (b,s) c
handle f h = proc (b,s) -> (f -< (b,s)) <+> (h -< (b,("FAIL",s)))

f :: ArrowPlus a => a (Int,Int) String
f = proc (x,y) ->
  ⦇handle
    (returnA -< show y)
    (\s -> returnA -< s ++ show x)
  ⦈

g :: ArrowPlus a => a (Int,Int) String
g = proc (x,y) ->
  ⦇handle
    (\msg -> returnA -< msg ++ show y)
    (\s msg -> returnA -< s ++ show x)
  ⦈ ("hello " ++ show x)

h :: ArrowPlus a => a (Int,Int) Int
h = proc (x,y) ->
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

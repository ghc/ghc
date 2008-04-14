{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module PrimCoercions where
import Core

-- Stuff the parser needs to know about

pv :: a -> Qual a
pv = qual primMname

pvz :: Id -> Qual Id
pvz = (qual primMname) . (++ "zh")

{- Coercions -}
symCoercion, transCoercion, unsafeCoercion :: Qual Tcon
symCoercion    = pv "sym"
transCoercion  = pv "trans"
unsafeCoercion = pv "CoUnsafe"
leftCoercion   = pv "left"
rightCoercion   = pv "right"

{- Addrzh -}
tcAddrzh = pvz "Addr"
tAddrzh = Tcon tcAddrzh
ktAddrzh = Kunlifted

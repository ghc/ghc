{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Language.Core.PrimCoercions where
import Language.Core.Core

-- Stuff the parser needs to know about

pv :: a -> Qual a
pv = qual primMname

pvz :: Id -> Qual Id
pvz = (qual primMname) . (++ "zh")

{- Coercions -}
symCoercion, transCoercion, unsafeCoercion,
 leftCoercion, rightCoercion, instCoercion :: Qual Tcon
symCoercion    = pv "sym"
transCoercion  = pv "trans"
unsafeCoercion = pv "CoUnsafe"
leftCoercion   = pv "left"
rightCoercion  = pv "right"
instCoercion   = pv "inst"

{- Addrzh -}
tcAddrzh = pvz "Addr"
tAddrzh = Tcon tcAddrzh
ktAddrzh = Kunlifted

module Main where

import Control.Monad (unless)
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as Bytes
import GHCi.TH.Binary ()
import Language.Haskell.TH.Syntax

main :: IO ()
main = do
  let v = mkName "b"
      t = VarT v
      old = [ForallT [] [] t, ForallVisT [] t, AppT t t, AppKindT t t, SigT t t,
             t, ConT v, PromotedT v, InfixT t v t, UInfixT t v t,
             PromotedInfixT t v t, PromotedUInfixT t v t, ParensT t,
             TupleT 2, UnboxedTupleT 2, UnboxedSumT 2, ArrowT, MulArrowT, EqualityT,
             ListT, PromotedTupleT 2, PromotedNilT, PromotedConsT, StarT, ConstraintT,
             LitT (NumTyLit 0), WildCardT, ImplicitParamT "x" t, ForallMatchT t t]
      tags = map (Bytes.head . encode) old
  unless (tags == [0 .. 28]) (fail (show tags))
  let ix = IndexVarT v
      new = [ForEachT BndrReq v (ConT (mkName "Bool")) ix, ix,
             ForEachT BndrInvis v (ConT (mkName "Bool")) ix,
             CaseT ix [TypeCaseAlt t (ConT v), TypeCaseAlt WildCardT t],
             LetT v StarT (AppT (VarT v) (VarT v)), LamT [v] t]
  unless (map (Bytes.head . encode) new == [29, 30, 29, 31, 32, 33])
    (fail "Unexpected dependent-type Binary tags")
  unless (map (decode . encode) (old ++ new) == old ++ new)
    (fail "Template Haskell Binary round-trip changed a type")

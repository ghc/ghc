module Main where

import Control.Monad (unless)
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as Bytes
import GHCi.TH.Binary ()
import Language.Haskell.TH.Syntax

main :: IO ()
main = do
  let c = mkName "C"
      b = mkName "b"
      t = ConT c
      bang = Bang NoSourceUnpackedness NoSourceStrictness
      old = [NormalC c [], RecC c [], InfixC (bang, t) c (bang, t),
             ForallC [] [] (NormalC c []), GadtC [c] [] t, RecGadtC [c] [] t,
             ForallMatchC t (ForallC [] [] (NormalC c []))]
      fields = [GadtValueArg bang t t, GadtIndexArg b t, GadtValueArg bang t (IndexVarT b)]
      new = RetainedGadtC [c] fields t
      tags = map (Bytes.head . encode) (old ++ [new])
  unless (tags == [0 .. 7]) (fail ("Unexpected Con tags " ++ show tags))
  unless (map (decode . encode) (old ++ [new]) == old ++ [new])
    (fail "Template Haskell Binary round-trip changed a constructor")

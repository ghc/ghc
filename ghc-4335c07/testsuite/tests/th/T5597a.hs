{-# LANGUAGE TemplateHaskell #-}
module T5597a where
import Language.Haskell.TH

f :: Q Type -> Q Exp
f t = [| (3,4) :: $t |]

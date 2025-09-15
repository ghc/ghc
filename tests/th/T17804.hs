{-# OPTIONS_GHC -Wimplicit-lift #-}
{-# LANGUAGE TemplateHaskell #-}
module T17804 where

import Language.Haskell.TH.Syntax

warning1 :: Lift t => t -> Q Exp
warning1 x = [| x |]

warning2 :: Lift t => t -> Code Q t
warning2 x = [|| x ||]

noWarning1 :: Q Exp
noWarning1 = [| \x -> x |]

noWarning2 :: Code Q (a -> a)
noWarning2 = [|| \x -> x ||]

i :: Int
i = 0

noWarning3 :: Q Exp
noWarning3 = [| i |]

noWarning4 :: Code Q Int
noWarning4 = [|| i ||]

{-# LANGUAGE TemplateHaskell #-}

module T16895a where

import Language.Haskell.TH

main = print $(uInfixE [|1|] [|id id|] [|2|])

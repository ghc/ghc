{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module T14843 where

import Language.Haskell.TH.Syntax

type T1 = $(return (PromotedTupleT 2))
type T2 = $([t| '(,) |])
type T3 = $(return (PromotedT (tupleDataName 2)))

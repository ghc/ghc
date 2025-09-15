{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
module T24702a where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

$(do
  let
    step = \acc n -> acc `appT` n
    args n = replicate n (conT ''Int)

    mkTupleTest mkTupTy mkTupCon boxity n = do
      let
          nil = conT (mkTupTy n)
          tup = foldl step nil (args n)
      f <- newName (boxity <> show n)

      -- f<n> :: (,,..n..,,) t1 t2 .. tn -> ()
      -- f<n> = \ (_, _, ...n..., _) -> ()
      sequence $
        sigD f [t|$(tup) -> ()|] :
        valD (varP f) (normalB [e| \ $(conP (mkTupCon n) (replicate n wildP)) -> ()|]) [] :
          []

    mkSumTest n = do
      let
        nil = conT (unboxedSumTypeName n)
        sumTy = foldl step nil (args n)
        mkSumAlt altN =
          let sumDataCon = unboxedSumDataName altN n
              varName =  mkName "x" in
          clause [conP sumDataCon [varP varName]]
            (normalB (conE sumDataCon `appE` varE varName)) []
      f <- newName ("sum" <> show n)

      -- f<n> :: (#||...n...||#) -> (#||...n...||#)
      -- f<n> (x||...n...||) = (x||...n...||)
      -- f<n> (|x||...n...||) = (|x||...n...||)
      -- ...n...
      -- f<n> (||...n...||x) = (||...n...||x)
      sequence $
        sigD f [t|$(sumTy) -> $(sumTy)|] :
        funD f (map mkSumAlt [1 .. n]) :
        []

  newDeclarationGroup <>
    mkTupleTest
      unboxedTupleTypeName unboxedTupleDataName "unboxed"
      `foldMap` (64 : [0 .. 8]) <>
    mkTupleTest
      tupleTypeName tupleDataName "boxed"
      `foldMap` (64 : [0 .. 8]) <>
    mkSumTest 
      `foldMap` (63 : [2 .. 8]) )

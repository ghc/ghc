{-# LANGUAGE TemplateHaskell #-}

module T24046 where

import Language.Haskell.TH.Syntax

-- Test added in relation to this issue: https://gitlab.haskell.org/ghc/ghc/-/issues/24046

{-# NOINLINE foo #-}
foo = undefined

$( let simplerule = [PragmaD $ RuleP "rejected-rule" Nothing foralld lhs rhs AllPhases]

       foralld = [RuleVar $ mkName "x", RuleVar $ mkName "y"]

       lhs = AppE (AppE (VarE $ mkName "foo") (VarE $ mkName "x")) (VarE $ mkName "y")

       rhs = AppE (AppE (VarE $ mkName "foo") (VarE $ mkName "y")) (VarE $ mkName "x")
   in return simplerule)

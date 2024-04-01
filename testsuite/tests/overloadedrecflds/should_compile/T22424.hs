{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module T22424 where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

{-
data R1 = C1 { fld :: Bool }
data R2 = C2 { fld :: Bool }
fun x1 x2 = fld x1 && fld x2 -- referring to the two different 'fld's
-}

$(do
    let noBang = Bang NoSourceUnpackedness NoSourceStrictness
    let mkData tn cn fn = (DataD [] tn [] Nothing [RecC cn [(fn, noBang, ConT ''Bool)]] [], fn)
    (r1, fld1) <- mkData <$> newName "R1" <*> newName "C1" <*> newName "fld"
    (r2, fld2) <- mkData <$> newName "R2" <*> newName "C2" <*> newName "fld"
    fun <- newName "fun"
    x1 <- newName "x1"
    x2 <- newName "x2"
    let expr = UInfixE (VarE fld1 `AppE` VarE x1) (VarE '(&&)) (VarE fld2 `AppE` VarE x2)
        pats = [VarP x1, VarP x2]
        fun_decl = FunD fun [Clause pats (NormalB expr) []]
    pure [r1,r2,fun_decl]
 )

$(do
    let noBang = Bang NoSourceUnpackedness NoSourceStrictness
    let mkData tn cn fn = (DataD [] tn [] Nothing [RecC cn [(fn, noBang, ConT ''Bool)]] [], fn)
    (r1, fld1) <- mkData <$> newName "R1'" <*> newName "C1'" <*> pure (mkNameG_fld "me" "T22424" "C1'" "fld'")
    (r2, fld2) <- mkData <$> newName "R2'" <*> newName "C2'" <*> pure (mkNameG_fld "me" "T22424" "C2'" "fld'")
    fun <- newName "fun'"
    x1 <- newName "x1"
    x2 <- newName "x2"
    let expr = UInfixE (VarE fld1 `AppE` VarE x1) (VarE '(&&)) (VarE fld2 `AppE` VarE x2)
        pats = [VarP x1, VarP x2]
        fun_decl = FunD fun [Clause pats (NormalB expr) []]
    pure [r1,r2,fun_decl]
 )

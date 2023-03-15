{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.StaticPtr
  ( initStaticPtrs
  )
where

import GHC.Prelude
import GHC.Linker.Types (SptEntry(..))
import GHC.Fingerprint.Type
import GHC.Types.Literal

import GHC.JS.Unsat.Syntax
import GHC.JS.Make

import GHC.StgToJS.Types
import GHC.StgToJS.Literal
import GHC.StgToJS.Ids

initStaticPtrs :: [SptEntry] -> G JStat
initStaticPtrs ptrs = mconcat <$> mapM initStatic ptrs
  where
    initStatic (SptEntry sp_id (Fingerprint w1 w2)) = do
      i <- varForId sp_id
      fpa <- concat <$> mapM (genLit . mkLitWord64 . fromIntegral) [w1,w2]
      let sptInsert = ApplExpr (var "h$hs_spt_insert") (fpa ++ [i])
      return $ (var "h$initStatic" .^ "push") `ApplStat` [jLam sptInsert]


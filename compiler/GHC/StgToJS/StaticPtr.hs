{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.StaticPtr
  ( initStaticPtrs
  )
where

import GHC.Prelude
import GHC.Linker.Types (SptEntry(..))
import GHC.Fingerprint.Type
import GHC.Types.Literal
import GHC.Types.Name

import GHC.JS.JStg.Syntax
import GHC.JS.Make
import GHC.JS.Ident (name)

import GHC.StgToJS.Symbols
import GHC.StgToJS.Literal
import GHC.StgToJS.Types
import GHC.Utils.Panic (panic)

initStaticPtrs :: [SptEntry] -> G JStgStat
initStaticPtrs ptrs = mconcat <$> mapM initStatic ptrs
  where
    -- Build a reference to the closure variable for a top-level Name.
    -- Static pointer bindings are exported, so we can construct the symbol
    -- directly from the Name's module and OccName.
    varForName :: Name -> G JStgExpr
    varForName n = do
      case nameModule_maybe n of
        Just m  -> do
          let sym = mkJsSymbol True m (occNameMangledFS (nameOccName n))
          pure (ValExpr (JVar (name sym)))
        Nothing ->
          -- Shouldn't happen for SPT entries
          panic "varForName: non-external Name in SptEntry"

    initStatic (SptEntry sp_name (Fingerprint w1 w2)) = do
      i <- varForName sp_name
      fpa <- concat <$> mapM (genLit . mkLitWord64 . fromIntegral) [w1,w2]
      let sptInsert = ApplStat hdHsSptInsert (fpa ++ [i])
      return $ (hdInitStatic .^ "push") `ApplStat` [Func [] sptInsert]

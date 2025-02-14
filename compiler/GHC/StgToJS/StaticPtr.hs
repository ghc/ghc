{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.StaticPtr
  ( initStaticPtrs
  )
where

import GHC.Prelude
import GHC.Builtin.Types
import GHC.Linker.Types (SptEntry(..))
import GHC.Fingerprint.Type
import GHC.Types.Id
import GHC.Types.Literal

import GHC.JS.JStg.Syntax
import GHC.JS.Make

import GHC.StgToJS.Symbols
import GHC.StgToJS.Ids
import GHC.StgToJS.Literal
import GHC.StgToJS.Types

initStaticPtrs :: [SptEntry] -> G JStgStat
initStaticPtrs ptrs = mconcat <$> mapM initStatic ptrs
  where
    initStatic (SptEntry sp_nm (Fingerprint w1 w2)) = do
      i <- varForId $ mkVanillaGlobal sp_nm anyTy
      fpa <- concat <$> mapM (genLit . mkLitWord64 . fromIntegral) [w1,w2]
      let sptInsert = ApplStat hdHsSptInsert (fpa ++ [i])
      return $ (hdInitStatic .^ "push") `ApplStat` [Func [] sptInsert]

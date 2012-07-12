{-# LANGUAGE TemplateHaskell #-}

module T7022a where

import Language.Haskell.TH

makeSList :: Q [Dec]
-- makes "type SList (a :: [k]) = Sing a"
makeSList = do
   a <- newName "a"
   k <- newName "k"
   return [TySynD (mkName "SList") [KindedTV a (AppT ListT (VarT k))]
                  (AppT (ConT (mkName "Sing")) (VarT a))]

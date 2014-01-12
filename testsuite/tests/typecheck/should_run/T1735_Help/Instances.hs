{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances, OverlappingInstances, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- This is a module full of orphans, so don't warn about them

module T1735_Help.Instances () where

import T1735_Help.Basics
import Data.Typeable

charType :: DataType
charType = mkStringType "Prelude.Char"

instance Sat (ctx Char) =>
         Data ctx Char where
  toConstr _ x = mkStringConstr charType [x]
  gunfold _ _ z c = case constrRep c of
                      (StringConstr [x]) -> z x
                      _ -> error "gunfold Char"
  dataTypeOf _ _ = charType

nilConstr :: Constr
nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr :: Constr
consConstr   = mkConstr listDataType "(:)" [] Infix
listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

instance (Sat (ctx [a]), Data ctx a) =>
         Data ctx [a] where
  gfoldl _ _ z []     = z []
  gfoldl _ f z (x:xs) = z (:) `f` x `f` xs
  toConstr _ []    = nilConstr
  toConstr _ (_:_) = consConstr
  gunfold _ k z c = case constrIndex c of
                      1 -> z []
                      2 -> k (k (z (:)))
                      _ -> error "gunfold List"
  dataTypeOf _ _ = listDataType
  dataCast1 _ f = gcast1 f


{-# LANGUAGE TemplateHaskell, MagicHash, UnboxedTuples #-}

module T8954 where

import Language.Haskell.TH

$( do _ <- reify '(##)
      _ <- reify '(#,#)
      _ <- reify ''(##)
      _ <- reify ''(#,#)
      _ <- reify '()
      _ <- reify ''()
      _ <- reify '[]
      _ <- reify ''[]
      return [] )

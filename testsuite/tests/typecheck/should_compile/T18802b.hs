{-# LANGUAGE GADTs, DataKinds #-}

module T18802b where

data G a where
  MkG :: { fld :: Char } -> G Float

recUpd :: ( Char -> Char ) -> G a -> G a
recUpd f g@(MkG { fld = c }) = g { fld = f c }

{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Woverlapping-patterns -Wincomplete-patterns #-}

module T11374 where

data Type   = TCon TCon [Type]
            | TUser String [Type] Type
            | TRec [(String,Type)]
            deriving (Show,Eq,Ord)

data TCon   = TC TC
            | TF TFun
            deriving (Show,Eq,Ord)

data TC     = TCNum Integer
            | TCInf
            | TCBit
            | TCSeq
            | TCFun
            | TCTuple Int
            deriving (Show,Eq,Ord)

data TFun   = TCAdd
            | TCSub
            | TCMul
            | TCDiv
            | TCMod
            | TCLg2
            | TCExp
            | TCWidth
            | TCMin
            | TCMax
            | TCLenFromThen
            | TCLenFromThenTo
            deriving (Show, Eq, Ord, Bounded, Enum)

simpFinTy :: Type -> Maybe [Type]
simpFinTy ty = case ty of
    TCon (TC (TCNum _)) _ -> Just []

    TCon (TF tf) [t1]
      | TCLg2    <- tf -> Just [t1]
      | TCWidth  <- tf -> Just [t1]

    TCon (TF tf) [t1,t2]
      | TCAdd <- tf -> Just [t1, t2]
      | TCSub <- tf -> Just [t1]
      | TCMul <- tf -> Just [t1, t2]
      | TCDiv <- tf -> Just [t1]
      | TCMod <- tf -> Just []
      | TCExp <- tf -> Just [t1, t2]
      | TCMin <- tf -> Nothing
      | TCMax <- tf -> Just [t1, t2]

    TCon (TF tf) [_,_,_]
      | TCLenFromThen   <- tf -> Just []
      | TCLenFromThenTo <- tf -> Just []

    _ -> Nothing

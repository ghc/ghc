{-# LANGUAGE CPP #-}

module T18733_Lib where

#if 1
data A = ARecu B | ABase String deriving (Show)
data B = BRecu A | BBase Int deriving (Show)

info :: B
info = BBase 1

#else
data A = ARecu B | ABase String deriving (Show)
data B = BRecu A | BBase Int deriving (Show)

info :: A
info = ABase "Hello"
#endif

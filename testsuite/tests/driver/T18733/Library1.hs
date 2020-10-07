module Library where

data A = ARecu B | ABase String deriving (Show)
data B = BRecu A | BBase Int deriving (Show)

info :: B
info = BBase 1

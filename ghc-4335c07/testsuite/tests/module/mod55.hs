-- !!! Illegal deriving Enum 
module M where
data T = K Int deriving (Enum)

-- !!! Illegal deriving Ix
module M where
import Data.Ix(Ix)
data T = K1 Int | K2 deriving (Eq,Ord,Ix)

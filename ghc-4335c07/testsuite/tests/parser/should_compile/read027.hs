module ShouldCompile where

infix 5 |- 
infix 9 :=

data Equal = Char := Int

-- fails in GHC (all versions), due to not doing fixity resolution on
-- the lhs before deciding which is the function symbol.

(|-) :: Int -> Equal -> Bool
0 |- x:=y = 1 |- x:=y      -- XXX fails here
2 |- (x:=y) = 0 |- x:=y
_ |-  _     = False  

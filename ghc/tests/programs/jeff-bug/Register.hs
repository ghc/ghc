module Register where

import Ix


-- Begin Signature -------------------------------------------------
{- 

   Register captures the common instances and methods that is 
   usually required of a register-set like type.  Register 
   supports PCs, speculative PCs, and predicate registers.

-}


class (Ix a,Eq a,Ord a,Bounded a,Show a,Read a,Enum a) => Register a where

        -- is the register read only?   for example, in DLX 
        --  r0 <- r0 + r0  is equivilant to a nop because r0 is read only
        readOnly :: a -> Bool

        -- pick out the PC register
        pc :: a 

        -- pick out the speculative PC register
        specpc :: a 

        ispc :: a -> Bool
        isspecpc :: a -> Bool 

        -- is the register a predicate register?
        ispred :: a -> Bool

        readOnly x = False
        ispred x = False

-- End Signature -------------------------------------------------
        -- ispc and isspec should probably be defined as :
	--       ispc r = pc == r.  havent tested this though....

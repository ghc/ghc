module Memory where

import Word
import Ix
import Maybe
import Arithmetic
import Array
import Words

-- Begin Signature -------------------------------------------------

{- 
   Some types to describe encodings of memory state and the 
   communication to memory
-}

type ArrayDesc i v = ((i,i),[(i,i,v)])

type InstrMemoryState w i = ArrayDesc w i
type MemoryState w i = (InstrMemoryState w i,DataMemoryState w)
type DataMemoryState w = ArrayDesc w w

data WordSize = Byte | HalfWord | FullWord
          deriving (Eq,Show, Read)

data LoadStoreOp = Load WordSize Sign
                 | Store WordSize 
                 | NOP  -- No operation
           deriving (Eq,Show, Read)



-- Array request
data ArrReq i a  = ReadArr i |
		   WriteArr i i a |
		   WriteFn i (a -> a) |	-- modify contents at location i
		   FreezeArr
		   deriving Show

-- Array response
data ArrResp i a = ReadVal a |
		   Written |
		   WrittenFn a |
		   ArrayVal (Array i a)
		   deriving Show

-- End Signature -------------------------------------------------------



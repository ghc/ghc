module PipeReg where


import Trans
import Signal 
import Register
import Instruction

-- Begin Signature ----------------------------------------------------------

{- 
  pipeReg is helpful for constructing in pipelines 
-}

data PipeRegCmd = Input | Stall | Kill
                  deriving (Eq,Ord,Enum,Bounded,Show)


-- pipeReg t cmd ts , on the first cycle return "t", in later cycles,
-- if cmd=Input then return ts, if cmd=Stall then return the previous
-- value and store the input, if cmd=Kill then return a nop

pipeReg :: (Instruction a, Register b) => 
           Trans a (c b d) -> Signal PipeRegCmd -> 
           Signal (Trans a (c b d)) -> Signal (Trans a (c b d))

input           :: Signal PipeRegCmd
stall           :: Signal PipeRegCmd
kill            :: Signal PipeRegCmd

-- End Signature ----------------------------------------------------------

pipeReg init cmd incoming = out
  where out = delay init (if' (cmd*==input) then' incoming
                          else' (if' (cmd*==stall) then' out 
                          else' (lift0 nop))
                         )

input = lift0 Input
stall = lift0 Stall
kill = lift0 Kill


module Devices where

import List
import TransSig
import qualified Trans as T
import qualified PreludeSig as Signaled
import Signal
import Words
import Register
import Arithmetic
import Cell
import Memory
import Utilities

import LazyST
import Instruction 

import Array
import Monad
import StateArray
import Ix



-- Begin Signature -------------------------------------------------------
{- 
   Devices defines common circuits (for example ,instruction 
   and data memory or alus) 
-}


-- flush k x b s , when b then return x for k cycles, otherwise s
-- flush 2 10 <False,False,True,False,False,False,False .. > <1 .. 7 .. >
--        = <1,2,10,10,5,6,7 .. >
flush       :: Int -> a -> Signal Bool -> Signal a -> Signal a

-- latch x b xs. return the value of xs when last b occurred.  initialize
-- with x
latch       :: a -> Signal Bool -> Signal a -> Signal a



regFile     :: Ix i  => (i,i) ->      -- Register addresses
                        [a] ->                   -- Initial contents
                        [(Signal Bool,           -- Write enables
                        (Signal i,Signal a))] -> -- Write ports
                        [Signal i] ->            -- Read ports
                        [Signal a]               -- Read port contents


dataMemory :: (Word a, Word b) => a -> ArrayDesc a b ->
              Signal LoadStoreOp -> Signal a -> Signal b -> Signal b


-- fetch (k,translate,max,memory) pc width = (instrs,pc')
--   k = the number of indices between addresses.  
--   translate = the function that creates transaction from
--               the representation in memory
--   max = the largest number of instructions to fetch on 
--         a single cycle
--   memory = the program
--   pc = the pc to fetch
--   width = the number of instructions to fetch starting at pc
fetch  :: (Cell c, Register r, Word w, Instruction i) =>
          (w,f -> Trans i (c r w),Int,ArrayDesc w f) ->       
          Signal (Trans i (c r w)) -> Signal Int ->       
          (Signal [Trans i (c r w)],Signal (Trans i (c r w)))


-- trans_alu is alu applied to transactions in the obvious way:
-- intuitively,   
--  trans_alu(Trans [dst] op [s1,s2]) = Trans [dst=alu(op,s1,s2)] op [s1,s2]
trans_alu :: (Register r, Cell c, Word w, Instruction i) => 
             (Trans i (c r w)) -> (Trans i (c r w))

--- exec is trans_alu lifted on signals
exec :: (Register r, Cell c, Word w, Instruction i) => 
        Signal (Trans i (c r w)) -> Signal (Trans i (c r w))


-- mem serves loads and stores
mem :: (Instruction i, Cell c, Register r,Word w) => 
       w -> ArrayDesc w w ->
       Signal (Trans i (c r w)) -> Signal (Trans i (c r w))

ss_mem :: (Instruction i, Cell c, Register r, Word w) => 
       w -> ArrayDesc w w -> 
       Signal [Trans i (c r w)] -> Signal [Trans i (c r w)]



-- End Signature -------------------------------------------------------

ss_mem k m = superscalar (mem k m)


--- WHOA! this code is a black-hole.  Read only if you must!
--- This could use some serious house-cleaning.

-- "fetch n mem pc size" fetches (min n size) consec. instructions
-- following "pc"
fetch (k,f,lim,memory@(range,_)) pc n
     = (id,last') >< instrsFetch k f memory pcs
  where
  size = Signaled.min n (lift0 lim)
  last' s = if' (Signaled.length s *< 1) then' pc
            else' (Signaled.last s)
  pcs = lift2 (buildPCs k range) pc n

  buildPCs :: (Word w, Cell c, Register r, Instruction i) =>
            w -> (w,w) -> Trans i (c r w) -> Int -> [Trans i (c r w)]
  buildPCs k range pctrans n
    = do p <- fmap getReg $ T.getDstPC pctrans
         pc <- fmap getVal $ T.getDstPC pctrans
         let pcs = filter (inRange range) $ take n [pc,pc+k .. ]
         return $ map (mkPC range) pcs
      `catchEx` []
  mkPC range x = if inRange range x then T.pcTrans x
                  else T.nop

instrFetch n convert m input = (head' x, head' y)
  where
  fetch = instrsFetch n convert m
  (x,y) = fetch (toList input)
  toList = lift1 $ \x -> [x]
  head' = lift1 head


instrsFetch n convert initContents pcs
  = (insertPCs curPC $ instructions `bypassList` nextPCTrans, nextPCTrans)
    where
      bypassList = lift2 $ \x y -> zipWith T.bypass x y
      instructions = lift1 (map convert) $ instrMemory n initContents curPC
      curPC = lift1 (map getpc) pcs
      nextPCTrans = lift1 (map (\x -> T.pcTrans $ x+n)) curPC
      getpc t = 
          do reg <- T.getDstPC t
             let p = getReg reg
             let x = getVal reg 
             guard $ ispc p
             return x
           `catchEx` (error "ugh" ) -- $ "getpc " ++ show t)
      insertPCs pcs l = lift2 addPCs pcs l   
      addPCs x y = zipWith addPC x y
      addPC pc (Trans d o s l) = Trans d o s (loc pc:l)


exec trans = lift1 trans_alu trans

trans_alu trans@(Trans (dest:_) op (src:_) _) 
    | isAluOp op && aluOp op == Input1 =
	 trans `T.evalTrans` (dest,(alu Input1 (getVal src) undefined))
trans_alu trans@(Trans (dest:_) op (src:_) _) 
    | isAluOp op && aluOp op == SetHi =
	 trans `T.evalTrans` (dest,(alu SetHi (getVal src) undefined))
trans_alu trans@(Trans [dest] op (src1:src2:_) _) 
    | isAluOp op =
	 trans `T.evalTrans` (dest,(alu (aluOp op) (getVal src1) (getVal src2)))
trans_alu t@(Trans (d1:d2:_) op (s1:s2:_) _) 
  | isAluOp op =
      let t' = t `T.evalTrans` (d1,(alu (aluOp op) (getVal s1) (getVal s2)))
          Trans (d1':_) _ _ _ = t'
      in t' `T.evalTrans` (d2,(alu Not (getVal d1') (getVal d1')))
trans_alu trans@(Trans (dest:_) op (cond:src1:src2:_) _) | isCond op = 
                let eqZeroFunc = fstOp op
                    neqZeroFunc = sndOp op 
                in trans `T.evalTrans` (dest,alu 
                        (if (getVal cond) == 0 then eqZeroFunc else neqZeroFunc)
				      (getVal src1)
				      (getVal src2))
trans_alu trans@(Trans (dest1:dest2:_)
                         op 
			 (src1:src2:_) _) | isPar op 
	= (trans `T.evalTrans` (dest1,alu aluFunc1 src1Data src2Data))
		 `T.evalTrans` (dest2,alu aluFunc2 src1Data src2Data)
	  where
            aluFunc1 = fstOp op
            aluFunc2 = sndOp op
	    src1Data = getVal src1
	    src2Data = getVal src2
trans_alu trans@(Trans _ o _ _ ) 
            | isMemOp o = trans
            | isNoOp o = trans
--            | otherwise = error ("Unexecutable transaction: ")
            | otherwise = trans


------------ Memory stage ------------


mem k initContents trans 
  = if' loadInstr
      then' (trans `evalTrans` (bundle2 (loadReg,lift1 Just 
            (dataMemory k initContents loadStoreOp address contents))))
      else' trans
    where
      (loadInstr,loadReg,loadStoreOp,address,contents)
	= unbundle5 $ lift1 dataMemOps trans
dataMemOps (Trans (dest:_) op (address:offset:_) _) | isLoadOp op
	= (True,dest,loadOp,getVal address + getVal offset,0)
           where loadOp = memOp op
dataMemOps(Trans _ op (address:offset:val:_) _) | isStoreOp op
	= (False,
	   undefined, -- pcNothing
	   storeOp,
	   getVal address + getVal offset,
	   getVal val)
          where storeOp = memOp op
dataMemOps _ 
	= (False,
	   undefined, -- pcNothing,
	   NOP,
	   0,
	   0)





flush n d s1 s2 = runST (
    do { n' <- newSTRef 0
       ; loop (bundle2 (s1,s2)) (\(s1,s2) -> 
         do { if s1 then writeSTRef n' n else return ()
            ; n <- readSTRef n'
            ; if n>0 then do {writeSTRef n' (n-1)
                             ; return d
                             }
               else return s2
            })
       }
  )



--------------- Latches ----------------
-- latch stores a value, until it is reset to a new value by
--  the boolean signal. "init" is the value stored in latch
--  at time zero.
latch init reset resetVal = out		
  where 
  out = (if' reset then' resetVal else' last)
  last = delay init out


--------------- Buses ------------------

--busReg      :: Int -> [a] -> [Signal a] -> [Signal a]
--busReg n inits xs = zipInt n delay inits xs
--  where
--  zipInt 0 f xs ys           = []
--  zipInt n f ~(x:xs) ~(y:ys) = f x y : zipInt (n-1) f xs ys


------------------ Register Files ----------------

-- A bank of registers. The bank
--  contains multiple read- and write-ports. Note that 
--  the contents of a write port are reflected in the
--  associated read port in the same clock cycle.
regFile bounds initContents writePorts readPorts
  = map (lift2 (!) registers) readPorts
    where
    registers = updateArray lastRegisters writePorts
    lastRegisters = delay (listArray bounds initContents) registers
			   


-- Bank of registers, where each register holds a pair of values.
{-
pairRegFile :: Ix i => (i,i) ->                -- Register addresses
		       [(a,b)] ->                         -- Initial contents
		       [(Signal Bool,                     -- Write enables
		        (Signal i,Signal a,Signal b))] -> -- Write ports
		       [Signal i] ->                      -- Read ports
		       [(Signal a,Signal b)]              -- Read port contents

pairRegFile bounds initVals writePorts readPorts
  = map unbundle2 (regFile bounds initVals zipWritePorts readPorts)
    where
    zipWritePorts = map (\(writeEnable,(writeAddress,writeA,writeB)) ->
		        (writeEnable,(writeAddress, bundle2 (writeA,writeB))))
		        writePorts

--type WriteBackData s reg w = (s w       -- writeback contents
--                          ,s reg     -- writeback register name
--                          )
 
 
-}


-- I THINK THAT THIS ONE SHOULD GO...
registers src1 src2 p
  = unbundle2 $ fmap getContents arrResps
    where
      (writebackContents,writebackReg) = unbundle2 p
      arrResps = stateArray ((minBound,maxBound),[(minBound,maxBound,0)])
                             (lift4 arrReqs src1 src2 writebackContents writebackReg)
 
      getContents [ReadVal src1, ReadVal src2]
        = (src1,src2)
      getContents [Written, ReadVal src1, ReadVal src2]
        = (src1,src2)
      getContents [Written,Written, ReadVal src1, ReadVal src2]
        = (src1,src2)
 
      arrReqs src1 src2 wbContents wbReg
        = wb ++
          [ ReadArr src1,
           ReadArr src2]
        where wb = map (\(x,y) -> WriteArr x x y) (zip wbReg wbContents)



instrMemory sz arrDesc pcAddresses = lift1 (map getInstr) arrResp
  where
  getInstr (ReadVal instr) = instr
  arrResp = let y = lift1 ( map (\addr -> ReadArr (addr `div` sz))) pcAddresses
            in stateArray arrDesc y
 
 

-----------------------------------------------------------------------------
dataMemory sz arrDesc loadStoreCmd addr writeVal
  = liftFn getLMD $ stateArray arrDesc arrReqs
    where
      {- Halfword and byte stores are implemented by loading the relevent
         word from memory, modifying the correct portion of the word, and
         then storing the revised word back to memory, all within one
         clock cycle
      -}
 
      liftFn = lift2 ($)

      (arrReqs,getLMD) = unbundle2 $ lift3 interpCmd loadStoreCmd addr writeVal
 
--      interpCmd :: Word w => LoadStoreOp -> w -> w ->
--                   ([ArrReq w w],[ArrResp w w] -> w)
 
      interpCmd (Load FullWord _ ) addr _
        = ([ReadArr (addr `div` sz)],
           (\[ReadVal val] -> val))
 
      interpCmd (Load HalfWord signedness) addr _
        = ([ReadArr wordAddr],
           (\[ReadVal val] -> subfield signedness 
                         (4 * sz) ((4 * sz) - (2 * sz) * wordMod) val)
           )
          where
            (wordAddr,wordMod) = addr `divMod` sz
 
      interpCmd (Load Byte signedness) addr _
        = ([ReadArr wordAddr],
           (\[ReadVal val] -> subfield signedness 
                   (2 * sz) ((6 * sz) - (2 * sz) * wordMod) val)
           )
          where
            (wordAddr,wordMod) = addr `divMod` sz
 
      interpCmd (Store FullWord) addr val
        = ([WriteArr wordAddr wordAddr val],
           (\[Written] -> val))
          where
            wordAddr = addr `div` sz
 
      interpCmd (Store HalfWord) addr val
        = ([WriteFn wordAddr modifyHalfword],
           (\[WrittenFn val] -> val))
          where
            (wordAddr,wordMod) = addr `divMod` sz
            modifyHalfword wordContents
              = setSubfield (4 * sz)
                            ((4 * sz) - (2 * sz) * wordMod)
                            wordContents
                            (wordContents `mod` num_half)
 
      interpCmd (Store Byte) addr val
        = ([WriteFn wordAddr modifyByte],
           (\[WrittenFn val] -> val))
          where
            (wordAddr,wordMod) = addr `divMod` 4
            modifyByte wordContents
              = setSubfield (2 * sz)
                            ((6 * sz) - (2 * sz) * wordMod)
                            wordContents
                            (wordContents `mod` num_bytes)
 
      interpCmd NOP _ _
        = ([],const 0)
 
 

--  treating 'n' as a bitfield, this function extracts a subrange
--  bitfield specified by 'subfieldLen' and 'subfieldStartPos'.
--  For subfieldStartPos, 0 indicates the subfield starts at the
--  least significant bit position of 'n'.
--  If 'signedness' == Signed, this function performs sign-extension
--  to the result subfield.
subfield :: (Integral a, Integral b, Integral c) => Sign -> c -> a -> b -> b
subfield signed subfieldLen subfieldStartPos n
 = (n `div` (2^subfieldStartPos)) `modOp` (2^subfieldLen)
 where
 modOp = case signed of
           Signed   -> signedModulus
           Unsigned -> mod
 
-- This function returns 'n', modified by replacing the subfield
--  denoted with (subfieldLen,subfieldStartPos), by 's'.
--  Note that s must satisfy ( 0 <= s <= 2^subfieldLen)
setSubfield :: (Integral a, Integral b, Integral c) => a -> b -> c -> c -> c
setSubfield subfieldLen subfieldStartPos n s
 = n + (2^subfieldLen)*(s - (subfield Unsigned subfieldLen subfieldStartPos n))




module TransSig
  (     Trans(..)

	,nop

	,isNop
	,isAdd
	,isAlu
	,isCmp
	,isBool
	,isSub
	,isMul
	,isDiv
	,isJump
	,isMem
	,isMove
	,isLoad
	,isStore
	,isBranch

	,updCells
	,repCells

	,updDst
        ,repDst
        ,addDst
	,getDst
        ,putDst

	,updSrc
        ,addSrc
        ,getSrc
        ,putSrc

	,addInfo
        ,getInfo
        ,putInfo

        ,getOp
	,putOp

	,getSpecPC
	,getDstPC
	,getSrcPC
        ,getLoc 

        ,getSrcRegs 
        ,getDstRegs
        ,getSrcRegVals 
        ,putDstRegVal

        ,bypass
	,bypassDst
	,bypassMany
	,bypassDstMany
	,broadcast

	,readyToRetire 
	,complete
	,readyToCompute

	,evalTrans
        ,rawHazard
	,filterDst

        ,pcTrans
        , getPredicate
        , isPredicated 
        , evalPredicate



  ) where

import List
import Instruction
import Signal
import Register
import Words
import Arithmetic
import qualified Trans as T
import Trans(Trans(..))
import Cell


-- Begin Signature ---------------------------------------------------
{- 
  The functions in TransSig are identical to Trans, except
  that they have been lifted on Signals
-}


nop		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w))
isNop		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isAdd		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isAlu		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isCmp		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isBool		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isSub		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isMul		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isDiv		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isJump		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isMem		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isMove		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isLoad		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isStore		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
isBranch	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
updCells	:: (Cell c, Word w, Register r) => 
                    Signal [c r w] -> Signal [c r w] -> Signal [c r w]
repCells	:: (Cell c, Word w, Register r) => 
                    (c r w -> c r w -> Bool) -> 
		    Signal [c r w] -> Signal [c r w] -> Signal [c r w]

updDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))
repDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   (c r w -> c r w -> Bool) ->
		   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))
addDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (c r w) -> Signal (Trans i (c r w)) -> 
                   Signal (Trans i (c r w))
getDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w]
putDst		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))
updSrc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))
addSrc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (c r w) -> Signal (Trans i (c r w)) -> 
                   Signal (Trans i (c r w))
getSrc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w]
putSrc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))

addInfo		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (c r w) -> Signal (Trans i (c r w)) -> 
                   Signal (Trans i (c r w))
getInfo		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w]
putInfo		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [c r w] -> 
                   Signal (Trans i (c r w))

getOp		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal i
putOp		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal i -> 
                   Signal (Trans i (c r w))

getSpecPC	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (Maybe (c r w))
getDstPC	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (Maybe (c r w))
getSrcPC	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (Maybe (c r w))
getLoc		:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (Maybe (c r w))

getSrcRegs	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [r]
getDstRegs	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [r]
getSrcRegVals	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal [w]
putDstRegVal	:: (Instruction i,Cell c,Register r,Word w) =>  
                   Signal (Trans i (c r w)) -> Signal w -> 
                   Signal (Trans i (c r w))
bypass		:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal (Trans i (c r w)) -> Signal (Trans i (c r w)) -> 
                   Signal (Trans i (c r w))
bypassDst	:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal (Trans i (c r w)) -> Signal (Trans i (c r w)) ->
	           Signal (Trans i (c r w))
bypassMany	:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal (Trans i (c r w)) -> Signal [Trans i (c r w)] ->
		   Signal (Trans i (c r w))
bypassDstMany	:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal (Trans i (c r w)) -> Signal [Trans i (c r w)] ->
		   Signal (Trans i (c r w))
broadcast ::       (Cell a, Register b, Word c) => 
                   Signal [Trans e (a b c)] -> Signal [Trans e (a b c)] -> 
                   Signal [Trans e (a b c)]


readyToRetire	:: (Instruction i,Cell c,Register r,Word w) =>
                   Signal [Trans i (c r w)] -> 
                   Signal ([Trans i (c r w)],[Trans i (c r w)])
complete	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal Bool
readyToCompute	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal [Trans i (c r w)] -> 
                   Signal ([Trans i (c r w)],[Trans i (c r w)])

evalTrans	:: (Instruction i,Cell c,Register r,Word w) => 
                   Signal (Trans i (c r w)) -> Signal (c r w, Maybe w) -> 
                   Signal (Trans i (c r w))
rawHazard	:: (Instruction i,Cell c,Register r,Word w) => 
                   (Signal (Trans i (c r w)),Signal (Trans i (c r w))) 
                   -> Signal Bool
filterDst	:: (Instruction i,Cell c,Register r,Word w) => 
                   (c r w -> Bool) -> Signal (Trans i (c r w)) -> 
	           Signal [c r w]

pcTrans         :: (Instruction i,Cell c,Register r,Word w) => 
                   Signal w -> Signal (Trans i (c r w))



getPredicate   :: (Cell c,Register r,Word w) => 
                  Signal (Trans i (c r w)) -> Signal (c r w)
isPredicated   :: (Cell c,Register r,Word w) => 
                  Signal (Trans i (c r w)) -> Signal Bool

evalPredicate   :: (Cell c,Register r,Word w) => 
                  Signal (Trans i (c r w)) -> Signal w


-- End Signature -------------------------------------------------------------
nop 			= lift0 $ T.nop

isNop			= lift1 T.isNop
isAdd			= lift1 T.isAdd
isAlu			= lift1 T.isAlu
isCmp			= lift1 T.isCmp
isBool			= lift1 T.isBool
isSub			= lift1 T.isSub
isMul			= lift1 T.isMul
isDiv			= lift1 T.isDiv
isJump			= lift1 T.isJump
isMem			= lift1 T.isMem
isMove			= lift1 T.isMove
isLoad			= lift1 T.isLoad
isStore			= lift1 T.isStore
isBranch		= lift1 T.isBranch

updCells		= lift2 T.updCells
repCells f		= lift2 $ T.repCells f

updDst	 		= lift2 T.updDst
repDst f		= lift2 $ T.repDst f
addDst			= lift2 T.addDst
getDst			= lift1 T.getDst
putDst			= lift2 T.putDst

updSrc			= lift2 T.updSrc
--repSrc		=
addSrc			= lift2 T.addSrc
getSrc			= lift1 T.getSrc
putSrc			= lift2 T.putSrc

addInfo			= lift2 T.addInfo
getInfo			= lift1 T.getInfo
putInfo			= lift2 T.putInfo

getOp			= lift1 T.getOp
putOp			= lift2 T.putOp

getSpecPC		= lift1 T.getSpecPC
getDstPC		= lift1 T.getDstPC
getSrcPC		= lift1 T.getSrcPC
getLoc			= lift1 T.getLoc

getSrcRegs		= lift1 T.getSrcRegs
getDstRegs		= lift1 T.getDstRegs
getSrcRegVals		= lift1 T.getSrcRegVals
putDstRegVal		= lift2 T.putDstRegVal

evalTrans		= lift2 T.evalTrans
rawHazard ts		= lift1 T.rawHazard (bundle2 ts)

bypass			= lift2 T.bypass
bypassDst		= lift2 T.bypassDst
bypassMany		= lift2 T.bypassMany
bypassDstMany		= lift2 T.bypassDstMany
broadcast		= lift2 T.broadcast

readyToRetire		= lift1 T.readyToRetire
complete		= lift1 T.complete
readyToCompute		= lift1 T.readyToCompute

filterDst f		= lift1 $ T.filterDst f


pcTrans = lift1 T.pcTrans

-- Predicated instructions
getPredicate = lift1 T.getPredicate
isPredicated = lift1 T.isPredicated
evalPredicate = lift1 T.evalPredicate

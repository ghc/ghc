module ROB_retire(retire)  where

import LazyST
import Hawk
import Trans

import qualified RF
import qualified AQ as Q
import qualified RAT 
import RF(RF)
import AQ(AQ)
import RAT(RAT)

import DLX
import Monad
import Utils



retire rat q regfile
  = do { perhaps <- retireable q
       ; let (retired,missed) = jumpHazard perhaps
       ; mapM (writeOut regfile rat) retired
       ; return (cleanUp retired,missed)
       }
  where cleanUp = map removeVirtuals

--retireable :: Register r => AQ s (Trans i r w) -> ST s [Trans i r w]
retireable q = Q.deQueueWhile q complete

--jumpHazard :: (Register r,Word w) => [VTrans r w] -> ([VTrans r w],Bool)
jumpHazard [] = ([],False)
jumpHazard (instr:is) 
  = if branchMissed instr then ([instr],True)
       else (instr:is',False || die)
  where (is',die) = jumpHazard is

--branchMissed :: (Register r,Word w) => VTrans r w -> Bool
branchMissed t = 
  do { Reg (Virtual _ (Just pc)) (Val x) <- getDstPC t
     ; Reg (Real spc) (Val y) <- getSpecPC t
     ; guard $ ispc pc
     ; guard $ isspecpc spc
     ; return $ x /= y
     }
   `catchEx` False

--writeOut :: Register r => RF s r w -> RAT s r Int -> 
--                          VTrans r w -> ST s ()
writeOut file rat t
  = do { let [Reg (Virtual vr (Just real)) (Val x)] = getDst t
       ; RF.write file real x
       ; a <- RAT.read rat real
       ; do {v <- a
            ; guard $ v == vr
            ; return $ RAT.remove rat real
            }
         `catchEx` return ()
       }




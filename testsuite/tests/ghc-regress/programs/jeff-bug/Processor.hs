module Processor where


import Hawk
import Trans 
import qualified TransSig as T
import qualified PreludeSig as Signaled
import qualified Word


import Utils
import DLX

import Predict

import IFU(ifu)
import RS(rs)
import ROB(rob)
import EUs

processor ((pgm,pgmdata),startingPC) =  retired -- retired 
  where

      (instrs,pc') = ifu (5,pgm) pc ([5,5] `before` space)
      instrs' = probe "UFO" instrs

      --testTrans = lift1 (\n -> [pcTrans n]) space

      pc   = delay (pcTrans startingPC) npc
      npc = if' miss then' (Signaled.last retired ) 
                     else' pc'

      annotated = delay [] (
                    if' miss then' (lift0 []) 
                             else' (annotate $ filterOut isNop $ instrs)
                    )

      (retired,ready,space,miss) = rob 100 (annotated, computed)
      --miss' = if' miss then' (lift0 $ pcTrans 1) else' (lift0 $ pcTrans 0)
  
      computed = rs (150,execUnits) (delay False miss, delay [] ready)


execUnits :: Word a => [EU DLX_Op (DLX_Cell (Virtual DLXReg Int) a)]
execUnits = [addUnit,addUnit,subUnit,jumpUnit,jumpUnit,multUnit,divUnit,cmpUnit,moveUnit]

multUnit' b s = probe "mu_out" out
        where b' = probe "mu_cnt" b
              s' = probe "mu_in" s
              out = multUnit b' s'

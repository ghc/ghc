module RS
  (
	rs
  )  where

import LazyST

import Hawk
import qualified PreludeSig as Sig
import Trans
import qualified TransSig as T
import qualified BoundedSet as Set

import EUs
import DLX


type RS i c r w = (Int,[EU i (c (VReg r) w)]) ->
            (Signal Bool,Signal [VTrans r w]) -> 
            Signal [VTrans r w]


--rs :: (Register r,Word w) => RS StandardOp c r w
rs (n,execUnits) (mispredicted,input)
  = computed
   where
   ready = runST (
        do { set <- Set.new n
           ; loop wires $ 
                \(instrs,mispredicted,computed,rejected) -> 
                           if mispredicted 
                             then do { Set.clear set
                                     ; return []
                                     }
                             else do { Set.insert set instrs
                                     ; Set.insert set rejected
                                     ; broadcast' set computed
                                     ; ready <- Set.rmSuch set isComputable
                                     ; return ready
                                     }
          }
        )
   wires = bundle4 (input,mispredicted,computed,rejected)
   (computed,rejected) = unbundle2 $ delay ([],[]) $ 
                         execUnit mispredicted ready

   execUnit = schedule execUnits

broadcast' set computed
  = do { s <- Set.read set
       ; let dests = concat $ map getDst computed
       ; Set.iterateSet set (flip fillInSrcCells dests)
       }


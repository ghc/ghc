module ROB(rob)  where
import LazyST

import Hawk
import Trans

import qualified RF 
import qualified AQ as Q
import qualified RAT 
import RF(RF)
import AQ(AQ)
import RAT(RAT)
import ROB_insert(insert)
import ROB_retire(retire)

import DLX


#include "hawk-macros.h"

{-
type ROB s i r w =  Int ->                                         
              (s [Trans i r w], s [VTrans r w]) ->
              (s [Trans i r w], s [VTrans r w], s Int,s Bool)
-}
 

--rob :: (Signal s,Register r,Word w) => ROB s StandardOp r w
rob n (fetched,computed)
  = unbundle4 $ runST (
    do { q <- Q.new n
       ; rat <- RAT.new
       ; regfile <- RF.new 
       ; step2(fetched,computed)
           { update q computed
           ; instrs <- insert rat q regfile fetched
           ; (retired,missed) <- retire rat q regfile
           ; inCase missed $ do { Q.clear q
                                ; RAT.clear rat
                                }
           ; capacity <- Q.space q
           ; let ready = if missed then [] else instrs
           ; return (retired,ready,capacity,missed)
           }
       }
   )   
    where 
    inCase x y = if x then y else return ()


-- assumes single register dest ops (not a good assumption)
--update :: (Register r,Word w) => AQ s (VTrans r w) -> [VTrans r w] -> ST s ()
update q
  = mapM_ $ \t ->
    do { let [Reg (Virtual v (Just r)) val] = getDst t 
       ; Q.insert q v t
       }



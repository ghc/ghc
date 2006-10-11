-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow -2006
--
-- Code generation relaed to GpH
-- 	(a) parallel
--	(b) GranSim
--
-----------------------------------------------------------------------------

module CgParallel(
	staticGranHdr,staticParHdr,
	granFetchAndReschedule, granYield,
	doGranAllocate
  ) where

import CgMonad
import CgCallConv
import Id
import Cmm
import StaticFlags
import Outputable

staticParHdr :: [CmmLit]
-- Parallel header words in a static closure
staticParHdr = []

--------------------------------------------------------
--		GranSim stuff
--------------------------------------------------------

staticGranHdr :: [CmmLit]
-- Gransim header words in a static closure
staticGranHdr = []

doGranAllocate :: CmmExpr -> Code	
-- macro DO_GRAN_ALLOCATE
doGranAllocate hp 
  | not opt_GranMacros = nopC
  | otherwise	       = panic "doGranAllocate"



-------------------------
granFetchAndReschedule :: [(Id,GlobalReg)]  -- Live registers
		       -> Bool           	-- Node reqd?
		       -> Code
-- Emit code for simulating a fetch and then reschedule.
granFetchAndReschedule regs node_reqd
  | opt_GranMacros && (node `elem` map snd regs || node_reqd)
  = do { fetch
       ; reschedule liveness node_reqd }
  | otherwise
  = nopC
  where
    liveness = mkRegLiveness regs 0 0

fetch = panic "granFetch"
	-- Was: absC (CMacroStmt GRAN_FETCH [])
	--HWL: generate GRAN_FETCH macro for GrAnSim
 	--     currently GRAN_FETCH and GRAN_FETCH_AND_RESCHEDULE are miai

reschedule liveness node_reqd = panic "granReschedule"
	-- Was: absC  (CMacroStmt GRAN_RESCHEDULE [
  	--                mkIntCLit (I# (word2Int# liveness_mask)), 
    	--		  mkIntCLit (if node_reqd then 1 else 0)])
    

-------------------------
-- The @GRAN_YIELD@ macro is taken from JSM's  code for Concurrent Haskell. It
-- allows to context-switch at  places where @node@ is  not alive (it uses the
-- @Continue@ rather  than the @EnterNodeCode@  function in the  RTS). We emit
-- this kind of macro at the beginning of the following kinds of basic bocks:
-- \begin{itemize}
--  \item Slow entry code where node is not alive (see @CgClosure.lhs@). Normally 
--        we use @fetchAndReschedule@ at a slow entry code.
--  \item Fast entry code (see @CgClosure.lhs@).
--  \item Alternatives in case expressions (@CLabelledCode@ structures), provided
--        that they are not inlined (see @CgCases.lhs@). These alternatives will 
--        be turned into separate functions.

granYield :: [(Id,GlobalReg)]   -- Live registers
          -> Bool               -- Node reqd?
          -> Code 

granYield regs node_reqd
  | opt_GranMacros && node_reqd = yield liveness
  | otherwise			= nopC
  where
     liveness = mkRegLiveness regs 0 0

yield liveness = panic "granYield"
	-- Was : absC (CMacroStmt GRAN_YIELD 
        --                  [mkIntCLit (I# (word2Int# liveness_mask))])



%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[AsmAssemble]{Assemble instructions into memory}

\begin{code}
module AsmAssemble ( asmAssemble ) where

#include "HsVersions.h"

import MachMisc		( Instr(..) )
--import PprMach		( pprInstr )	-- Just for debugging
--import RegAllocInfo

import FiniteMap	( FiniteMap, lookupFM, listToFM, filterFM )
import Outputable
import CLabel		( CLabel, pprCLabel, isAsmTemp )

import Foreign		( Ptr, Word8, plusPtr, nullPtr, poke, mallocBytes )
import List		( mapAccumL )
\end{code}

This is the generic assembler.  It assembles code into memory, knowing
not very much at all about instructions.  For simplicity a 2 pass
scheme is used.

\begin{code}
asmAssemble :: FiniteMap CLabel (Ptr Word8)		-- incoming address map
	    -> [[Instr]]				-- to assemble
	    -> IO (FiniteMap CLabel (Ptr Word8))	-- contribs to addr map
asmAssemble in_map instrss
   = do 
        -- FIRST PASS: find out the insn lengths
        let instrs = concat instrss
        let objects = map (assembleInstr nullPtr Nothing) instrs
        -- Extract the (label,offset) pairs for any labels defined in it
        let (tot_len, maybe_label_offsets)
                = mapAccumL getOffset 0 (zip instrs objects)
        -- Now we know the size of the output; malloc accordingly
        base_addr
           <- mallocBytes tot_len
        -- Build an env to map all local labels to their addresses
        let local_label_env
               = listToFM [(lab, base_addr `plusPtr` off) 
                            | Just (lab,off) <- maybe_label_offsets]
        
        -- SECOND PASS: assemble for real
        let find_label :: CLabel -> Ptr Word8
            find_label lab
               = case lookupFM local_label_env lab of
                    Just xx -> xx
                    Nothing -> case lookupFM in_map lab of
                                  Just yy -> yy
                                  Nothing -> pprPanic "asmAssemble1: can't find" 
                                                      (pprCLabel lab)
        let (_, final_bytess)
               = mapAccumL (doOneInsn find_label) base_addr instrs

        -- We now have the final bytes; blast 'em into memory
        pokeList base_addr (concat final_bytess)

        -- Remove labels of only local scope from the local label env
        let clean_label_env
               = filterFM (\k e -> not (isAsmTemp k)) local_label_env

        return clean_label_env

pokeList :: Ptr Word8 -> [Word8] -> IO ()
pokeList addr []     = return ()
pokeList addr (b:bs) = poke addr b >> pokeList (addr `plusPtr` 1) bs
   

                
doOneInsn :: (CLabel -> Ptr Word8) -> Ptr Word8 -> Instr -> (Ptr Word8, [Word8])
doOneInsn find_label addr insn
   = let bytes = assembleInstr addr (Just find_label) insn
     in  (addr `plusPtr` (length bytes), bytes)


getOffset :: Int -> (Instr,[Word8]) -> (Int, Maybe (CLabel,Int))
getOffset curr_off (LABEL l, bytes)
  = (curr_off + length bytes, Just (l, curr_off))
getOffset  curr_off (not_label, bytes)
  = (curr_off + length bytes, Nothing)


assembleInstr :: Ptr Word8 -> Maybe (CLabel -> Ptr Word8) -> Instr -> [Word8]
assembleInstr = undefined
\end{code}

%
% (c) The University of Glasgow 2000
%
\section[ByteCodeInstrs]{Bytecode instruction definitions}

\begin{code}
module ByteCodeInstr ( BCInstr(..), ProtoBCO(..), nameOfProtoBCO ) where

#include "HsVersions.h"

import Outputable
import Name		( Name )
import Id		( Id )
import CoreSyn
import PprCore		( pprCoreExpr, pprCoreAlt )
import Literal		( Literal )
import PrimRep		( PrimRep )
import DataCon		( DataCon )
import VarSet		( VarSet )

\end{code}

%************************************************************************
%*									*
\subsection{Bytecodes, and Outputery.}
%*									*
%************************************************************************

\begin{code}

data ProtoBCO a 
   = ProtoBCO a 			-- name, in some sense
              [BCInstr] 		-- instrs
					-- what the BCO came from
              (Either [AnnAlt Id VarSet]
                      (AnnExpr Id VarSet))

nameOfProtoBCO (ProtoBCO nm insns origin) = nm

type LocalLabel = Int

data BCInstr
   -- Messing with the stack
   = ARGCHECK  Int
   -- Push locals (existing bits of the stack)
   | PUSH_L    Int{-offset-}
   | PUSH_LL   Int Int{-2 offsets-}
   | PUSH_LLL  Int Int Int{-3 offsets-}
   -- Push a ptr
   | PUSH_G    Name
   -- Push an alt continuation
   | PUSH_AS   Name PrimRep	-- push alts and BCO_ptr_ret_info
				-- PrimRep so we know which itbl
   -- Pushing literals
   | PUSH_UBX  Literal	Int 
                        -- push this int/float/double, NO TAG, on the stack
			-- Int is # of words to copy from literal pool
   | PUSH_TAG  Int      -- push this tag on the stack

   | SLIDE     Int{-this many-} Int{-down by this much-}
   -- To do with the heap
   | ALLOC     Int	-- make an AP_UPD with this many payload words, zeroed
   | MKAP      Int{-ptr to AP_UPD is this far down stack-} Int{-# words-}
   | UNPACK    Int	-- unpack N ptr words from t.o.s Constr
   | UPK_TAG   Int Int Int
			-- unpack N non-ptr words from offset M in constructor
			-- K words down the stack
   | PACK      DataCon Int
			-- after assembly, the DataCon is an index into the
			-- itbl array
   -- For doing case trees
   | LABEL     LocalLabel
   | TESTLT_I  Int    LocalLabel
   | TESTEQ_I  Int    LocalLabel
   | TESTLT_F  Float  LocalLabel
   | TESTEQ_F  Float  LocalLabel
   | TESTLT_D  Double LocalLabel
   | TESTEQ_D  Double LocalLabel

   -- The Int value is a constructor number and therefore
   -- stored in the insn stream rather than as an offset into
   -- the literal pool.
   | TESTLT_P  Int    LocalLabel
   | TESTEQ_P  Int    LocalLabel

   | CASEFAIL
   -- To Infinity And Beyond
   | ENTER
   | RETURN	PrimRep
		-- unboxed value on TOS.  Use tag to find underlying ret itbl
		-- and return as per that.


instance Outputable BCInstr where
   ppr (ARGCHECK n)          = text "ARGCHECK" <+> int n
   ppr (PUSH_L offset)       = text "PUSH_L  " <+> int offset
   ppr (PUSH_LL o1 o2)       = text "PUSH_LL " <+> int o1 <+> int o2
   ppr (PUSH_LLL o1 o2 o3)   = text "PUSH_LLL" <+> int o1 <+> int o2 <+> int o3
   ppr (PUSH_G nm)           = text "PUSH_G  " <+> ppr nm
   ppr (PUSH_AS nm pk)       = text "PUSH_AS " <+> ppr nm <+> ppr pk
   ppr (PUSH_UBX lit nw)     = text "PUSH_UBX" <+> parens (int nw) <+> ppr lit
   ppr (PUSH_TAG n)          = text "PUSH_TAG" <+> int n
   ppr (SLIDE n d)           = text "SLIDE   " <+> int n <+> int d
   ppr (ALLOC sz)            = text "ALLOC   " <+> int sz
   ppr (MKAP offset sz)      = text "MKAP    " <+> int sz <+> text "words," 
                                               <+> int offset <+> text "stkoff"
   ppr (UNPACK sz)           = text "UNPACK  " <+> int sz
   ppr (UPK_TAG n m k)       = text "UPK_TAG " <+> int n <> text "words" 
                                               <+> int m <> text "conoff"
                                               <+> int k <> text "stkoff"
   ppr (PACK dcon sz)        = text "PACK    " <+> ppr dcon <+> ppr sz
   ppr (LABEL     lab)       = text "__"       <> int lab <> colon
   ppr (TESTLT_I  i lab)     = text "TESTLT_I" <+> int i <+> text "__" <> int lab
   ppr (TESTEQ_I  i lab)     = text "TESTEQ_I" <+> int i <+> text "__" <> int lab
   ppr (TESTLT_F  f lab)     = text "TESTLT_F" <+> float f <+> text "__" <> int lab
   ppr (TESTEQ_F  f lab)     = text "TESTEQ_F" <+> float f <+> text "__" <> int lab
   ppr (TESTLT_D  d lab)     = text "TESTLT_D" <+> double d <+> text "__" <> int lab
   ppr (TESTEQ_D  d lab)     = text "TESTEQ_D" <+> double d <+> text "__" <> int lab
   ppr (TESTLT_P  i lab)     = text "TESTLT_P" <+> int i <+> text "__" <> int lab
   ppr (TESTEQ_P  i lab)     = text "TESTEQ_P" <+> int i <+> text "__" <> int lab
   ppr CASEFAIL              = text "CASEFAIL"
   ppr ENTER                 = text "ENTER"
   ppr (RETURN pk)           = text "RETURN  " <+> ppr pk

instance Outputable a => Outputable (ProtoBCO a) where
   ppr (ProtoBCO name instrs origin)
      = (text "ProtoBCO" <+> ppr name <> colon)
        $$ nest 6 (vcat (map ppr instrs))
        $$ case origin of
              Left alts -> vcat (map (pprCoreAlt.deAnnAlt) alts)
              Right rhs -> pprCoreExpr (deAnnotate rhs)
\end{code}

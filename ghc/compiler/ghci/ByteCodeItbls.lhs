%
% (c) The University of Glasgow 2000
%
\section[ByteCodeItbls]{Generate infotables for interpreter-made bytecodes}

\begin{code}
module ByteCodeItbls ( ItblEnv, mkITbls ) where

#include "HsVersions.h"

import Name		( Name, getName )
import FiniteMap	( FiniteMap, listToFM, emptyFM, plusFM )
import Type		( typePrimRep )
import DataCon		( DataCon, dataConRepArgTys )
import TyCon		( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import Constants	( mIN_SIZE_NonUpdHeapObject )
import ClosureInfo	( mkVirtHeapOffsets )
import FastString	( FastString(..) )

import Foreign		( Storable(..), Word8, Word16, Word32, Ptr(..), 
			  malloc, castPtr, plusPtr )
import Addr		( addrToInt )
import Bits		( Bits(..), shiftR )

import PrelBase		( Int(..) )
import PrelAddr		( Addr(..) )
import PrelIOBase	( IO(..) )

\end{code}

%************************************************************************
%*									*
\subsection{Manufacturing of info tables for DataCons}
%*									*
%************************************************************************

\begin{code}

type ItblEnv = FiniteMap Name (Ptr StgInfoTable)

#if __GLASGOW_HASKELL__ <= 408
type ItblPtr = Addr
#else
type ItblPtr = Ptr StgInfoTable
#endif

-- Make info tables for the data decls in this module
mkITbls :: [TyCon] -> IO ItblEnv
mkITbls [] = return emptyFM
mkITbls (tc:tcs) = do itbls  <- mkITbl tc
                      itbls2 <- mkITbls tcs
                      return (itbls `plusFM` itbls2)

mkITbl :: TyCon -> IO ItblEnv
mkITbl tc
   | not (isDataTyCon tc) 
   = return emptyFM
   | n == length dcs  -- paranoia; this is an assertion.
   = make_constr_itbls dcs
     where
        dcs = tyConDataCons tc
        n   = tyConFamilySize tc

cONSTR :: Int
cONSTR = 1  -- as defined in ghc/includes/ClosureTypes.h

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: [DataCon] -> IO ItblEnv
make_constr_itbls cons
   | length cons <= 8
   = do is <- mapM mk_vecret_itbl (zip cons [0..])
	return (listToFM is)
   | otherwise
   = do is <- mapM mk_dirret_itbl (zip cons [0..])
	return (listToFM is)
     where
        mk_vecret_itbl (dcon, conNo)
           = mk_itbl dcon conNo (vecret_entry conNo)
        mk_dirret_itbl (dcon, conNo)
           = mk_itbl dcon conNo stg_interp_constr_entry

        mk_itbl :: DataCon -> Int -> Addr -> IO (Name,ItblPtr)
        mk_itbl dcon conNo entry_addr
           = let (tot_wds, ptr_wds, _) 
                    = mkVirtHeapOffsets typePrimRep (dataConRepArgTys dcon)
                 ptrs  = ptr_wds
                 nptrs = tot_wds - ptr_wds
                 nptrs_really
                    | ptrs + nptrs >= mIN_SIZE_NonUpdHeapObject = nptrs
                    | otherwise = mIN_SIZE_NonUpdHeapObject - ptrs
                 itbl  = StgInfoTable {
                           ptrs  = fromIntegral ptrs, 
                           nptrs = fromIntegral nptrs_really,
                           tipe  = fromIntegral cONSTR,
                           srtlen = fromIntegral conNo,
                           code0 = fromIntegral code0, code1 = fromIntegral code1,
                           code2 = fromIntegral code2, code3 = fromIntegral code3,
                           code4 = fromIntegral code4, code5 = fromIntegral code5,
                           code6 = fromIntegral code6, code7 = fromIntegral code7 
                        }
                 -- Make a piece of code to jump to "entry_label".
                 -- This is the only arch-dependent bit.
                 -- On x86, if entry_label has an address 0xWWXXYYZZ,
                 -- emit   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
                 -- which is
                 -- B8 ZZ YY XX WW FF E0
                 (code0,code1,code2,code3,code4,code5,code6,code7)
                    = (0xB8, byte 0 entry_addr_w, byte 1 entry_addr_w, 
                             byte 2 entry_addr_w, byte 3 entry_addr_w, 
                       0xFF, 0xE0, 
                       0x90 {-nop-})

                 entry_addr_w :: Word32
                 entry_addr_w = fromIntegral (addrToInt entry_addr)
             in
                 do addr <- malloc
                    --putStrLn ("SIZE of itbl is " ++ show (sizeOf itbl))
                    --putStrLn ("# ptrs  of itbl is " ++ show ptrs)
                    --putStrLn ("# nptrs of itbl is " ++ show nptrs_really)
                    poke addr itbl
                    return (getName dcon, addr `plusPtr` 8)


byte :: Int -> Word32 -> Word32
byte 0 w = w .&. 0xFF
byte 1 w = (w `shiftR` 8) .&. 0xFF
byte 2 w = (w `shiftR` 16) .&. 0xFF
byte 3 w = (w `shiftR` 24) .&. 0xFF


vecret_entry 0 = stg_interp_constr1_entry
vecret_entry 1 = stg_interp_constr2_entry
vecret_entry 2 = stg_interp_constr3_entry
vecret_entry 3 = stg_interp_constr4_entry
vecret_entry 4 = stg_interp_constr5_entry
vecret_entry 5 = stg_interp_constr6_entry
vecret_entry 6 = stg_interp_constr7_entry
vecret_entry 7 = stg_interp_constr8_entry

-- entry point for direct returns for created constr itbls
foreign label "stg_interp_constr_entry" stg_interp_constr_entry :: Addr
-- and the 8 vectored ones
foreign label "stg_interp_constr1_entry" stg_interp_constr1_entry :: Addr
foreign label "stg_interp_constr2_entry" stg_interp_constr2_entry :: Addr
foreign label "stg_interp_constr3_entry" stg_interp_constr3_entry :: Addr
foreign label "stg_interp_constr4_entry" stg_interp_constr4_entry :: Addr
foreign label "stg_interp_constr5_entry" stg_interp_constr5_entry :: Addr
foreign label "stg_interp_constr6_entry" stg_interp_constr6_entry :: Addr
foreign label "stg_interp_constr7_entry" stg_interp_constr7_entry :: Addr
foreign label "stg_interp_constr8_entry" stg_interp_constr8_entry :: Addr





-- Ultra-minimalist version specially for constructors
data StgInfoTable = StgInfoTable {
   ptrs :: Word16,
   nptrs :: Word16,
   srtlen :: Word16,
   tipe :: Word16,
   code0, code1, code2, code3, code4, code5, code6, code7 :: Word8
}


instance Storable StgInfoTable where

   sizeOf itbl 
      = (sum . map (\f -> f itbl))
        [fieldSz ptrs, fieldSz nptrs, fieldSz srtlen, fieldSz tipe,
         fieldSz code0, fieldSz code1, fieldSz code2, fieldSz code3, 
         fieldSz code4, fieldSz code5, fieldSz code6, fieldSz code7]

   alignment itbl 
      = (sum . map (\f -> f itbl))
        [fieldAl ptrs, fieldAl nptrs, fieldAl srtlen, fieldAl tipe,
         fieldAl code0, fieldAl code1, fieldAl code2, fieldAl code3, 
         fieldAl code4, fieldAl code5, fieldAl code6, fieldAl code7]

   poke a0 itbl
      = do a1 <- store (ptrs   itbl) (castPtr a0)
           a2 <- store (nptrs  itbl) a1
           a3 <- store (tipe   itbl) a2
           a4 <- store (srtlen itbl) a3
           a5 <- store (code0  itbl) a4
           a6 <- store (code1  itbl) a5
           a7 <- store (code2  itbl) a6
           a8 <- store (code3  itbl) a7
           a9 <- store (code4  itbl) a8
           aA <- store (code5  itbl) a9
           aB <- store (code6  itbl) aA
           aC <- store (code7  itbl) aB
           return ()

   peek a0
      = do (a1,ptrs)   <- load (castPtr a0)
           (a2,nptrs)  <- load a1
           (a3,tipe)   <- load a2
           (a4,srtlen) <- load a3
           (a5,code0)  <- load a4
           (a6,code1)  <- load a5
           (a7,code2)  <- load a6
           (a8,code3)  <- load a7
           (a9,code4)  <- load a8
           (aA,code5)  <- load a9
           (aB,code6)  <- load aA
           (aC,code7)  <- load aB
           return StgInfoTable { ptrs = ptrs, nptrs = nptrs, 
                                 srtlen = srtlen, tipe = tipe,
                                 code0 = code0, code1 = code1, code2 = code2,
                                 code3 = code3, code4 = code4, code5 = code5,
                                 code6 = code6, code7 = code7 }

fieldSz :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

fieldAl :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldAl sel x = alignment (sel x)

store :: Storable a => a -> Ptr a -> IO (Ptr b)
store x addr = do poke addr x
                  return (castPtr (addr `plusPtr` sizeOf x))

load :: Storable a => Ptr a -> IO (Ptr b, a)
load addr = do x <- peek addr
               return (castPtr (addr `plusPtr` sizeOf x), x)

\end{code}

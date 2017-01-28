{-# LANGUAGE CPP, ScopedTypeVariables, DoAndIfThenElse, NondecreasingIndentation, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | A disassembler for ByteCode objects as used by GHCi.
module GHC.Disassembler (
    toBytes,
    disassemble,
    BCI(..) ) where

import Data.Word
import Data.Int
import Data.Bits
import Data.Foldable    ( Foldable )
import Data.Traversable ( Traversable )

#include "ghcautoconf.h"
#include "rts/Bytecodes.h"

-- | Converts the first @n@ bytes of this list of Words to a ByteString.
toBytes :: Word -> [Word] -> ByteString
toBytes n =
    BS.take (fromIntegral n) .
    toLazyByteString .
    mconcat .
    map (wordHost . fromIntegral)

-- | Given a list of pointers, a list of literals and a ByteString containing
-- byte code instructions, disassembles them into a list of byte code instructions.
disassemble :: forall box. [box] -> [Word] -> ByteString -> [BCI box]
disassemble ptrs lits = runGet $ do
    -- Ignore length tag. Needs to be skipped with GHC versions with
    -- http://hackage.haskell.org/trac/ghc/ticket/7518 included
    _ <- getWord16host
#if SIZEOF_VOID_P == 8
    _ <- getWord16host
    _ <- getWord16host
#endif
    nextInst
  where
    getLiteral :: Get Word
    getLiteral = ((!!) lits) . fromIntegral <$> getWord16host

    getLiterals = do
        p <- fromIntegral <$> getWord16host
        n <- fromIntegral <$> getWord16host
        return $ take n (drop p lits)

    getAddr :: Int -> box
    getAddr p = ptrs !! p

    getPtr :: Get box
    getPtr = getAddr . fromIntegral <$> getWord16host

    nextInst :: Get [BCI box]
    nextInst = do
        e <- isEmpty
        if e then return [] else do
        w <- getWord16host
        let large = 0 /= w .&. 0x8000

        let getLarge = if large then getWordhost else fromIntegral `fmap` getWord16host
        let getLargeInt = if large then getInthost else fromIntegral `fmap` getInt16host

        i <- case w .&. 0xff of
            bci_STKCHECK -> do
                n <- getLarge
                return $ BCISTKCHECK (n + 1)
            bci_PUSH_L -> do
                o1 <- getWord16host
                return $ BCIPUSH_L o1
            bci_PUSH_LL -> do
                o1 <- getWord16host
                o2 <- getWord16host
                return $ BCIPUSH_LL o1 o2
            bci_PUSH_LLL -> do
                o1 <- getWord16host
                o2 <- getWord16host
                o3 <- getWord16host
                return $ BCIPUSH_LLL o1 o2 o3
            bci_PUSH_G -> do
                p <- getPtr
                return $ BCIPUSH_G p
            bci_PUSH_ALTS -> do
                p <- getPtr
                return $ BCIPUSH_ALTS p
            bci_PUSH_ALTS_P -> do
                p <- getPtr
                return $ BCIPUSH_ALTS_P p
            bci_PUSH_ALTS_N -> do
                p <- getPtr
                return $ BCIPUSH_ALTS_N p
            bci_PUSH_ALTS_F -> do
                p <- getPtr
                return $ BCIPUSH_ALTS_F p
            bci_PUSH_ALTS_D -> do
                p <- getPtr
                return $ BCIPUSH_ALTS_D p
            bci_PUSH_ALTS_L -> do
                p <- getPtr
                return $ BCIPUSH_ALTS_L p
            bci_PUSH_ALTS_V -> do
                p <- getPtr
                return $ BCIPUSH_ALTS_V p
            bci_PUSH_UBX -> do
                ubx_lits <- getLiterals
                return $ BCIPUSH_UBX ubx_lits
            bci_PUSH_APPLY_N -> do
                return BCIPUSH_APPLY_N
            bci_PUSH_APPLY_F -> do
                return BCIPUSH_APPLY_F
            bci_PUSH_APPLY_D -> do
                return BCIPUSH_APPLY_D
            bci_PUSH_APPLY_L -> do
                return BCIPUSH_APPLY_L
            bci_PUSH_APPLY_V -> do
                return BCIPUSH_APPLY_V
            bci_PUSH_APPLY_P -> do
                return BCIPUSH_APPLY_P
            bci_PUSH_APPLY_PP -> do
                return BCIPUSH_APPLY_PP
            bci_PUSH_APPLY_PPP -> do
                return BCIPUSH_APPLY_PPP
            bci_PUSH_APPLY_PPPP -> do
                return BCIPUSH_APPLY_PPPP
            bci_PUSH_APPLY_PPPPP -> do
                return BCIPUSH_APPLY_PPPPP
            bci_PUSH_APPLY_PPPPPP -> do
                return BCIPUSH_APPLY_PPPPPP
            bci_SLIDE -> do
                p <- getWord16host
                n <- getWord16host
                return $ BCISLIDE p n
            bci_ALLOC_AP -> do
                n <- getWord16host
                return $ BCIALLOC_AP n
            bci_ALLOC_AP_NOUPD -> do
                n <- getWord16host
                return $ BCIALLOC_AP_NOUPD n
            bci_ALLOC_PAP -> do
                a <- getWord16host
                n <- getWord16host
                return $ BCIALLOC_PAP a n
            bci_MKAP -> do
                n <- getWord16host
                s <- getWord16host
                return $ BCIMKAP n s
            bci_MKPAP -> do
                n <- getWord16host
                s <- getWord16host
                return $ BCIMKPAP n s
            bci_UNPACK -> do
                n <- getWord16host
                return $ BCIUNPACK n
            bci_PACK -> do
                p <- getLiteral
                n <- getWord16host
                return $ BCIPACK p n
            bci_TESTLT_I -> do
                d <- getLargeInt
                t <- getLargeInt
                return $ BCITESTLT_I d t
            bci_TESTEQ_I -> do
                d <- getLargeInt
                t <- getLargeInt
                return $ BCITESTEQ_I d t
            bci_TESTLT_W -> do
                d <- getLarge
                t <- getLargeInt
                return $ BCITESTLT_W d t
            bci_TESTEQ_W -> do
                d <- getLarge
                t <- getLargeInt
                return $ BCITESTEQ_W d t
            bci_TESTLT_F -> do
                d <- getLarge
                t <- getLargeInt
                return $ BCITESTLT_F d t
            bci_TESTEQ_F -> do
                d <- getLarge
                t <- getLargeInt
                return $ BCITESTEQ_F d t
            bci_TESTLT_D -> do
                d <- getLarge
                t <- getLargeInt
                return $ BCITESTLT_D d t
            bci_TESTEQ_D -> do
                d <- getLarge
                t <- getLargeInt
                return $ BCITESTEQ_D d t
            bci_TESTLT_P -> do
                d <- getWord16host
                t <- getLargeInt
                return $ BCITESTLT_P d t
            bci_TESTEQ_P -> do
                d <- getWord16host
                t <- getLargeInt
                return $ BCITESTEQ_P d t
            bci_CASEFAIL -> do
                return BCICASEFAIL
            bci_JMP -> do
                return BCIJMP
            bci_CCALL -> do
                p <- getLiteral
                return $ BCICCALL p
            bci_SWIZZLE -> do
                p <- getWord16host
                n <- getInt16host
                return $ BCISWIZZLE p n
            bci_ENTER -> do
                return BCIENTER
            bci_RETURN -> do
                return BCIRETURN
            bci_RETURN_P -> do
                return BCIRETURN_P
            bci_RETURN_N -> do
                return BCIRETURN_N
            bci_RETURN_F -> do
                return BCIRETURN_F
            bci_RETURN_D -> do
                return BCIRETURN_D
            bci_RETURN_L -> do
                return BCIRETURN_L
            bci_RETURN_V -> do
                return BCIRETURN_V
            bci_BRK_FUN -> do
                _ <- getWord16host
                _ <- getWord16host
                _ <- getWord16host
                return BCIBRK_FUN
            x -> error $ "Unknown opcode " ++ show x
        (i :) `fmap` nextInst


-- | The various byte code instructions that GHCi supports.
data BCI box
    = BCISTKCHECK Word
    | BCIPUSH_L Word16
    | BCIPUSH_LL Word16 Word16
    | BCIPUSH_LLL Word16 Word16 Word16
    | BCIPUSH_G box
    | BCIPUSH_ALTS box
    | BCIPUSH_ALTS_P box
    | BCIPUSH_ALTS_N box
    | BCIPUSH_ALTS_F box
    | BCIPUSH_ALTS_D box
    | BCIPUSH_ALTS_L box
    | BCIPUSH_ALTS_V box
    | BCIPUSH_UBX [Word]
    | BCIPUSH_APPLY_N
    | BCIPUSH_APPLY_F
    | BCIPUSH_APPLY_D
    | BCIPUSH_APPLY_L
    | BCIPUSH_APPLY_V
    | BCIPUSH_APPLY_P
    | BCIPUSH_APPLY_PP
    | BCIPUSH_APPLY_PPP
    | BCIPUSH_APPLY_PPPP
    | BCIPUSH_APPLY_PPPPP
    | BCIPUSH_APPLY_PPPPPP
/*     | BCIPUSH_APPLY_PPPPPPP */
    | BCISLIDE Word16 Word16
    | BCIALLOC_AP Word16
    | BCIALLOC_AP_NOUPD Word16
    | BCIALLOC_PAP Word16 Word16
    | BCIMKAP Word16 Word16
    | BCIMKPAP Word16 Word16
    | BCIUNPACK Word16
    | BCIPACK Word Word16
    | BCITESTLT_I Int Int
    | BCITESTEQ_I Int Int
    | BCITESTLT_F Word Int
    | BCITESTEQ_F Word Int
    | BCITESTLT_D Word Int
    | BCITESTEQ_D Word Int
    | BCITESTLT_P Word16 Int
    | BCITESTEQ_P Word16 Int
    | BCICASEFAIL
    | BCIJMP
    | BCICCALL Word
    | BCISWIZZLE Word16 Int16
    | BCIENTER
    | BCIRETURN
    | BCIRETURN_P
    | BCIRETURN_N
    | BCIRETURN_F
    | BCIRETURN_D
    | BCIRETURN_L
    | BCIRETURN_V
    | BCIBRK_FUN -- ^ We do not parse this opcode's arguments
    | BCITESTLT_W Word Int
    | BCITESTEQ_W Word Int
    deriving (Show, Functor, Traversable, Foldable)

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: literals
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Lit (
    cgLit, mkSimpleLit,
    newStringCLit, newByteStringCLit
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Env
import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Utils

import GHC.Types.Literal
import GHC.Types.RepType( runtimeRepPrimRep )
import GHC.Builtin.Types ( unitDataConId )
import GHC.Core.TyCon
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (ord)

newStringCLit :: String -> FCode CmmLit
-- ^ Make a global definition for the string,
-- and return its label
newStringCLit str = newByteStringCLit (BS8.pack str)

newByteStringCLit :: ByteString -> FCode CmmLit
newByteStringCLit bytes
  = do  { uniq <- newUnique
        ; let (lit, decl) = mkByteStringCLit (mkStringLitLabel uniq) bytes
        ; emitDecl decl
        ; return lit }

cgLit :: Literal -> FCode CmmExpr
cgLit (LitString s) =
  CmmLit <$> newByteStringCLit s
 -- not unpackFS; we want the UTF-8 byte stream.
cgLit (LitRubbish _ rep) =
  case expectOnly "cgLit" prim_reps of -- Note [Post-unarisation invariants]
    BoxedRep _  -> idInfoToAmode <$> getCgIdInfo unitDataConId
    AddrRep     -> cgLit LitNullAddr
    VecRep n elem -> do
      platform <- getPlatform
      let elem_lit = mkSimpleLit platform (num_rep_lit (primElemRepToPrimRep elem))
      pure (CmmLit (CmmVec (replicate n elem_lit)))
    prep        -> cgLit (num_rep_lit prep)
  where
      prim_reps = runtimeRepPrimRep (text "cgLit") rep

      num_rep_lit IntRep    = mkLitIntUnchecked 0
      num_rep_lit Int8Rep   = mkLitInt8Unchecked 0
      num_rep_lit Int16Rep  = mkLitInt16Unchecked 0
      num_rep_lit Int32Rep  = mkLitInt32Unchecked 0
      num_rep_lit Int64Rep  = mkLitInt64Unchecked 0
      num_rep_lit WordRep   = mkLitWordUnchecked 0
      num_rep_lit Word8Rep  = mkLitWord8Unchecked 0
      num_rep_lit Word16Rep = mkLitWord16Unchecked 0
      num_rep_lit Word32Rep = mkLitWord32Unchecked 0
      num_rep_lit Word64Rep = mkLitWord64Unchecked 0
      num_rep_lit FloatRep  = LitFloat 0
      num_rep_lit DoubleRep = LitDouble 0
      num_rep_lit other     = pprPanic "num_rep_lit: Not a num lit" (ppr other)

cgLit other_lit = do
  platform <- getPlatform
  pure (CmmLit (mkSimpleLit platform other_lit))

mkSimpleLit :: Platform -> Literal -> CmmLit
mkSimpleLit platform = \case
   (LitChar   c)                -> CmmInt (fromIntegral (ord c))
                                          (wordWidth platform)
   LitNullAddr                  -> zeroCLit platform
   (LitNumber LitNumInt i)      -> CmmInt i (wordWidth platform)
   (LitNumber LitNumInt8 i)     -> CmmInt i W8
   (LitNumber LitNumInt16 i)    -> CmmInt i W16
   (LitNumber LitNumInt32 i)    -> CmmInt i W32
   (LitNumber LitNumInt64 i)    -> CmmInt i W64
   (LitNumber LitNumWord i)     -> CmmInt i (wordWidth platform)
   (LitNumber LitNumWord8 i)    -> CmmInt i W8
   (LitNumber LitNumWord16 i)   -> CmmInt i W16
   (LitNumber LitNumWord32 i)   -> CmmInt i W32
   (LitNumber LitNumWord64 i)   -> CmmInt i W64
   (LitFloat r)                 -> CmmFloat r W32
   (LitDouble r)                -> CmmFloat r W64
   (LitLabel fs fod)
     -> let -- TODO: Literal labels might not actually be in the current package...
            labelSrc = ForeignLabelInThisPackage
        in CmmLabel (mkForeignLabel fs labelSrc fod)
   other -> pprPanic "mkSimpleLit" (ppr other)

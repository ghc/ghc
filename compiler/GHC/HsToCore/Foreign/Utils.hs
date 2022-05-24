module GHC.HsToCore.Foreign.Utils
  ( Binding
  , getPrimTyOf
  , primTyDescChar
  )
where

import GHC.Prelude

import GHC.Platform

import GHC.Tc.Utils.TcType

import GHC.Core (CoreExpr)
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep

import GHC.Types.Id
import GHC.Types.RepType

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

type Binding = (Id, CoreExpr) -- No rec/nonrec structure;
                              -- the occurrence analyser will sort it all out

-- This function returns the primitive type associated with the boxed
-- type argument to a foreign export (eg. Int ==> Int#).
getPrimTyOf :: Type -> UnaryType
getPrimTyOf ty
  | isBoolTy rep_ty = intPrimTy
  -- Except for Bool, the types we are interested in have a single constructor
  -- with a single primitive-typed argument (see TcType.legalFEArgTyCon).
  | otherwise =
  case splitDataProductType_maybe rep_ty of
     Just (_, _, data_con, [Scaled _ prim_ty]) ->
        assert (dataConSourceArity data_con == 1) $
        assertPpr (isUnliftedType prim_ty) (ppr prim_ty)
          -- NB: it's OK to call isUnliftedType here, as we don't allow
          -- representation-polymorphic types in foreign import/export declarations
        prim_ty
     _other -> pprPanic "getPrimTyOf" (ppr ty)
  where
        rep_ty = unwrapType ty

-- represent a primitive type as a Char, for building a string that
-- described the foreign function type.  The types are size-dependent,
-- e.g. 'W' is a signed 32-bit integer.
primTyDescChar :: Platform -> Type -> Char
primTyDescChar !platform ty
 | ty `eqType` unitTy = 'v'
 | otherwise
 = case typePrimRep1 (getPrimTyOf ty) of
     IntRep      -> signed_word
     WordRep     -> unsigned_word
     Int8Rep     -> 'B'
     Word8Rep    -> 'b'
     Int16Rep    -> 'S'
     Word16Rep   -> 's'
     Int32Rep    -> 'W'
     Word32Rep   -> 'w'
     Int64Rep    -> 'L'
     Word64Rep   -> 'l'
     AddrRep     -> 'p'
     FloatRep    -> 'f'
     DoubleRep   -> 'd'
     _           -> pprPanic "primTyDescChar" (ppr ty)
  where
    (signed_word, unsigned_word) = case platformWordSize platform of
      PW4 -> ('W','w')
      PW8 -> ('L','l')

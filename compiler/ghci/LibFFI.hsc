-----------------------------------------------------------------------------
--
-- libffi bindings
--
-- (c) The University of Glasgow 2008
--
-----------------------------------------------------------------------------

#include <ffi.h>

module LibFFI (
  ForeignCallToken,
  prepForeignCall
 ) where

import TyCon
import ForeignCall
import Panic
import DynFlags

import Control.Monad
import Foreign
import Foreign.C

----------------------------------------------------------------------------

type ForeignCallToken = C_ffi_cif

prepForeignCall
    :: DynFlags
    -> CCallConv
    -> [PrimRep]                        -- arg types
    -> PrimRep                          -- result type
    -> IO (Ptr ForeignCallToken)        -- token for making calls
                                        -- (must be freed by caller)
prepForeignCall dflags cconv arg_types result_type
  = do
    let n_args = length arg_types
    arg_arr <- mallocArray n_args
    let init_arg ty n = pokeElemOff arg_arr n (primRepToFFIType dflags ty)
    zipWithM_ init_arg arg_types [0..]
    cif <- mallocBytes (#const sizeof(ffi_cif))
    let abi = convToABI cconv
    let res_ty = primRepToFFIType dflags result_type
    r <- ffi_prep_cif cif abi (fromIntegral n_args) res_ty arg_arr
    if (r /= fFI_OK)
       then throwGhcExceptionIO (InstallationError
                                     ("prepForeignCallFailed: " ++ show r))
       else return cif

convToABI :: CCallConv -> C_ffi_abi
convToABI CCallConv   = fFI_DEFAULT_ABI
#if defined(mingw32_HOST_OS) && defined(i386_HOST_ARCH)
convToABI StdCallConv = fFI_STDCALL
#endif
-- unknown conventions are mapped to the default, (#3336)
convToABI _           = fFI_DEFAULT_ABI

-- c.f. DsForeign.primTyDescChar
primRepToFFIType :: DynFlags -> PrimRep -> Ptr C_ffi_type
primRepToFFIType dflags r
  = case r of
     VoidRep     -> ffi_type_void
     IntRep      -> signed_word
     WordRep     -> unsigned_word
     Int64Rep    -> ffi_type_sint64
     Word64Rep   -> ffi_type_uint64
     AddrRep     -> ffi_type_pointer
     FloatRep    -> ffi_type_float
     DoubleRep   -> ffi_type_double
     _           -> panic "primRepToFFIType"
  where
    (signed_word, unsigned_word)
       | wORD_SIZE dflags == 4  = (ffi_type_sint32, ffi_type_uint32)
       | wORD_SIZE dflags == 8  = (ffi_type_sint64, ffi_type_uint64)
       | otherwise              = panic "primTyDescChar"


data C_ffi_type
data C_ffi_cif

type C_ffi_status = (#type ffi_status)
type C_ffi_abi    = (#type ffi_abi)

foreign import ccall "&ffi_type_void"   ffi_type_void    :: Ptr C_ffi_type
--foreign import ccall "&ffi_type_uint8"  ffi_type_uint8   :: Ptr C_ffi_type
--foreign import ccall "&ffi_type_sint8"  ffi_type_sint8   :: Ptr C_ffi_type
--foreign import ccall "&ffi_type_uint16" ffi_type_uint16  :: Ptr C_ffi_type
--foreign import ccall "&ffi_type_sint16" ffi_type_sint16  :: Ptr C_ffi_type
foreign import ccall "&ffi_type_uint32" ffi_type_uint32  :: Ptr C_ffi_type
foreign import ccall "&ffi_type_sint32" ffi_type_sint32  :: Ptr C_ffi_type
foreign import ccall "&ffi_type_uint64" ffi_type_uint64  :: Ptr C_ffi_type
foreign import ccall "&ffi_type_sint64" ffi_type_sint64  :: Ptr C_ffi_type
foreign import ccall "&ffi_type_float"  ffi_type_float   :: Ptr C_ffi_type
foreign import ccall "&ffi_type_double" ffi_type_double  :: Ptr C_ffi_type
foreign import ccall "&ffi_type_pointer"ffi_type_pointer :: Ptr C_ffi_type

fFI_OK            :: C_ffi_status
fFI_OK            = (#const FFI_OK)
--fFI_BAD_ABI     :: C_ffi_status
--fFI_BAD_ABI     = (#const FFI_BAD_ABI)
--fFI_BAD_TYPEDEF :: C_ffi_status
--fFI_BAD_TYPEDEF = (#const FFI_BAD_TYPEDEF)

fFI_DEFAULT_ABI :: C_ffi_abi
fFI_DEFAULT_ABI = (#const FFI_DEFAULT_ABI)
#if defined(mingw32_HOST_OS) && defined(i386_HOST_ARCH)
fFI_STDCALL     :: C_ffi_abi
fFI_STDCALL     = (#const FFI_STDCALL)
#endif

-- ffi_status ffi_prep_cif(ffi_cif *cif,
--                         ffi_abi abi,
--                         unsigned int nargs,
--                         ffi_type *rtype,
--                         ffi_type **atypes);

foreign import ccall "ffi_prep_cif"
  ffi_prep_cif :: Ptr C_ffi_cif         -- cif
               -> C_ffi_abi             -- abi
               -> CUInt                 -- nargs
               -> Ptr C_ffi_type        -- result type
               -> Ptr (Ptr C_ffi_type)  -- arg types
               -> IO C_ffi_status

-- Currently unused:

-- void ffi_call(ffi_cif *cif,
--               void (*fn)(),
--               void *rvalue,
--               void **avalue);

-- foreign import ccall "ffi_call"
--   ffi_call :: Ptr C_ffi_cif             -- cif
--            -> FunPtr (IO ())            -- function to call
--            -> Ptr ()                    -- put result here
--            -> Ptr (Ptr ())              -- arg values
--            -> IO ()

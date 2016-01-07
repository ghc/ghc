-----------------------------------------------------------------------------
--
-- libffi bindings
--
-- (c) The University of Glasgow 2008
--
-----------------------------------------------------------------------------

#include <ffi.h>

{-# LANGUAGE CPP, DeriveGeneric, DeriveAnyClass #-}
module GHCi.FFI
  ( FFIType(..)
  , FFIConv(..)
  , C_ffi_cif
  , prepForeignCall
  , freeForeignCallInfo
  ) where

import Control.Exception
import Data.Binary
import GHC.Generics
import Foreign
import Foreign.C

data FFIType
  = FFIVoid
  | FFIPointer
  | FFIFloat
  | FFIDouble
  | FFISInt8
  | FFISInt16
  | FFISInt32
  | FFISInt64
  | FFIUInt8
  | FFIUInt16
  | FFIUInt32
  | FFIUInt64
  deriving (Show, Generic, Binary)

data FFIConv
  = FFICCall
  | FFIStdCall
  deriving (Show, Generic, Binary)


prepForeignCall
    :: FFIConv
    -> [FFIType]          -- arg types
    -> FFIType            -- result type
    -> IO (Ptr C_ffi_cif) -- token for making calls (must be freed by caller)

prepForeignCall cconv arg_types result_type = do
  let n_args = length arg_types
  arg_arr <- mallocArray n_args
  pokeArray arg_arr (map ffiType arg_types)
  cif <- mallocBytes (#const sizeof(ffi_cif))
  let abi = convToABI cconv
  r <- ffi_prep_cif cif abi (fromIntegral n_args) (ffiType result_type) arg_arr
  if (r /= fFI_OK)
     then throwIO (ErrorCall ("prepForeignCallFailed: " ++ show r))
     else return (castPtr cif)

freeForeignCallInfo :: Ptr C_ffi_cif -> IO ()
freeForeignCallInfo p = do
  free ((#ptr ffi_cif, arg_types) p)
  free p

convToABI :: FFIConv -> C_ffi_abi
convToABI FFICCall  = fFI_DEFAULT_ABI
#if defined(mingw32_HOST_OS) && defined(i386_HOST_ARCH)
convToABI FFIStdCall = fFI_STDCALL
#endif
-- unknown conventions are mapped to the default, (#3336)
convToABI _           = fFI_DEFAULT_ABI

ffiType :: FFIType -> Ptr C_ffi_type
ffiType FFIVoid     = ffi_type_void
ffiType FFIPointer  = ffi_type_pointer
ffiType FFIFloat    = ffi_type_float
ffiType FFIDouble   = ffi_type_double
ffiType FFISInt8    = ffi_type_sint8
ffiType FFISInt16   = ffi_type_sint16
ffiType FFISInt32   = ffi_type_sint32
ffiType FFISInt64   = ffi_type_sint64
ffiType FFIUInt8    = ffi_type_uint8
ffiType FFIUInt16   = ffi_type_uint16
ffiType FFIUInt32   = ffi_type_uint32
ffiType FFIUInt64   = ffi_type_uint64

data C_ffi_type
data C_ffi_cif

type C_ffi_status = (#type ffi_status)
type C_ffi_abi    = (#type ffi_abi)

foreign import ccall "&ffi_type_void"   ffi_type_void    :: Ptr C_ffi_type
foreign import ccall "&ffi_type_uint8"  ffi_type_uint8   :: Ptr C_ffi_type
foreign import ccall "&ffi_type_sint8"  ffi_type_sint8   :: Ptr C_ffi_type
foreign import ccall "&ffi_type_uint16" ffi_type_uint16  :: Ptr C_ffi_type
foreign import ccall "&ffi_type_sint16" ffi_type_sint16  :: Ptr C_ffi_type
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

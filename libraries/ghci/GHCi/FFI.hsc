-----------------------------------------------------------------------------
--
-- libffi bindings
--
-- (c) The University of Glasgow 2008
--
-----------------------------------------------------------------------------

{- Note [FFI for the JS-Backend]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The JS-backend does not use GHC's native rts, as such you might think that it
   doesn't require ghci. However, that is not true, because we need ghci in
   order to interoperate with iserv even if we do not use any of the FFI stuff
   in this file. So obviously we do not require libffi, but we still need to be
   able to build ghci in order for the JS-Backend to supply its own iserv
   interop solution. Thus we bite the bullet and wrap all the unneeded bits in a
   CPP conditional compilation blocks that detect the JS-backend. A necessary
   evil to be sure; notice that the only symbols remaining the JS_HOST_ARCH case
   are those that are explicitly exported by this module and set to error if
   they are every used.
-}

#if !defined(javascript_HOST_ARCH)
-- See Note [FFI_GO_CLOSURES workaround] in ghc_ffi.h
-- We can't include ghc_ffi.h here as we must build with stage0
#if defined(darwin_HOST_OS)
#if !defined(FFI_GO_CLOSURES)
#define FFI_GO_CLOSURES 0
#endif
#endif

#include <ffi.h>
#endif

{-# LANGUAGE CPP, DeriveGeneric, DeriveAnyClass #-}
module GHCi.FFI
  ( FFIType(..)
  , C_ffi_cif
  , prepForeignCall
  , freeForeignCallInfo
  ) where

import Prelude -- See note [Why do we import Prelude here?]
#if !defined(javascript_HOST_ARCH)
import Control.Exception
import Foreign.C
#endif
import Data.Binary
import GHC.Generics
import Foreign

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

prepForeignCall
    :: [FFIType]          -- arg types
    -> FFIType            -- result type
    -> IO (Ptr C_ffi_cif) -- token for making calls (must be freed by caller)

#if !defined(javascript_HOST_ARCH)
prepForeignCall arg_types result_type = do
  let n_args = length arg_types
  arg_arr <- mallocArray n_args
  pokeArray arg_arr (map ffiType arg_types)
  cif <- mallocBytes (#const sizeof(ffi_cif))
  r <- ffi_prep_cif cif fFI_DEFAULT_ABI (fromIntegral n_args) (ffiType result_type) arg_arr
  if r /= fFI_OK then
    throwIO $ ErrorCall $ concat
      [ "prepForeignCallFailed: ", strError r,
        "(arg tys: ", show arg_types,
        " res ty: ", show result_type, ")" ]
  else
    return (castPtr cif)
#else
prepForeignCall _ _ =
  error "GHCi.FFI.prepForeignCall: Called with JS_HOST_ARCH! Perhaps you need to run configure?"
#endif


freeForeignCallInfo :: Ptr C_ffi_cif -> IO ()
#if !defined(javascript_HOST_ARCH)
freeForeignCallInfo p = do
  free ((#ptr ffi_cif, arg_types) p)
  free p
#else
freeForeignCallInfo _ =
  error "GHCi.FFI.freeForeignCallInfo: Called with JS_HOST_ARCH! Perhaps you need to run configure?"
#endif

data C_ffi_cif

#if !defined(javascript_HOST_ARCH)
data C_ffi_type

strError :: C_ffi_status -> String
strError r
  | r == fFI_BAD_ABI
  = "invalid ABI (FFI_BAD_ABI)"
  | r == fFI_BAD_TYPEDEF
  = "invalid type description (FFI_BAD_TYPEDEF)"
  | otherwise
  = "unknown error: " ++ show r

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

fFI_OK, fFI_BAD_ABI, fFI_BAD_TYPEDEF :: C_ffi_status
fFI_OK = (#const FFI_OK)
fFI_BAD_ABI = (#const FFI_BAD_ABI)
fFI_BAD_TYPEDEF = (#const FFI_BAD_TYPEDEF)

fFI_DEFAULT_ABI :: C_ffi_abi
fFI_DEFAULT_ABI = (#const FFI_DEFAULT_ABI)

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
#endif

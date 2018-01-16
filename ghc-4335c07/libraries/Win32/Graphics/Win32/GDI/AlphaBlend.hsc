{-# LANGUAGE CPP #-}
{- |
   Module      :  Graphics.Win32.GDI.AlphaBlend
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Provides alpha blending  functionality.
-}
module Graphics.Win32.GDI.AlphaBlend where
import Foreign.Storable         ( Storable(..) )
import Foreign.Ptr              ( Ptr )
import Graphics.Win32.GDI.Types ( HDC )
import System.Win32.Types       ( BOOL, BYTE, UINT )

#include <windows.h>
#include "alignment.h"
##include "windows_cconv.h"

foreign import ccall unsafe "alphablend.h"
  c_AlphaBlend :: HDC -> Int -> Int -> Int -> Int -> HDC -> Int -> Int -> Int -> Int -> PBLENDFUNCTION -> IO BOOL
{-
We use C wrapper function to call this API.
Because foreign stacall/ccall/capi doesn't work with non-pointer user defined type.

We think that capi should support that when user defined type has Storable class instance
and using CTYPE pragma in the scope.

{-# LANGUAGE CApiFFI #-}

data {-# CTYPE "windows.h" "BLENDFUNCTION" #-} BLENDFUNCTION =

foreign import capi unsafe "windows.h AlphaBlend"
  c_AlphaBlend :: HDC -> Int -> Int -> Int -> Int -> HDC -> Int -> Int -> Int -> Int -> BLENDFUNCTION -> IO BOOL
-}

foreign import WINDOWS_CCONV unsafe "windows.h TransparentBlt"
  c_TransparentBlt :: HDC -> Int -> Int -> Int -> Int -> HDC -> Int -> Int -> Int -> Int -> UINT -> IO BOOL

aC_SRC_OVER :: BYTE
aC_SRC_OVER = #const AC_SRC_OVER

aC_SRC_ALPHA :: BYTE
aC_SRC_ALPHA = #const AC_SRC_ALPHA

type PBLENDFUNCTION  = Ptr BLENDFUNCTION
type LPBLENDFUNCTION = Ptr BLENDFUNCTION

data BLENDFUNCTION = BLENDFUNCTION
    { blendOp     :: BYTE
    , blendFlags  :: BYTE
    , sourceConstantAlpha :: BYTE
    , alphaFormat :: BYTE
    } deriving (Show)

instance Storable BLENDFUNCTION where
    sizeOf = const #size BLENDFUNCTION
    alignment _ = #alignment BLENDFUNCTION
    poke buf func = do
        (#poke BLENDFUNCTION, BlendOp)     buf (blendOp func)
        (#poke BLENDFUNCTION, BlendFlags)  buf (blendFlags func)
        (#poke BLENDFUNCTION, SourceConstantAlpha) buf (sourceConstantAlpha func)
        (#poke BLENDFUNCTION, AlphaFormat) buf (alphaFormat func)

    peek buf = do
        blendOp'     <- (#peek BLENDFUNCTION, BlendOp) buf
        blendFlags'  <- (#peek BLENDFUNCTION, BlendFlags) buf
        sourceConstantAlpha' <-
            (#peek BLENDFUNCTION, SourceConstantAlpha) buf
        alphaFormat' <- (#peek BLENDFUNCTION, AlphaFormat) buf
        return $ BLENDFUNCTION blendOp' blendFlags' sourceConstantAlpha' alphaFormat'

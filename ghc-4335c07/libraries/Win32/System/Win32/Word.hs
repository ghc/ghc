{- |
   Module      :  System.Win32.Word
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Windows' unsigned integer types and pointer type.
-}
module System.Win32.Word
  ( WORD, DWORD, PDWORD, LPDWORD
  , DWORDLONG, DDWORD
  , DWORD32, DWORD64, DWORD_PTR
  ) where
import Data.Word          ( Word64 )
import Foreign.Ptr        ( Ptr )
import System.Win32.Types ( WORD, DWORD, LPDWORD, DDWORD,
                            DWORD32, DWORD64, DWORD_PTR )

type PDWORD = Ptr DWORD
type DWORDLONG = Word64

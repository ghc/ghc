#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.NLS
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.NLS  (
        module System.Win32.NLS,

        -- defined in System.Win32.Types
        LCID, LANGID, SortID, SubLANGID, PrimaryLANGID,
        mAKELCID, lANGIDFROMLCID, sORTIDFROMLCID,
        mAKELANGID, pRIMARYLANGID, sUBLANGID
        ) where

import System.Win32.Types

import Foreign
import Foreign.C

##include "windows_cconv.h"

#include <windows.h>
#include "errors.h"
#include "win32debug.h"

#{enum LCID,
 , lOCALE_SYSTEM_DEFAULT = LOCALE_SYSTEM_DEFAULT
 , lOCALE_USER_DEFAULT   = LOCALE_USER_DEFAULT
 , lOCALE_NEUTRAL        = LOCALE_NEUTRAL
 }

foreign import WINDOWS_CCONV unsafe "windows.h ConvertDefaultLocale"
  convertDefaultLocale :: LCID -> IO LCID

-- ToDo: various enum functions.

type CodePage = UINT

#{enum CodePage,
 , cP_ACP       = CP_ACP
 , cP_MACCP     = CP_MACCP
 , cP_OEMCP     = CP_OEMCP
 }

foreign import WINDOWS_CCONV unsafe "windows.h GetACP"
  getACP :: IO CodePage

foreign import WINDOWS_CCONV unsafe "windows.h SetThreadLocale"
  setThreadLocale :: LCID -> IO ()

type LCTYPE = UINT

#{enum LCTYPE,
 , lOCALE_ICALENDARTYPE = LOCALE_ICALENDARTYPE
 , lOCALE_SDATE         = LOCALE_SDATE
 , lOCALE_ICURRDIGITS   = LOCALE_ICURRDIGITS
 , lOCALE_SDECIMAL      = LOCALE_SDECIMAL
 , lOCALE_ICURRENCY     = LOCALE_ICURRENCY
 , lOCALE_SGROUPING     = LOCALE_SGROUPING
 , lOCALE_IDIGITS       = LOCALE_IDIGITS
 , lOCALE_SLIST         = LOCALE_SLIST
 , lOCALE_IFIRSTDAYOFWEEK = LOCALE_IFIRSTDAYOFWEEK
 , lOCALE_SLONGDATE     = LOCALE_SLONGDATE
 , lOCALE_IFIRSTWEEKOFYEAR = LOCALE_IFIRSTWEEKOFYEAR
 , lOCALE_SMONDECIMALSEP = LOCALE_SMONDECIMALSEP
 , lOCALE_ILZERO        = LOCALE_ILZERO
 , lOCALE_SMONGROUPING  = LOCALE_SMONGROUPING
 , lOCALE_IMEASURE      = LOCALE_IMEASURE
 , lOCALE_SMONTHOUSANDSEP = LOCALE_SMONTHOUSANDSEP
 , lOCALE_INEGCURR      = LOCALE_INEGCURR
 , lOCALE_SNEGATIVESIGN = LOCALE_SNEGATIVESIGN
 , lOCALE_INEGNUMBER    = LOCALE_INEGNUMBER
 , lOCALE_SPOSITIVESIGN = LOCALE_SPOSITIVESIGN
 , lOCALE_SSHORTDATE    = LOCALE_SSHORTDATE
 , lOCALE_ITIME         = LOCALE_ITIME
 , lOCALE_STHOUSAND     = LOCALE_STHOUSAND
 , lOCALE_S1159         = LOCALE_S1159
 , lOCALE_STIME         = LOCALE_STIME
 , lOCALE_S2359         = LOCALE_S2359
 , lOCALE_STIMEFORMAT   = LOCALE_STIMEFORMAT
 , lOCALE_SCURRENCY     = LOCALE_SCURRENCY
 }

setLocaleInfo :: LCID -> LCTYPE -> String -> IO ()
setLocaleInfo locale ty info =
  withTString info $ \ c_info ->
  failIfFalse_ "SetLocaleInfo" $ c_SetLocaleInfo locale ty c_info
foreign import WINDOWS_CCONV unsafe "windows.h SetLocaleInfoW"
  c_SetLocaleInfo :: LCID -> LCTYPE -> LPCTSTR -> IO Bool

type LCMapFlags = DWORD

#{enum LCMapFlags,
 , lCMAP_BYTEREV        = LCMAP_BYTEREV
 , lCMAP_FULLWIDTH      = LCMAP_FULLWIDTH
 , lCMAP_HALFWIDTH      = LCMAP_HALFWIDTH
 , lCMAP_HIRAGANA       = LCMAP_HIRAGANA
 , lCMAP_KATAKANA       = LCMAP_KATAKANA
 , lCMAP_LOWERCASE      = LCMAP_LOWERCASE
 , lCMAP_SORTKEY        = LCMAP_SORTKEY
 , lCMAP_UPPERCASE      = LCMAP_UPPERCASE
 , nORM_IGNORECASE      = NORM_IGNORECASE
 , nORM_IGNORENONSPACE  = NORM_IGNORENONSPACE
 , nORM_IGNOREKANATYPE  = NORM_IGNOREKANATYPE
 , nORM_IGNORESYMBOLS   = NORM_IGNORESYMBOLS
 , nORM_IGNOREWIDTH     = NORM_IGNOREWIDTH
 , sORT_STRINGSORT      = SORT_STRINGSORT
 , lCMAP_LINGUISTIC_CASING      = LCMAP_LINGUISTIC_CASING
 , lCMAP_SIMPLIFIED_CHINESE     = LCMAP_SIMPLIFIED_CHINESE
 , lCMAP_TRADITIONAL_CHINESE    = LCMAP_TRADITIONAL_CHINESE
 }

lCMapString :: LCID -> LCMapFlags -> String -> Int -> IO String
lCMapString locale flags src dest_size =
  withTStringLen src $ \ (c_src, src_len) ->
  allocaArray dest_size $ \ c_dest -> do
  _ <- failIfZero "LCMapString" $
    c_LCMapString locale flags c_src src_len c_dest dest_size
  peekTString c_dest
foreign import WINDOWS_CCONV unsafe "windows.h LCMapStringW"
  c_LCMapString :: LCID -> LCMapFlags -> LPCTSTR -> Int -> LPCTSTR -> Int -> IO Int

type LocaleTestFlags = DWORD

#{enum LocaleTestFlags,
 , lCID_INSTALLED       = LCID_INSTALLED
 , lCID_SUPPORTED       = LCID_SUPPORTED
 }

foreign import WINDOWS_CCONV unsafe "windows.h IsValidLocale"
  isValidLocale :: LCID -> LocaleTestFlags -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h IsValidCodePage"
  isValidCodePage :: CodePage -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetUserDefaultLCID"
  getUserDefaultLCID :: LCID

foreign import WINDOWS_CCONV unsafe "windows.h GetUserDefaultLangID"
  getUserDefaultLangID :: LANGID

foreign import WINDOWS_CCONV unsafe "windows.h GetThreadLocale"
  getThreadLocale :: IO LCID

foreign import WINDOWS_CCONV unsafe "windows.h GetSystemDefaultLCID"
  getSystemDefaultLCID :: LCID

foreign import WINDOWS_CCONV unsafe "windows.h GetSystemDefaultLangID"
  getSystemDefaultLangID :: LANGID

foreign import WINDOWS_CCONV unsafe "windows.h GetOEMCP"
  getOEMCP :: CodePage

#{enum PrimaryLANGID,
 , lANG_NEUTRAL         = LANG_NEUTRAL
 , lANG_BULGARIAN       = LANG_BULGARIAN
 , lANG_CHINESE         = LANG_CHINESE
 , lANG_CZECH           = LANG_CZECH
 , lANG_DANISH          = LANG_DANISH
 , lANG_GERMAN          = LANG_GERMAN
 , lANG_GREEK           = LANG_GREEK
 , lANG_ENGLISH         = LANG_ENGLISH
 , lANG_SPANISH         = LANG_SPANISH
 , lANG_FINNISH         = LANG_FINNISH
 , lANG_FRENCH          = LANG_FRENCH
 , lANG_HUNGARIAN       = LANG_HUNGARIAN
 , lANG_ICELANDIC       = LANG_ICELANDIC
 , lANG_ITALIAN         = LANG_ITALIAN
 , lANG_JAPANESE        = LANG_JAPANESE
 , lANG_KOREAN          = LANG_KOREAN
 , lANG_DUTCH           = LANG_DUTCH
 , lANG_NORWEGIAN       = LANG_NORWEGIAN
 , lANG_POLISH          = LANG_POLISH
 , lANG_PORTUGUESE      = LANG_PORTUGUESE
 , lANG_ROMANIAN        = LANG_ROMANIAN
 , lANG_RUSSIAN         = LANG_RUSSIAN
 , lANG_CROATIAN        = LANG_CROATIAN
 , lANG_SLOVAK          = LANG_SLOVAK
 , lANG_SWEDISH         = LANG_SWEDISH
 , lANG_TURKISH         = LANG_TURKISH
 , lANG_SLOVENIAN       = LANG_SLOVENIAN
 , lANG_ARABIC          = LANG_ARABIC
 , lANG_CATALAN         = LANG_CATALAN
 , lANG_HEBREW          = LANG_HEBREW
 , lANG_SERBIAN         = LANG_SERBIAN
 , lANG_ALBANIAN        = LANG_ALBANIAN
 , lANG_THAI            = LANG_THAI
 , lANG_URDU            = LANG_URDU
 , lANG_INDONESIAN      = LANG_INDONESIAN
 , lANG_BELARUSIAN      = LANG_BELARUSIAN
 , lANG_ESTONIAN        = LANG_ESTONIAN
 , lANG_LATVIAN         = LANG_LATVIAN
 , lANG_LITHUANIAN      = LANG_LITHUANIAN
 , lANG_FARSI           = LANG_FARSI
 , lANG_VIETNAMESE      = LANG_VIETNAMESE
 , lANG_ARMENIAN        = LANG_ARMENIAN
 , lANG_AZERI           = LANG_AZERI
 , lANG_BASQUE          = LANG_BASQUE
 , lANG_MACEDONIAN      = LANG_MACEDONIAN
 , lANG_AFRIKAANS       = LANG_AFRIKAANS
 , lANG_GEORGIAN        = LANG_GEORGIAN
 , lANG_FAEROESE        = LANG_FAEROESE
 , lANG_HINDI           = LANG_HINDI
 , lANG_MALAY           = LANG_MALAY
 , lANG_KAZAK           = LANG_KAZAK
 , lANG_SWAHILI         = LANG_SWAHILI
 , lANG_UZBEK           = LANG_UZBEK
 , lANG_TATAR           = LANG_TATAR
 , lANG_BENGALI         = LANG_BENGALI
 , lANG_PUNJABI         = LANG_PUNJABI
 , lANG_GUJARATI        = LANG_GUJARATI
 , lANG_ORIYA           = LANG_ORIYA
 , lANG_TAMIL           = LANG_TAMIL
 , lANG_TELUGU          = LANG_TELUGU
 , lANG_KANNADA         = LANG_KANNADA
 , lANG_MALAYALAM       = LANG_MALAYALAM
 , lANG_ASSAMESE        = LANG_ASSAMESE
 , lANG_MARATHI         = LANG_MARATHI
 , lANG_SANSKRIT        = LANG_SANSKRIT
 , lANG_KONKANI         = LANG_KONKANI
 , lANG_MANIPURI        = LANG_MANIPURI
 , lANG_SINDHI          = LANG_SINDHI
 , lANG_KASHMIRI        = LANG_KASHMIRI
 , lANG_NEPALI          = LANG_NEPALI
 }

#{enum SortID,
 , sORT_DEFAULT         = SORT_DEFAULT
 , sORT_JAPANESE_XJIS   = SORT_JAPANESE_XJIS
 , sORT_JAPANESE_UNICODE = SORT_JAPANESE_UNICODE
 , sORT_CHINESE_BIG5    = SORT_CHINESE_BIG5
 , sORT_CHINESE_UNICODE = SORT_CHINESE_UNICODE
 , sORT_KOREAN_KSC      = SORT_KOREAN_KSC
 , sORT_KOREAN_UNICODE  = SORT_KOREAN_UNICODE
 }

#{enum SubLANGID,
 , sUBLANG_NEUTRAL                      = SUBLANG_NEUTRAL
 , sUBLANG_DEFAULT                      = SUBLANG_DEFAULT
 , sUBLANG_SYS_DEFAULT                  = SUBLANG_SYS_DEFAULT
 , sUBLANG_CHINESE_TRADITIONAL          = SUBLANG_CHINESE_TRADITIONAL
 , sUBLANG_CHINESE_SIMPLIFIED           = SUBLANG_CHINESE_SIMPLIFIED
 , sUBLANG_CHINESE_HONGKONG             = SUBLANG_CHINESE_HONGKONG
 , sUBLANG_CHINESE_SINGAPORE            = SUBLANG_CHINESE_SINGAPORE
 , sUBLANG_DUTCH                        = SUBLANG_DUTCH
 , sUBLANG_DUTCH_BELGIAN                = SUBLANG_DUTCH_BELGIAN
 , sUBLANG_ENGLISH_US                   = SUBLANG_ENGLISH_US
 , sUBLANG_ENGLISH_UK                   = SUBLANG_ENGLISH_UK
 , sUBLANG_ENGLISH_AUS                  = SUBLANG_ENGLISH_AUS
 , sUBLANG_ENGLISH_CAN                  = SUBLANG_ENGLISH_CAN
 , sUBLANG_ENGLISH_NZ                   = SUBLANG_ENGLISH_NZ
 , sUBLANG_ENGLISH_EIRE                 = SUBLANG_ENGLISH_EIRE
 , sUBLANG_FRENCH                       = SUBLANG_FRENCH
 , sUBLANG_FRENCH_BELGIAN               = SUBLANG_FRENCH_BELGIAN
 , sUBLANG_FRENCH_CANADIAN              = SUBLANG_FRENCH_CANADIAN
 , sUBLANG_FRENCH_SWISS                 = SUBLANG_FRENCH_SWISS
 , sUBLANG_GERMAN                       = SUBLANG_GERMAN
 , sUBLANG_GERMAN_SWISS                 = SUBLANG_GERMAN_SWISS
 , sUBLANG_GERMAN_AUSTRIAN              = SUBLANG_GERMAN_AUSTRIAN
 , sUBLANG_ITALIAN                      = SUBLANG_ITALIAN
 , sUBLANG_ITALIAN_SWISS                = SUBLANG_ITALIAN_SWISS
 , sUBLANG_NORWEGIAN_BOKMAL             = SUBLANG_NORWEGIAN_BOKMAL
 , sUBLANG_NORWEGIAN_NYNORSK            = SUBLANG_NORWEGIAN_NYNORSK
 , sUBLANG_PORTUGUESE                   = SUBLANG_PORTUGUESE
 , sUBLANG_PORTUGUESE_BRAZILIAN         = SUBLANG_PORTUGUESE_BRAZILIAN
 , sUBLANG_SPANISH                      = SUBLANG_SPANISH
 , sUBLANG_SPANISH_MEXICAN              = SUBLANG_SPANISH_MEXICAN
 , sUBLANG_SPANISH_MODERN               = SUBLANG_SPANISH_MODERN
 , sUBLANG_ARABIC_SAUDI_ARABIA          = SUBLANG_ARABIC_SAUDI_ARABIA
 , sUBLANG_ARABIC_IRAQ                  = SUBLANG_ARABIC_IRAQ
 , sUBLANG_ARABIC_EGYPT                 = SUBLANG_ARABIC_EGYPT
 , sUBLANG_ARABIC_LIBYA                 = SUBLANG_ARABIC_LIBYA
 , sUBLANG_ARABIC_ALGERIA               = SUBLANG_ARABIC_ALGERIA
 , sUBLANG_ARABIC_MOROCCO               = SUBLANG_ARABIC_MOROCCO
 , sUBLANG_ARABIC_TUNISIA               = SUBLANG_ARABIC_TUNISIA
 , sUBLANG_ARABIC_OMAN                  = SUBLANG_ARABIC_OMAN
 , sUBLANG_ARABIC_YEMEN                 = SUBLANG_ARABIC_YEMEN
 , sUBLANG_ARABIC_SYRIA                 = SUBLANG_ARABIC_SYRIA
 , sUBLANG_ARABIC_JORDAN                = SUBLANG_ARABIC_JORDAN
 , sUBLANG_ARABIC_LEBANON               = SUBLANG_ARABIC_LEBANON
 , sUBLANG_ARABIC_KUWAIT                = SUBLANG_ARABIC_KUWAIT
 , sUBLANG_ARABIC_UAE                   = SUBLANG_ARABIC_UAE
 , sUBLANG_ARABIC_BAHRAIN               = SUBLANG_ARABIC_BAHRAIN
 , sUBLANG_ARABIC_QATAR                 = SUBLANG_ARABIC_QATAR
 , sUBLANG_AZERI_CYRILLIC               = SUBLANG_AZERI_CYRILLIC
 , sUBLANG_AZERI_LATIN                  = SUBLANG_AZERI_LATIN
 , sUBLANG_CHINESE_MACAU                = SUBLANG_CHINESE_MACAU
 , sUBLANG_ENGLISH_SOUTH_AFRICA         = SUBLANG_ENGLISH_SOUTH_AFRICA
 , sUBLANG_ENGLISH_JAMAICA              = SUBLANG_ENGLISH_JAMAICA
 , sUBLANG_ENGLISH_CARIBBEAN            = SUBLANG_ENGLISH_CARIBBEAN
 , sUBLANG_ENGLISH_BELIZE               = SUBLANG_ENGLISH_BELIZE
 , sUBLANG_ENGLISH_TRINIDAD             = SUBLANG_ENGLISH_TRINIDAD
 , sUBLANG_ENGLISH_PHILIPPINES          = SUBLANG_ENGLISH_PHILIPPINES
 , sUBLANG_ENGLISH_ZIMBABWE             = SUBLANG_ENGLISH_ZIMBABWE
 , sUBLANG_FRENCH_LUXEMBOURG            = SUBLANG_FRENCH_LUXEMBOURG
 , sUBLANG_FRENCH_MONACO                = SUBLANG_FRENCH_MONACO
 , sUBLANG_GERMAN_LUXEMBOURG            = SUBLANG_GERMAN_LUXEMBOURG
 , sUBLANG_GERMAN_LIECHTENSTEIN         = SUBLANG_GERMAN_LIECHTENSTEIN
 , sUBLANG_KASHMIRI_INDIA               = SUBLANG_KASHMIRI_INDIA
 , sUBLANG_KOREAN                       = SUBLANG_KOREAN
 , sUBLANG_LITHUANIAN                   = SUBLANG_LITHUANIAN
 , sUBLANG_MALAY_MALAYSIA               = SUBLANG_MALAY_MALAYSIA
 , sUBLANG_MALAY_BRUNEI_DARUSSALAM      = SUBLANG_MALAY_BRUNEI_DARUSSALAM
 , sUBLANG_NEPALI_INDIA                 = SUBLANG_NEPALI_INDIA
 , sUBLANG_SERBIAN_LATIN                = SUBLANG_SERBIAN_LATIN
 , sUBLANG_SERBIAN_CYRILLIC             = SUBLANG_SERBIAN_CYRILLIC
 , sUBLANG_SPANISH_GUATEMALA            = SUBLANG_SPANISH_GUATEMALA
 , sUBLANG_SPANISH_COSTA_RICA           = SUBLANG_SPANISH_COSTA_RICA
 , sUBLANG_SPANISH_PANAMA               = SUBLANG_SPANISH_PANAMA
 , sUBLANG_SPANISH_DOMINICAN_REPUBLIC   = SUBLANG_SPANISH_DOMINICAN_REPUBLIC
 , sUBLANG_SPANISH_VENEZUELA            = SUBLANG_SPANISH_VENEZUELA
 , sUBLANG_SPANISH_COLOMBIA             = SUBLANG_SPANISH_COLOMBIA
 , sUBLANG_SPANISH_PERU                 = SUBLANG_SPANISH_PERU
 , sUBLANG_SPANISH_ARGENTINA            = SUBLANG_SPANISH_ARGENTINA
 , sUBLANG_SPANISH_ECUADOR              = SUBLANG_SPANISH_ECUADOR
 , sUBLANG_SPANISH_CHILE                = SUBLANG_SPANISH_CHILE
 , sUBLANG_SPANISH_URUGUAY              = SUBLANG_SPANISH_URUGUAY
 , sUBLANG_SPANISH_PARAGUAY             = SUBLANG_SPANISH_PARAGUAY
 , sUBLANG_SPANISH_BOLIVIA              = SUBLANG_SPANISH_BOLIVIA
 , sUBLANG_SPANISH_EL_SALVADOR          = SUBLANG_SPANISH_EL_SALVADOR
 , sUBLANG_SPANISH_HONDURAS             = SUBLANG_SPANISH_HONDURAS
 , sUBLANG_SPANISH_NICARAGUA            = SUBLANG_SPANISH_NICARAGUA
 , sUBLANG_SPANISH_PUERTO_RICO          = SUBLANG_SPANISH_PUERTO_RICO
 , sUBLANG_SWEDISH                      = SUBLANG_SWEDISH
 , sUBLANG_SWEDISH_FINLAND              = SUBLANG_SWEDISH_FINLAND
 , sUBLANG_URDU_PAKISTAN                = SUBLANG_URDU_PAKISTAN
 , sUBLANG_URDU_INDIA                   = SUBLANG_URDU_INDIA
 , sUBLANG_UZBEK_LATIN                  = SUBLANG_UZBEK_LATIN
 , sUBLANG_UZBEK_CYRILLIC               = SUBLANG_UZBEK_CYRILLIC
 }

-- , SUBLANG_LITHUANIAN_CLASSIC (not in mingw-20001111)

-- ----------------------------------------------------------------------------

-- | The `System.IO` input functions (e.g. `getLine`) don't
-- automatically convert to Unicode, so this function is provided to
-- make the conversion from a multibyte string in the given code page
-- to a proper Unicode string.  To get the code page for the console,
-- use `getConsoleCP`.

stringToUnicode :: CodePage -> String -> IO String
stringToUnicode _cp "" = return ""
     -- MultiByteToWideChar doesn't handle empty strings (#1929)
stringToUnicode cp mbstr =
  withCAStringLen mbstr $ \(cstr,len) -> do
    wchars <- failIfZero "MultiByteToWideChar" $ multiByteToWideChar
                cp
                0
                cstr
                (fromIntegral len)
                nullPtr 0
    -- wchars is the length of buffer required
    allocaArray (fromIntegral wchars) $ \cwstr -> do
      wchars' <- failIfZero "MultiByteToWideChar" $ multiByteToWideChar
                cp
                0
                cstr
                (fromIntegral len)
                cwstr wchars
      peekCWStringLen (cwstr,fromIntegral wchars')  -- converts UTF-16 to [Char]

foreign import WINDOWS_CCONV unsafe "MultiByteToWideChar"
  multiByteToWideChar
        :: CodePage
        -> DWORD   -- dwFlags,
        -> LPCSTR  -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> LPWSTR  -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> IO CInt

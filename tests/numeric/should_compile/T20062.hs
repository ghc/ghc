module T20062 where

import GHC.Int
import GHC.Word
import Data.Char
import Control.Monad
import Foreign.C.Types

---------------------------------------------------------------
-- This is used to generate the code that is copy-pasted below
---------------------------------------------------------------

types :: [String]
types = [ "Word64", "Word32", "Word16", "Word8"
        , "Int64" , "Int32" , "Int16" , "Int8"
        , "CChar" , "CUShort", "CUInt", "CULong", "CULLong"
        , "CSChar", "CShort" , "CInt" , "CLong" , "CLLong"
        ]

main :: IO ()
main = do
  forM_ types $ \t1 ->
    forM_ types $ \t2 -> do
      let n = fmap toLower t1++"_to_"++fmap toLower t2
      let t = ":: "++t1++" -> "++t2
      putStrLn $ mconcat
        [ n, t, "\n"
        , n, " = fromIntegral\n"
        ]

---------------------------------------------------------------
-- End of generator
--
-- Copy-paste of generated code:
---------------------------------------------------------------

word64_to_word64:: Word64 -> Word64
word64_to_word64 = fromIntegral

word64_to_word32:: Word64 -> Word32
word64_to_word32 = fromIntegral

word64_to_word16:: Word64 -> Word16
word64_to_word16 = fromIntegral

word64_to_word8:: Word64 -> Word8
word64_to_word8 = fromIntegral

word64_to_int64:: Word64 -> Int64
word64_to_int64 = fromIntegral

word64_to_int32:: Word64 -> Int32
word64_to_int32 = fromIntegral

word64_to_int16:: Word64 -> Int16
word64_to_int16 = fromIntegral

word64_to_int8:: Word64 -> Int8
word64_to_int8 = fromIntegral

word64_to_cchar:: Word64 -> CChar
word64_to_cchar = fromIntegral

word64_to_cushort:: Word64 -> CUShort
word64_to_cushort = fromIntegral

word64_to_cuint:: Word64 -> CUInt
word64_to_cuint = fromIntegral

word64_to_culong:: Word64 -> CULong
word64_to_culong = fromIntegral

word64_to_cullong:: Word64 -> CULLong
word64_to_cullong = fromIntegral

word64_to_cschar:: Word64 -> CSChar
word64_to_cschar = fromIntegral

word64_to_cshort:: Word64 -> CShort
word64_to_cshort = fromIntegral

word64_to_cint:: Word64 -> CInt
word64_to_cint = fromIntegral

word64_to_clong:: Word64 -> CLong
word64_to_clong = fromIntegral

word64_to_cllong:: Word64 -> CLLong
word64_to_cllong = fromIntegral

word32_to_word64:: Word32 -> Word64
word32_to_word64 = fromIntegral

word32_to_word32:: Word32 -> Word32
word32_to_word32 = fromIntegral

word32_to_word16:: Word32 -> Word16
word32_to_word16 = fromIntegral

word32_to_word8:: Word32 -> Word8
word32_to_word8 = fromIntegral

word32_to_int64:: Word32 -> Int64
word32_to_int64 = fromIntegral

word32_to_int32:: Word32 -> Int32
word32_to_int32 = fromIntegral

word32_to_int16:: Word32 -> Int16
word32_to_int16 = fromIntegral

word32_to_int8:: Word32 -> Int8
word32_to_int8 = fromIntegral

word32_to_cchar:: Word32 -> CChar
word32_to_cchar = fromIntegral

word32_to_cushort:: Word32 -> CUShort
word32_to_cushort = fromIntegral

word32_to_cuint:: Word32 -> CUInt
word32_to_cuint = fromIntegral

word32_to_culong:: Word32 -> CULong
word32_to_culong = fromIntegral

word32_to_cullong:: Word32 -> CULLong
word32_to_cullong = fromIntegral

word32_to_cschar:: Word32 -> CSChar
word32_to_cschar = fromIntegral

word32_to_cshort:: Word32 -> CShort
word32_to_cshort = fromIntegral

word32_to_cint:: Word32 -> CInt
word32_to_cint = fromIntegral

word32_to_clong:: Word32 -> CLong
word32_to_clong = fromIntegral

word32_to_cllong:: Word32 -> CLLong
word32_to_cllong = fromIntegral

word16_to_word64:: Word16 -> Word64
word16_to_word64 = fromIntegral

word16_to_word32:: Word16 -> Word32
word16_to_word32 = fromIntegral

word16_to_word16:: Word16 -> Word16
word16_to_word16 = fromIntegral

word16_to_word8:: Word16 -> Word8
word16_to_word8 = fromIntegral

word16_to_int64:: Word16 -> Int64
word16_to_int64 = fromIntegral

word16_to_int32:: Word16 -> Int32
word16_to_int32 = fromIntegral

word16_to_int16:: Word16 -> Int16
word16_to_int16 = fromIntegral

word16_to_int8:: Word16 -> Int8
word16_to_int8 = fromIntegral

word16_to_cchar:: Word16 -> CChar
word16_to_cchar = fromIntegral

word16_to_cushort:: Word16 -> CUShort
word16_to_cushort = fromIntegral

word16_to_cuint:: Word16 -> CUInt
word16_to_cuint = fromIntegral

word16_to_culong:: Word16 -> CULong
word16_to_culong = fromIntegral

word16_to_cullong:: Word16 -> CULLong
word16_to_cullong = fromIntegral

word16_to_cschar:: Word16 -> CSChar
word16_to_cschar = fromIntegral

word16_to_cshort:: Word16 -> CShort
word16_to_cshort = fromIntegral

word16_to_cint:: Word16 -> CInt
word16_to_cint = fromIntegral

word16_to_clong:: Word16 -> CLong
word16_to_clong = fromIntegral

word16_to_cllong:: Word16 -> CLLong
word16_to_cllong = fromIntegral

word8_to_word64:: Word8 -> Word64
word8_to_word64 = fromIntegral

word8_to_word32:: Word8 -> Word32
word8_to_word32 = fromIntegral

word8_to_word16:: Word8 -> Word16
word8_to_word16 = fromIntegral

word8_to_word8:: Word8 -> Word8
word8_to_word8 = fromIntegral

word8_to_int64:: Word8 -> Int64
word8_to_int64 = fromIntegral

word8_to_int32:: Word8 -> Int32
word8_to_int32 = fromIntegral

word8_to_int16:: Word8 -> Int16
word8_to_int16 = fromIntegral

word8_to_int8:: Word8 -> Int8
word8_to_int8 = fromIntegral

word8_to_cchar:: Word8 -> CChar
word8_to_cchar = fromIntegral

word8_to_cushort:: Word8 -> CUShort
word8_to_cushort = fromIntegral

word8_to_cuint:: Word8 -> CUInt
word8_to_cuint = fromIntegral

word8_to_culong:: Word8 -> CULong
word8_to_culong = fromIntegral

word8_to_cullong:: Word8 -> CULLong
word8_to_cullong = fromIntegral

word8_to_cschar:: Word8 -> CSChar
word8_to_cschar = fromIntegral

word8_to_cshort:: Word8 -> CShort
word8_to_cshort = fromIntegral

word8_to_cint:: Word8 -> CInt
word8_to_cint = fromIntegral

word8_to_clong:: Word8 -> CLong
word8_to_clong = fromIntegral

word8_to_cllong:: Word8 -> CLLong
word8_to_cllong = fromIntegral

int64_to_word64:: Int64 -> Word64
int64_to_word64 = fromIntegral

int64_to_word32:: Int64 -> Word32
int64_to_word32 = fromIntegral

int64_to_word16:: Int64 -> Word16
int64_to_word16 = fromIntegral

int64_to_word8:: Int64 -> Word8
int64_to_word8 = fromIntegral

int64_to_int64:: Int64 -> Int64
int64_to_int64 = fromIntegral

int64_to_int32:: Int64 -> Int32
int64_to_int32 = fromIntegral

int64_to_int16:: Int64 -> Int16
int64_to_int16 = fromIntegral

int64_to_int8:: Int64 -> Int8
int64_to_int8 = fromIntegral

int64_to_cchar:: Int64 -> CChar
int64_to_cchar = fromIntegral

int64_to_cushort:: Int64 -> CUShort
int64_to_cushort = fromIntegral

int64_to_cuint:: Int64 -> CUInt
int64_to_cuint = fromIntegral

int64_to_culong:: Int64 -> CULong
int64_to_culong = fromIntegral

int64_to_cullong:: Int64 -> CULLong
int64_to_cullong = fromIntegral

int64_to_cschar:: Int64 -> CSChar
int64_to_cschar = fromIntegral

int64_to_cshort:: Int64 -> CShort
int64_to_cshort = fromIntegral

int64_to_cint:: Int64 -> CInt
int64_to_cint = fromIntegral

int64_to_clong:: Int64 -> CLong
int64_to_clong = fromIntegral

int64_to_cllong:: Int64 -> CLLong
int64_to_cllong = fromIntegral

int32_to_word64:: Int32 -> Word64
int32_to_word64 = fromIntegral

int32_to_word32:: Int32 -> Word32
int32_to_word32 = fromIntegral

int32_to_word16:: Int32 -> Word16
int32_to_word16 = fromIntegral

int32_to_word8:: Int32 -> Word8
int32_to_word8 = fromIntegral

int32_to_int64:: Int32 -> Int64
int32_to_int64 = fromIntegral

int32_to_int32:: Int32 -> Int32
int32_to_int32 = fromIntegral

int32_to_int16:: Int32 -> Int16
int32_to_int16 = fromIntegral

int32_to_int8:: Int32 -> Int8
int32_to_int8 = fromIntegral

int32_to_cchar:: Int32 -> CChar
int32_to_cchar = fromIntegral

int32_to_cushort:: Int32 -> CUShort
int32_to_cushort = fromIntegral

int32_to_cuint:: Int32 -> CUInt
int32_to_cuint = fromIntegral

int32_to_culong:: Int32 -> CULong
int32_to_culong = fromIntegral

int32_to_cullong:: Int32 -> CULLong
int32_to_cullong = fromIntegral

int32_to_cschar:: Int32 -> CSChar
int32_to_cschar = fromIntegral

int32_to_cshort:: Int32 -> CShort
int32_to_cshort = fromIntegral

int32_to_cint:: Int32 -> CInt
int32_to_cint = fromIntegral

int32_to_clong:: Int32 -> CLong
int32_to_clong = fromIntegral

int32_to_cllong:: Int32 -> CLLong
int32_to_cllong = fromIntegral

int16_to_word64:: Int16 -> Word64
int16_to_word64 = fromIntegral

int16_to_word32:: Int16 -> Word32
int16_to_word32 = fromIntegral

int16_to_word16:: Int16 -> Word16
int16_to_word16 = fromIntegral

int16_to_word8:: Int16 -> Word8
int16_to_word8 = fromIntegral

int16_to_int64:: Int16 -> Int64
int16_to_int64 = fromIntegral

int16_to_int32:: Int16 -> Int32
int16_to_int32 = fromIntegral

int16_to_int16:: Int16 -> Int16
int16_to_int16 = fromIntegral

int16_to_int8:: Int16 -> Int8
int16_to_int8 = fromIntegral

int16_to_cchar:: Int16 -> CChar
int16_to_cchar = fromIntegral

int16_to_cushort:: Int16 -> CUShort
int16_to_cushort = fromIntegral

int16_to_cuint:: Int16 -> CUInt
int16_to_cuint = fromIntegral

int16_to_culong:: Int16 -> CULong
int16_to_culong = fromIntegral

int16_to_cullong:: Int16 -> CULLong
int16_to_cullong = fromIntegral

int16_to_cschar:: Int16 -> CSChar
int16_to_cschar = fromIntegral

int16_to_cshort:: Int16 -> CShort
int16_to_cshort = fromIntegral

int16_to_cint:: Int16 -> CInt
int16_to_cint = fromIntegral

int16_to_clong:: Int16 -> CLong
int16_to_clong = fromIntegral

int16_to_cllong:: Int16 -> CLLong
int16_to_cllong = fromIntegral

int8_to_word64:: Int8 -> Word64
int8_to_word64 = fromIntegral

int8_to_word32:: Int8 -> Word32
int8_to_word32 = fromIntegral

int8_to_word16:: Int8 -> Word16
int8_to_word16 = fromIntegral

int8_to_word8:: Int8 -> Word8
int8_to_word8 = fromIntegral

int8_to_int64:: Int8 -> Int64
int8_to_int64 = fromIntegral

int8_to_int32:: Int8 -> Int32
int8_to_int32 = fromIntegral

int8_to_int16:: Int8 -> Int16
int8_to_int16 = fromIntegral

int8_to_int8:: Int8 -> Int8
int8_to_int8 = fromIntegral

int8_to_cchar:: Int8 -> CChar
int8_to_cchar = fromIntegral

int8_to_cushort:: Int8 -> CUShort
int8_to_cushort = fromIntegral

int8_to_cuint:: Int8 -> CUInt
int8_to_cuint = fromIntegral

int8_to_culong:: Int8 -> CULong
int8_to_culong = fromIntegral

int8_to_cullong:: Int8 -> CULLong
int8_to_cullong = fromIntegral

int8_to_cschar:: Int8 -> CSChar
int8_to_cschar = fromIntegral

int8_to_cshort:: Int8 -> CShort
int8_to_cshort = fromIntegral

int8_to_cint:: Int8 -> CInt
int8_to_cint = fromIntegral

int8_to_clong:: Int8 -> CLong
int8_to_clong = fromIntegral

int8_to_cllong:: Int8 -> CLLong
int8_to_cllong = fromIntegral

cchar_to_word64:: CChar -> Word64
cchar_to_word64 = fromIntegral

cchar_to_word32:: CChar -> Word32
cchar_to_word32 = fromIntegral

cchar_to_word16:: CChar -> Word16
cchar_to_word16 = fromIntegral

cchar_to_word8:: CChar -> Word8
cchar_to_word8 = fromIntegral

cchar_to_int64:: CChar -> Int64
cchar_to_int64 = fromIntegral

cchar_to_int32:: CChar -> Int32
cchar_to_int32 = fromIntegral

cchar_to_int16:: CChar -> Int16
cchar_to_int16 = fromIntegral

cchar_to_int8:: CChar -> Int8
cchar_to_int8 = fromIntegral

cchar_to_cchar:: CChar -> CChar
cchar_to_cchar = fromIntegral

cchar_to_cushort:: CChar -> CUShort
cchar_to_cushort = fromIntegral

cchar_to_cuint:: CChar -> CUInt
cchar_to_cuint = fromIntegral

cchar_to_culong:: CChar -> CULong
cchar_to_culong = fromIntegral

cchar_to_cullong:: CChar -> CULLong
cchar_to_cullong = fromIntegral

cchar_to_cschar:: CChar -> CSChar
cchar_to_cschar = fromIntegral

cchar_to_cshort:: CChar -> CShort
cchar_to_cshort = fromIntegral

cchar_to_cint:: CChar -> CInt
cchar_to_cint = fromIntegral

cchar_to_clong:: CChar -> CLong
cchar_to_clong = fromIntegral

cchar_to_cllong:: CChar -> CLLong
cchar_to_cllong = fromIntegral

cushort_to_word64:: CUShort -> Word64
cushort_to_word64 = fromIntegral

cushort_to_word32:: CUShort -> Word32
cushort_to_word32 = fromIntegral

cushort_to_word16:: CUShort -> Word16
cushort_to_word16 = fromIntegral

cushort_to_word8:: CUShort -> Word8
cushort_to_word8 = fromIntegral

cushort_to_int64:: CUShort -> Int64
cushort_to_int64 = fromIntegral

cushort_to_int32:: CUShort -> Int32
cushort_to_int32 = fromIntegral

cushort_to_int16:: CUShort -> Int16
cushort_to_int16 = fromIntegral

cushort_to_int8:: CUShort -> Int8
cushort_to_int8 = fromIntegral

cushort_to_cchar:: CUShort -> CChar
cushort_to_cchar = fromIntegral

cushort_to_cushort:: CUShort -> CUShort
cushort_to_cushort = fromIntegral

cushort_to_cuint:: CUShort -> CUInt
cushort_to_cuint = fromIntegral

cushort_to_culong:: CUShort -> CULong
cushort_to_culong = fromIntegral

cushort_to_cullong:: CUShort -> CULLong
cushort_to_cullong = fromIntegral

cushort_to_cschar:: CUShort -> CSChar
cushort_to_cschar = fromIntegral

cushort_to_cshort:: CUShort -> CShort
cushort_to_cshort = fromIntegral

cushort_to_cint:: CUShort -> CInt
cushort_to_cint = fromIntegral

cushort_to_clong:: CUShort -> CLong
cushort_to_clong = fromIntegral

cushort_to_cllong:: CUShort -> CLLong
cushort_to_cllong = fromIntegral

cuint_to_word64:: CUInt -> Word64
cuint_to_word64 = fromIntegral

cuint_to_word32:: CUInt -> Word32
cuint_to_word32 = fromIntegral

cuint_to_word16:: CUInt -> Word16
cuint_to_word16 = fromIntegral

cuint_to_word8:: CUInt -> Word8
cuint_to_word8 = fromIntegral

cuint_to_int64:: CUInt -> Int64
cuint_to_int64 = fromIntegral

cuint_to_int32:: CUInt -> Int32
cuint_to_int32 = fromIntegral

cuint_to_int16:: CUInt -> Int16
cuint_to_int16 = fromIntegral

cuint_to_int8:: CUInt -> Int8
cuint_to_int8 = fromIntegral

cuint_to_cchar:: CUInt -> CChar
cuint_to_cchar = fromIntegral

cuint_to_cushort:: CUInt -> CUShort
cuint_to_cushort = fromIntegral

cuint_to_cuint:: CUInt -> CUInt
cuint_to_cuint = fromIntegral

cuint_to_culong:: CUInt -> CULong
cuint_to_culong = fromIntegral

cuint_to_cullong:: CUInt -> CULLong
cuint_to_cullong = fromIntegral

cuint_to_cschar:: CUInt -> CSChar
cuint_to_cschar = fromIntegral

cuint_to_cshort:: CUInt -> CShort
cuint_to_cshort = fromIntegral

cuint_to_cint:: CUInt -> CInt
cuint_to_cint = fromIntegral

cuint_to_clong:: CUInt -> CLong
cuint_to_clong = fromIntegral

cuint_to_cllong:: CUInt -> CLLong
cuint_to_cllong = fromIntegral

culong_to_word64:: CULong -> Word64
culong_to_word64 = fromIntegral

culong_to_word32:: CULong -> Word32
culong_to_word32 = fromIntegral

culong_to_word16:: CULong -> Word16
culong_to_word16 = fromIntegral

culong_to_word8:: CULong -> Word8
culong_to_word8 = fromIntegral

culong_to_int64:: CULong -> Int64
culong_to_int64 = fromIntegral

culong_to_int32:: CULong -> Int32
culong_to_int32 = fromIntegral

culong_to_int16:: CULong -> Int16
culong_to_int16 = fromIntegral

culong_to_int8:: CULong -> Int8
culong_to_int8 = fromIntegral

culong_to_cchar:: CULong -> CChar
culong_to_cchar = fromIntegral

culong_to_cushort:: CULong -> CUShort
culong_to_cushort = fromIntegral

culong_to_cuint:: CULong -> CUInt
culong_to_cuint = fromIntegral

culong_to_culong:: CULong -> CULong
culong_to_culong = fromIntegral

culong_to_cullong:: CULong -> CULLong
culong_to_cullong = fromIntegral

culong_to_cschar:: CULong -> CSChar
culong_to_cschar = fromIntegral

culong_to_cshort:: CULong -> CShort
culong_to_cshort = fromIntegral

culong_to_cint:: CULong -> CInt
culong_to_cint = fromIntegral

culong_to_clong:: CULong -> CLong
culong_to_clong = fromIntegral

culong_to_cllong:: CULong -> CLLong
culong_to_cllong = fromIntegral

cullong_to_word64:: CULLong -> Word64
cullong_to_word64 = fromIntegral

cullong_to_word32:: CULLong -> Word32
cullong_to_word32 = fromIntegral

cullong_to_word16:: CULLong -> Word16
cullong_to_word16 = fromIntegral

cullong_to_word8:: CULLong -> Word8
cullong_to_word8 = fromIntegral

cullong_to_int64:: CULLong -> Int64
cullong_to_int64 = fromIntegral

cullong_to_int32:: CULLong -> Int32
cullong_to_int32 = fromIntegral

cullong_to_int16:: CULLong -> Int16
cullong_to_int16 = fromIntegral

cullong_to_int8:: CULLong -> Int8
cullong_to_int8 = fromIntegral

cullong_to_cchar:: CULLong -> CChar
cullong_to_cchar = fromIntegral

cullong_to_cushort:: CULLong -> CUShort
cullong_to_cushort = fromIntegral

cullong_to_cuint:: CULLong -> CUInt
cullong_to_cuint = fromIntegral

cullong_to_culong:: CULLong -> CULong
cullong_to_culong = fromIntegral

cullong_to_cullong:: CULLong -> CULLong
cullong_to_cullong = fromIntegral

cullong_to_cschar:: CULLong -> CSChar
cullong_to_cschar = fromIntegral

cullong_to_cshort:: CULLong -> CShort
cullong_to_cshort = fromIntegral

cullong_to_cint:: CULLong -> CInt
cullong_to_cint = fromIntegral

cullong_to_clong:: CULLong -> CLong
cullong_to_clong = fromIntegral

cullong_to_cllong:: CULLong -> CLLong
cullong_to_cllong = fromIntegral

cschar_to_word64:: CSChar -> Word64
cschar_to_word64 = fromIntegral

cschar_to_word32:: CSChar -> Word32
cschar_to_word32 = fromIntegral

cschar_to_word16:: CSChar -> Word16
cschar_to_word16 = fromIntegral

cschar_to_word8:: CSChar -> Word8
cschar_to_word8 = fromIntegral

cschar_to_int64:: CSChar -> Int64
cschar_to_int64 = fromIntegral

cschar_to_int32:: CSChar -> Int32
cschar_to_int32 = fromIntegral

cschar_to_int16:: CSChar -> Int16
cschar_to_int16 = fromIntegral

cschar_to_int8:: CSChar -> Int8
cschar_to_int8 = fromIntegral

cschar_to_cchar:: CSChar -> CChar
cschar_to_cchar = fromIntegral

cschar_to_cushort:: CSChar -> CUShort
cschar_to_cushort = fromIntegral

cschar_to_cuint:: CSChar -> CUInt
cschar_to_cuint = fromIntegral

cschar_to_culong:: CSChar -> CULong
cschar_to_culong = fromIntegral

cschar_to_cullong:: CSChar -> CULLong
cschar_to_cullong = fromIntegral

cschar_to_cschar:: CSChar -> CSChar
cschar_to_cschar = fromIntegral

cschar_to_cshort:: CSChar -> CShort
cschar_to_cshort = fromIntegral

cschar_to_cint:: CSChar -> CInt
cschar_to_cint = fromIntegral

cschar_to_clong:: CSChar -> CLong
cschar_to_clong = fromIntegral

cschar_to_cllong:: CSChar -> CLLong
cschar_to_cllong = fromIntegral

cshort_to_word64:: CShort -> Word64
cshort_to_word64 = fromIntegral

cshort_to_word32:: CShort -> Word32
cshort_to_word32 = fromIntegral

cshort_to_word16:: CShort -> Word16
cshort_to_word16 = fromIntegral

cshort_to_word8:: CShort -> Word8
cshort_to_word8 = fromIntegral

cshort_to_int64:: CShort -> Int64
cshort_to_int64 = fromIntegral

cshort_to_int32:: CShort -> Int32
cshort_to_int32 = fromIntegral

cshort_to_int16:: CShort -> Int16
cshort_to_int16 = fromIntegral

cshort_to_int8:: CShort -> Int8
cshort_to_int8 = fromIntegral

cshort_to_cchar:: CShort -> CChar
cshort_to_cchar = fromIntegral

cshort_to_cushort:: CShort -> CUShort
cshort_to_cushort = fromIntegral

cshort_to_cuint:: CShort -> CUInt
cshort_to_cuint = fromIntegral

cshort_to_culong:: CShort -> CULong
cshort_to_culong = fromIntegral

cshort_to_cullong:: CShort -> CULLong
cshort_to_cullong = fromIntegral

cshort_to_cschar:: CShort -> CSChar
cshort_to_cschar = fromIntegral

cshort_to_cshort:: CShort -> CShort
cshort_to_cshort = fromIntegral

cshort_to_cint:: CShort -> CInt
cshort_to_cint = fromIntegral

cshort_to_clong:: CShort -> CLong
cshort_to_clong = fromIntegral

cshort_to_cllong:: CShort -> CLLong
cshort_to_cllong = fromIntegral

cint_to_word64:: CInt -> Word64
cint_to_word64 = fromIntegral

cint_to_word32:: CInt -> Word32
cint_to_word32 = fromIntegral

cint_to_word16:: CInt -> Word16
cint_to_word16 = fromIntegral

cint_to_word8:: CInt -> Word8
cint_to_word8 = fromIntegral

cint_to_int64:: CInt -> Int64
cint_to_int64 = fromIntegral

cint_to_int32:: CInt -> Int32
cint_to_int32 = fromIntegral

cint_to_int16:: CInt -> Int16
cint_to_int16 = fromIntegral

cint_to_int8:: CInt -> Int8
cint_to_int8 = fromIntegral

cint_to_cchar:: CInt -> CChar
cint_to_cchar = fromIntegral

cint_to_cushort:: CInt -> CUShort
cint_to_cushort = fromIntegral

cint_to_cuint:: CInt -> CUInt
cint_to_cuint = fromIntegral

cint_to_culong:: CInt -> CULong
cint_to_culong = fromIntegral

cint_to_cullong:: CInt -> CULLong
cint_to_cullong = fromIntegral

cint_to_cschar:: CInt -> CSChar
cint_to_cschar = fromIntegral

cint_to_cshort:: CInt -> CShort
cint_to_cshort = fromIntegral

cint_to_cint:: CInt -> CInt
cint_to_cint = fromIntegral

cint_to_clong:: CInt -> CLong
cint_to_clong = fromIntegral

cint_to_cllong:: CInt -> CLLong
cint_to_cllong = fromIntegral

clong_to_word64:: CLong -> Word64
clong_to_word64 = fromIntegral

clong_to_word32:: CLong -> Word32
clong_to_word32 = fromIntegral

clong_to_word16:: CLong -> Word16
clong_to_word16 = fromIntegral

clong_to_word8:: CLong -> Word8
clong_to_word8 = fromIntegral

clong_to_int64:: CLong -> Int64
clong_to_int64 = fromIntegral

clong_to_int32:: CLong -> Int32
clong_to_int32 = fromIntegral

clong_to_int16:: CLong -> Int16
clong_to_int16 = fromIntegral

clong_to_int8:: CLong -> Int8
clong_to_int8 = fromIntegral

clong_to_cchar:: CLong -> CChar
clong_to_cchar = fromIntegral

clong_to_cushort:: CLong -> CUShort
clong_to_cushort = fromIntegral

clong_to_cuint:: CLong -> CUInt
clong_to_cuint = fromIntegral

clong_to_culong:: CLong -> CULong
clong_to_culong = fromIntegral

clong_to_cullong:: CLong -> CULLong
clong_to_cullong = fromIntegral

clong_to_cschar:: CLong -> CSChar
clong_to_cschar = fromIntegral

clong_to_cshort:: CLong -> CShort
clong_to_cshort = fromIntegral

clong_to_cint:: CLong -> CInt
clong_to_cint = fromIntegral

clong_to_clong:: CLong -> CLong
clong_to_clong = fromIntegral

clong_to_cllong:: CLong -> CLLong
clong_to_cllong = fromIntegral

cllong_to_word64:: CLLong -> Word64
cllong_to_word64 = fromIntegral

cllong_to_word32:: CLLong -> Word32
cllong_to_word32 = fromIntegral

cllong_to_word16:: CLLong -> Word16
cllong_to_word16 = fromIntegral

cllong_to_word8:: CLLong -> Word8
cllong_to_word8 = fromIntegral

cllong_to_int64:: CLLong -> Int64
cllong_to_int64 = fromIntegral

cllong_to_int32:: CLLong -> Int32
cllong_to_int32 = fromIntegral

cllong_to_int16:: CLLong -> Int16
cllong_to_int16 = fromIntegral

cllong_to_int8:: CLLong -> Int8
cllong_to_int8 = fromIntegral

cllong_to_cchar:: CLLong -> CChar
cllong_to_cchar = fromIntegral

cllong_to_cushort:: CLLong -> CUShort
cllong_to_cushort = fromIntegral

cllong_to_cuint:: CLLong -> CUInt
cllong_to_cuint = fromIntegral

cllong_to_culong:: CLLong -> CULong
cllong_to_culong = fromIntegral

cllong_to_cullong:: CLLong -> CULLong
cllong_to_cullong = fromIntegral

cllong_to_cschar:: CLLong -> CSChar
cllong_to_cschar = fromIntegral

cllong_to_cshort:: CLLong -> CShort
cllong_to_cshort = fromIntegral

cllong_to_cint:: CLLong -> CInt
cllong_to_cint = fromIntegral

cllong_to_clong:: CLLong -> CLong
cllong_to_clong = fromIntegral

cllong_to_cllong:: CLLong -> CLLong
cllong_to_cllong = fromIntegral


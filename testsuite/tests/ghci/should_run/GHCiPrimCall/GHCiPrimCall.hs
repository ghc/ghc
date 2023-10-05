{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes, MagicHash, UnboxedTuples #-}

module Main where

import GHC.Int
import GHC.Word
import GHC.Prim
import GHC.Exts
import GHC.Types
import Unsafe.Coerce
import GHC.IO

data Box (a :: UnliftedType) = MkBox a

i2a :: Int -> Any
i2a = unsafeCoerce

a2i :: Any -> Int
a2i = unsafeCoerce

main :: IO ()
main = do

    let i8s :: [Int8]
        i8s = [0, -1, 1, minBound, maxBound]

        i16s :: [Int16]
        i16s = [0, -1, 1, minBound, maxBound]

        i32s :: [Int32]
        i32s = [0, -1, 1, minBound, maxBound]

        i64s :: [Int64]
        i64s = [0, -1, 1, minBound, maxBound]

        ws :: [Word]
        ws = [0, 1, 2, maxBound]

        fs :: [Float]
        fs = [-0, 0, -1, 1, -2, 2, 1/0, 1e37, -1e-37]

        ds :: [Double]
        ds = [-0, 0, -1, 1, -2, 2, 1/0, 1e307, -1e-307]

        ls :: [Word64]
        ls = [0, 1, 2, 4294967296, maxBound]

        mi2a :: Maybe Integer -> Any
        mi2a = unsafeCoerce

        a2mi :: Any -> Maybe Integer
        a2mi = unsafeCoerce

        a0, a1, a2, a3 :: Any
        a0 = mi2a Nothing
        a1 = mi2a (Just 1)
        a2 = mi2a (Just 18446744073709551617)
        a3 = mi2a (Just (-18446744073709551617))

        as :: [Any]
        as = [a0, a1, a2, a3]


    putStrLn "zero arguments"
    IO (\st -> case cmm_zero st of (# st' #) -> (# st', () #))
    print =<< IO (\st -> case cmm_zero_w st of (# st', w #) -> (# st', W# w #))
    print =<< IO (\st -> case cmm_zero_d st of (# st', w #) -> (# st', D# w #))
    print =<< IO (\st -> case cmm_zero_f st of (# st', w #) -> (# st', F# w #))

    -- XXX zero argument functions

    putStrLn "one argument functions"
    print [ I8# (cmm_one1_i8 x) | (I8# x) <- i8s ]
    print [ I16# (cmm_one1_i16 x) | (I16# x) <- i16s ]
    print [ I32# (cmm_one1_i32 x) | (I32# x) <- i32s ]
    print [ I64# (cmm_one1_i64 x) | (I64# x) <- i64s ]
    print [ case cmm_one1_p x of (# y #) -> a2mi y | x <- as ]
    print [ W# (cmm_one1_w x) | (W# x) <- ws ]
    print [ F# (cmm_one1_f x) | (F# x) <- fs ]
    print [ D# (cmm_one1_d x) | (D# x) <- ds ]
    print [ W64# (cmm_one1_l x) | (W64# x) <- ls ]

    print [ case cmm_one2_i8 x of (# y1, y2 #) -> (I8# y1, I8# y2) | (I8# x) <- i8s ]
    print [ case cmm_one2_i16 x of (# y1, y2 #) -> (I16# y1, I16# y2) | (I16# x) <- i16s ]
    print [ case cmm_one2_i32 x of (# y1, y2 #) -> (I32# y1, I32# y2) | (I32# x) <- i32s ]
    print [ case cmm_one2_i64 x of (# y1, y2 #) -> (I64# y1, I64# y2) | (I64# x) <- i64s ]
    print [ case cmm_one2_p x of (# y1, y2 #) -> (a2mi y1, a2mi y2) | x <- as ]
    print [ case cmm_one2_w x of (# y1, y2 #) -> (W# y1, W# y2) | (W# x) <- ws ]
    print [ case cmm_one2_f x of (# y1, y2 #) -> (F# y1, F# y2) | (F# x) <- fs ]
    print [ case cmm_one2_d x of (# y1, y2 #) -> (D# y1, D# y2) | (D# x) <- ds ]
    print [ case cmm_one2_l x of (# y1, y2 #) -> (W64# y1, W64# y2) | (W64# x) <- ls ]

    putStrLn "two argument functions"
    print [ I8# (cmm_two1_i8 x1 x2) | (I8# x1) <- i8s, (I8# x2) <- i8s ]
    print [ I16# (cmm_two1_i16 x1 x2) | (I16# x1) <- i16s, (I16# x2) <- i16s ]
    print [ I32# (cmm_two1_i32 x1 x2) | (I32# x1) <- i32s, (I32# x2) <- i32s ]
    print [ I64# (cmm_two1_i64 x1 x2) | (I64# x1) <- i64s, (I64# x2) <- i64s ]
    print [ case cmm_two1_p x1 x2 of (# y #) -> a2mi y | x1 <- as, x2 <- as ]
    print [ W# (cmm_two1_w x1 x2) | (W# x1) <- ws, (W# x2) <- ws ]
    print [ F# (cmm_two1_f x1 x2) | (F# x1) <- fs, (F# x2) <- fs ]
    print [ D# (cmm_two1_d x1 x2) | (D# x1) <- ds, (D# x2) <- ds ]
    print [ W64# (cmm_two1_l x1 x2) | (W64# x1) <- ls, (W64# x2) <- ls ]

    print [ I8# (cmm_two2_i8 x1 x2) | (I8# x1) <- i8s, (I8# x2) <- i8s ]
    print [ I16# (cmm_two2_i16 x1 x2) | (I16# x1) <- i16s, (I16# x2) <- i16s ]
    print [ I32# (cmm_two2_i32 x1 x2) | (I32# x1) <- i32s, (I32# x2) <- i32s ]
    print [ I64# (cmm_two2_i64 x1 x2) | (I64# x1) <- i64s, (I64# x2) <- i64s ]
    print [ case cmm_two2_p x1 x2 of (# y #) -> a2mi y | x1 <- as, x2 <- as ]
    print [ W# (cmm_two2_w x1 x2) | (W# x1) <- ws, (W# x2) <- ws ]
    print [ F# (cmm_two2_f x1 x2) | (F# x1) <- fs, (F# x2) <- fs ]
    print [ D# (cmm_two2_d x1 x2) | (D# x1) <- ds, (D# x2) <- ds ]
    print [ W64# (cmm_two2_l x1 x2) | (W64# x1) <- ls, (W64# x2) <- ls ]

    putStrLn "additional floating point tests"
    print [ F# (cmm_floating_1 x1 x2) | (F# x1) <- fs, (F# x2) <- fs ]
    print [ D# (cmm_floating_2 x1 x2) | (D# x1) <- ds, (D# x2) <- ds ]
    print [ F# (cmm_floating_3 x1 x2) | (F# x1) <- fs, (D# x2) <- ds ]
    print [ D# (cmm_floating_4 x1 x2) | (F# x1) <- fs, (D# x2) <- ds ]
    print [ case cmm_floating_5 x1 x2 3.0e1# 4.0e2## 5.0e3# 6.0e4## 7.0e5# 8.0e6## of (# y1, y2 #) -> (F# y1, D# y2)
          | (F# x1) <- fs, (D# x2) <- ds ]
    print [ case cmm_floating_6 x1 x2 3.0e1## 4.0e2# 5.0e3## 6.0e4# 7.0e5## 8.0e6# of (# y1, y2 #) -> (D# y1, F# y2)
          | (D# x1) <- ds, (F# x2) <- fs ]
    print [ case cmm_floating_7 w1 x1 x2
                                4## 5.0e1# 6.0e2##
                                7## 8.0e3# 9.0e4##
                                10## 11.0e5# 12.0e6##
                                13## 14.0e7# 15.0e8##
                                16## 17.0e9# 18.0e10##
                                19## 20.0e11# 21.0e12## of (# y1, y2, y3 #) -> (W# y1, F# y2, D# y3)
          | (W# w1) <- ws, (F# x1) <- fs, (D# x2) <- ds ]

    putStrLn "various sized tuple returns"
    print [ case cmm_tuple_1 x1 x2 3## of (# y1, y2, y3 #) -> (W# y1, W# y2, W# y3) | (W# x1) <- ws, (W# x2) <- ws]
    print [ case cmm_tuple_2 x1 x2 3## 4## a0 a1 7.0## 8.0## a2 a3 11.0# 12.0#
            of (# y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12 #) ->
                (F# y1, F# y2, a2mi y3, a2mi y4, D# y5, D# y6, a2mi y7, a2mi y8, W# y9, W# y10, a2mi y11, a2mi y12)
          | x1 <- as, x2 <- as ]
    print [ case cmm_tuple_3 x1 x2 a0 3.0## a1 5.0#
            of (# y1, y2, y3, y4, y5, y6 #) -> (F# y1, a2mi y2, D# y3, a2mi y4, W# y5, a2mi y6)
          | x1 <- as, (W# x2) <- ws ]
    print [ case cmm_tuple_4 x1 x2 of (# y1, y2 #) -> (a2mi y1, a2mi y2) | x1 <- as, x2 <- as ]

    putStrLn "arrays"
    MkBox marr0 <- IO (\s -> case newArray# 10# a0 s of (# s', a #) -> (# s', MkBox a #))
    MkBox marr1 <- IO (\s -> case newArray# 12# a1 s of (# s', a #) -> (# s', MkBox a #))
    MkBox arr0 <- IO (\s -> case unsafeFreezeArray# marr0 s of (# s', a #) -> (# s', MkBox a #))
    MkBox arr1 <- IO (\s -> case unsafeFreezeArray# marr1 s of (# s', a #) -> (# s', MkBox a #))
    print (I# (cmm_array_1 arr0), I# (cmm_array_1 arr1))
    case cmm_array_2 arr0 arr1 of (# arr2, arr3 #) -> print (I# (cmm_array_1 arr2), I# (cmm_array_1 arr3))

    putStrLn "many arguments"
    print (W# (cmm_many_arguments 1## 2## 3## 4## 5## 6## 7## 8## 9## 10## 11## 12## 13## 14## 15## 16## 17## 18## 19## 20## 21## 22## 23## 24## 25## 26## 27## 28## 29## 30## 31## 32## 33## 34## 35## 36## 37## 38## 39## 40## 41## 42## 43## 44## 45## 46## 47## 48## 49## 50## 51## 52## 53## 54## 55## 56## 57## 58## 59## 60## 61## 62## 63## 64## 65## 66## 67## 68## 69## 70## 71## 72## 73## 74## 75## 76## 77## 78## 79## 80## 81## 82## 83## 84## 85## 86## 87## 88## 89## 90## 91## 92## 93## 94## 95## 96## 97## 98## 99## 100## 101## 102## 103## 104## 105## 106## 107## 108## 109## 110## 111## 112## 113## 114## 115## 116## 117## 118## 119## 120## 121## 122## 123## 124## 125## 126## 127## 128## 129## 130## 131## 132## 133## 134## 135## 136## 137## 138## 139## 140## 141## 142## 143## 144## 145## 146## 147## 148## 149## 150## 151## 152## 153## 154## 155## 156## 157## 158## 159## 160## 161## 162## 163## 164## 165## 166## 167## 168## 169## 170## 171## 172## 173## 174## 175## 176## 177## 178## 179## 180## 181## 182## 183## 184## 185## 186## 187## 188## 189## 190## 191## 192## 193## 194## 195## 196## 197## 198## 199## 200## 201## 202## 203## 204## 205## 206## 207## 208## 209## 210## 211## 212## 213## 214## 215## 216## 217## 218## 219## 220## 221## 222## 223## 224## 225## 226## 227## 228## 229## 230## 231## 232## 233## 234## 235## 236## 237## 238## 239## 240## 241## 242## 243## 244## 245## 246## 247## 248## 249## 250## 251## 252## 253## 254## 255## 256## 257## 258## 259## 260## 261## 262## 263## 264## 265## 266## 267## 268## 269## 270## 271## 272## 273## 274## 275## 276## 277## 278## 279## 280## 281## 282## 283## 284## 285## 286## 287## 288## 289## 290## 291## 292## 293## 294## 295## 296## 297## 298## 299## 300## 301## 302## 303## 304## 305## 306## 307## 308## 309## 310## 311## 312## 313## 314## 315## 316## 317## 318## 319## 320## 321## 322## 323## 324## 325## 326## 327## 328## 329## 330## 331## 332## 333## 334## 335## 336## 337## 338## 339## 340## 341## 342## 343## 344## 345## 346## 347## 348## 349## 350## 351## 352## 353## 354## 355## 356## 357## 358## 359## 360## 361## 362## 363## 364## 365## 366## 367## 368## 369## 370## 371## 372## 373## 374## 375## 376## 377## 378## 379## 380## 381## 382## 383## 384## 385## 386## 387## 388## 389## 390## 391## 392## 393## 394## 395## 396## 397## 398## 399## 400##))

-- XXX why don't we accept State# RealWorld -> State# RealWorld ?
foreign import prim cmm_zero :: State# RealWorld -> (# State# RealWorld #)
foreign import prim cmm_zero_w :: State# RealWorld -> (# State# RealWorld, Word# #)
foreign import prim cmm_zero_d :: State# RealWorld -> (# State# RealWorld, Double# #)
foreign import prim cmm_zero_f :: State# RealWorld -> (# State# RealWorld, Float# #)

-- one argument functions
foreign import prim cmm_one1_i8 :: Int8# -> Int8#
foreign import prim cmm_one1_i16 :: Int16# -> Int16#
foreign import prim cmm_one1_i32 :: Int32# -> Int32#
foreign import prim cmm_one1_i64 :: Int64# -> Int64#
foreign import prim cmm_one1_p :: Any -> (# Any #)
foreign import prim cmm_one1_w :: Word# -> Word#
foreign import prim cmm_one1_f :: Float# -> Float#
foreign import prim cmm_one1_d :: Double# -> Double#
foreign import prim cmm_one1_l :: Word64# -> Word64#

foreign import prim cmm_one2_i8 :: Int8# -> (# Int8#, Int8# #)
foreign import prim cmm_one2_i16 :: Int16# -> (# Int16#, Int16# #)
foreign import prim cmm_one2_i32 :: Int32# -> (# Int32#, Int32# #)
foreign import prim cmm_one2_i64 :: Int64# -> (# Int64#, Int64# #)
foreign import prim cmm_one2_p :: Any -> (# Any, Any #)
foreign import prim cmm_one2_w :: Word# -> (# Word#, Word# #)
foreign import prim cmm_one2_f :: Float# -> (# Float#, Float# #)
foreign import prim cmm_one2_d :: Double# -> (# Double#, Double# #)
foreign import prim cmm_one2_l :: Word64# -> (# Word64#, Word64# #)

-- two argument functions
foreign import prim cmm_two1_i8 :: Int8# -> Int8# -> Int8#
foreign import prim cmm_two1_i16 :: Int16# -> Int16# -> Int16#
foreign import prim cmm_two1_i32 :: Int32# -> Int32# -> Int32#
foreign import prim cmm_two1_i64 :: Int64# -> Int64# -> Int64#
foreign import prim cmm_two1_p :: Any -> Any -> (# Any #)
foreign import prim cmm_two1_w :: Word# -> Word# -> Word#
foreign import prim cmm_two1_f :: Float# -> Float# -> Float#
foreign import prim cmm_two1_d :: Double# -> Double# -> Double#
foreign import prim cmm_two1_l :: Word64# -> Word64# -> Word64#

foreign import prim cmm_two2_i8 :: Int8# -> Int8# -> Int8#
foreign import prim cmm_two2_i16 :: Int16# -> Int16# -> Int16#
foreign import prim cmm_two2_i32 :: Int32# -> Int32# -> Int32#
foreign import prim cmm_two2_i64 :: Int64# -> Int64# -> Int64#
foreign import prim cmm_two2_p :: Any -> Any -> (# Any #)
foreign import prim cmm_two2_w :: Word# -> Word# -> Word#
foreign import prim cmm_two2_f :: Float# -> Float# -> Float#
foreign import prim cmm_two2_d :: Double# -> Double# -> Double#
foreign import prim cmm_two2_l :: Word64# -> Word64# -> Word64#

{- additional tests for floating point, since D_ and F_ registers
   overlap on some platforms -}

foreign import prim cmm_floating_1 :: Float# -> Float# -> Float#
foreign import prim cmm_floating_2 :: Double# -> Double# -> Double#
foreign import prim cmm_floating_3 :: Float# -> Double# -> Float#
foreign import prim cmm_floating_4 :: Float# -> Double# -> Double#
foreign import prim cmm_floating_5 :: Float# -> Double# -> Float# -> Double# -> Float# -> Double# -> Float# -> Double# -> (# Float#, Double# #)
foreign import prim cmm_floating_6 :: Double# -> Float# -> Double# -> Float# -> Double# -> Float# -> Double# -> Float# -> (# Double#, Float# #)
foreign import prim cmm_floating_7 :: Word# -> Float# -> Double# -> Word# -> Float# -> Double# -> Word# -> Float# -> Double# -> Word# -> Float# -> Double# -> Word# -> Float# -> Double# -> Word# -> Float# -> Double# -> Word# -> Float# -> Double# -> (# Word#, Float#, Double# #)
-- various sized tuple returns

foreign import prim cmm_tuple_1 :: Word# -> Word# -> Word# -> (# Word# , Word#, Word# #)
foreign import prim cmm_tuple_2 :: Any -> Any -> Word# -> Word# -> Any -> Any -> Double# -> Double# -> Any -> Any -> Float# -> Float# ->
    (# Float#, Float#, Any, Any, Double#, Double#, Any, Any, Word#, Word#, Any, Any #)
foreign import prim cmm_tuple_3 :: Any -> Word# -> Any -> Double# -> Any -> Float# ->
    (# Float#, Any, Double#, Any, Word#, Any #)
foreign import prim cmm_tuple_4 :: Any -> Any -> (# Any, Any #)

-- boxed primitive types

-- get the length of an array
foreign import prim cmm_array_1 :: Array# Any -> Int#
-- return some arrays
foreign import prim cmm_array_2 :: Array# Any -> Array# Any -> (# Array# Any, Array# Any #)

foreign import prim cmm_many_arguments ::
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# ->
    Word#

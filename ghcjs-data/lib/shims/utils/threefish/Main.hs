{-# LANGUAGE QuasiQuotes #-}
{-
  Generator for a JavaScript implementation of the Skein block function.
  Based on the public domain C implementation by Doug Whiting.

  Code size is ~1.7kB zipped after minification when fully unrolled
  performance ~ 50MB/s on v8 and SpiderMonkey

  author: Luite Stegeman - 2014
 -}

module Main where

import Data.Bits
import Data.Maybe
import Data.Monoid

import System.IO

import Language.Javascript.JMacro
import Text.PrettyPrint.Leijen.Text (renderPretty, displayIO)
import qualified Control.Exception as E
import System.Process

--------------------------------------------------------------------------------
-- configuration

-- number of rounds
nRounds :: Int
nRounds = 72

-- arguments are byte arrays, no separate offsets
byteArray :: Bool
byteArray = True

-- completely unroll all rounds, eliminating the loop
unroll :: Bool
unroll  = True

-- store w in local variables instead of an array
localW :: Bool
localW = True

-- store kw (ks and ts) in local variables (only for unrolled loop!)
localKw :: Bool
localKw = True

-- inline the 64 bit addition calls
inlineAdd :: Bool
inlineAdd = True

--------------------------------------------------------------------------------
-- constants

-- key schedule parity
skein_ks_parity_a, skein_ks_parity_b :: Integer
skein_ks_parity_a = 0xA9FC1A22
skein_ks_parity_b = 0x1BD11BDA

-- rotation constants for each round
threefish_rotation :: [(Int,Int)]
threefish_rotation =
  [ (14,16), (52,57), (23,40), (5,37)
  , (25,33), (46,12), (58,22), (32,32)
  ]

--------------------------------------------------------------------------------
-- yep we have to emulate 64 bit arithmetic

data JInt64
  = JInt64Var  Ident Ident
  | JInt64Arr  Ident Int (Maybe JExpr)
  | JInt64Val  Integer
  deriving Show

val :: Integer -> JInt64
val = JInt64Val

var :: Ident -> JExpr
var = ValExpr . JVar

-- expressions
eA, eB :: JInt64 -> JExpr
eA (JInt64Var a _)    = var a
eA (JInt64Arr a n me) = [jmacroE| `var a`[`arrE (2*n) me`] |]
eA (JInt64Val n)      = toJExpr (n .&. 0xFFFFFFFF)

eB (JInt64Var _ b)    = var b
eB (JInt64Arr a n me) = [jmacroE| `var a`[`arrE (2*n+1) me`] |]
eB (JInt64Val n)      = toJExpr $ (n `shiftR` 32) .&. 0xFFFFFFFF

arrE :: Int -> Maybe JExpr -> JExpr
arrE 0 me = fromMaybe (toJExpr (0::Integer)) me
arrE n me = case me of
              Nothing -> toJExpr n
              Just e  -> [jmacroE| `n`+2*`e` |]

-- lvalues
lvA, lvB :: JInt64 -> JExpr
lvA j@(JInt64Val{}) = error ("not an lvalue: " ++ show j)
lvA j               = eA j
lvB j@(JInt64Val{}) = error ("not an lvalue: " ++ show j)
lvB j               = eB j

x :: Int -> JInt64
x n
  | n >=0 && n <= 3 = JInt64Var (StrI $ 'x':show n++"a") (StrI $ 'x':show n++"b")
  | otherwise       = error ("x out of range: " ++ show n)


declI64 :: String -> JStat
declI64 name = DeclStat (StrI $ name ++ "a") Nothing <>
               DeclStat (StrI $ name ++ "b") Nothing

declWGlobal, declWLocal :: JStat
declWGlobal
  | localW    = mempty
  | otherwise = [jmacro| var !h$Threefish_w  = new Int32Array(8); |]
declWLocal
  | localW    = mconcat $ map (declI64.(('w':).show)) [0..3]
  | otherwise = [jmacro| var !_w = $Threefish_w; |]

w :: Int -> JInt64
w n | n < 0 || n > 3 = error "w: out of range"
    | localW    = JInt64Var (StrI $ "w" ++ show n ++ "a") (StrI $ "w" ++ show n ++ "b")
    | otherwise = JInt64Arr (StrI "_w") n Nothing

declKwGlobal, declKwLocal :: JStat
declKwGlobal
    | localKw && unroll = mempty
    | localKw           = error "localKw is only available when unrolled"
    | otherwise =
        let n = if unroll then 16 else 16 + nRounds * 4
        in  [jmacro| var !h$Threefish_kw = new Int32Array(`n`); |]
declKwLocal
  | localKw && unroll = mconcat $ map (declI64.(("kw"++).show)) [0..7]
  | localKw           = error "localKw is only available when unrolled"
  | otherwise         = [jmacro| var !_kw = $Threefish_kw; |]

kw :: Int -> Maybe JExpr -> JInt64
kw n e
  | localKw && isJust e = error "dynamic offset with localKw"
  | localKw             = JInt64Var (StrI $ "kw" ++ show n ++ "a") (StrI $ "kw" ++ show n ++ "b")
  | otherwise           = JInt64Arr (StrI "_kw") n e

kw_key_base, kw_twk_base :: Int
kw_key_base = 3
kw_twk_base = 0

ks, ts :: Int -> JInt64
ks n = kw (n + kw_key_base) Nothing
ts n = kw (n + kw_twk_base) Nothing

ks', ts' :: Int -> JExpr -> JInt64
ks' n e = kw (n + kw_key_base) (Just e)
ts' n e = kw (n + kw_twk_base) (Just e)

-- x = y + z
add_64 :: JInt64 -> JInt64 -> JInt64 -> JStat
add_64 x y z = jadd64 (eA x) (eB x) (eA y) (eB y) (eA z) (eB z)

-- x = y + z + w
add3_64 :: JInt64 -> JInt64 -> JInt64 -> JInt64 -> JStat
add3_64 x y z w =
  jadd3_64 (eA x) (eB x) (eA y) (eB y) (eA z) (eB z) (eA w) (eB w)

-- x += y
addTo_64 :: JInt64 -> JInt64 -> JStat
addTo_64 x y = jadd64 (eA x) (eB x) (eA x) (eB x) (eA y) (eB y)

-- x += y + z
addTo2_64 :: JInt64 -> JInt64 -> JInt64 -> JStat
addTo2_64 x y z =
  jadd3_64 (eA x) (eB x) (eA x) (eB x) (eA y) (eB y) (eA z) (eB z)

-- x += y + c  (c must be in [0,2^31-1] )
addTo2_64_c :: JInt64 -> JInt64 -> Int -> JStat
addTo2_64_c x y 0 = addTo_64 x y
addTo2_64_c x y c =
  jadd3_64 (eA x) (eB x) (eA x) (eB x) (eA y) (eB y) (toJExpr c) (toJExpr (0::Int))

-- add a regular JS number to an int64, must be small!
addNumTo_64 :: JInt64 -> Int -> Maybe JExpr -> JStat
addNumTo_64 x 0 Nothing = mempty
addNumTo_64 x n me =
  let n' = toJExpr n
      e  = maybe n' (\ee -> [jmacroE| `n'`+`ee` |]) me
  in jadd64 (eA x) (eB x) (eA x) (eB x) e [jmacroE| 0 |]

-- x ^= y
xorTo_64 :: JInt64 -> JInt64 -> JStat
xorTo_64 x y = [jmacro| `eA x` ^= `eA y`;
                        `eB x` ^= `eB y`;
                      |]

-- x = y + z
jadd64 :: JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JStat
jadd64 xa xb ya yb za zb
  | inlineAdd = add64Body (Just xa) xb ya yb za zb
  | otherwise = [jmacro|
  `xa` = add64(`ya`,`yb`,`za`,`zb`);
  `xb` = add64_ret1;
|]

-- x = y + z + w
jadd3_64 :: JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JStat
jadd3_64 xa xb ya yb za zb wa wb
  | inlineAdd = add3_64Body (Just xa) xb ya yb za zb wa wb
  | otherwise = [jmacro|
  `xa` = add3_64(`ya`,`yb`,`za`,`zb`, `wa`, `wb`);
  `xb` = add64_ret1;
|]

add64Decl :: JStat
add64Decl
  | inlineAdd = add64LocalDecl
  | otherwise = [jmacro| var !add64_ret1;
                         function !add64(xa,xb,ya,yb) {
                           `add64LocalDecl`;
                           `add64Body Nothing add64_ret1 xa xb ya yb`;
                         }
                         function !add3_64(xa,xb,ya,yb,za,zb) {
                           `add64LocalDecl`;
                           `add3_64Body Nothing add64_ret1 xa xb ya yb za zb`;
                         }

                       |]

add64LocalDecl =
  [jmacro| var !c1, !c0; |]

add64Body :: Maybe JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JStat
add64Body ta tb xa xb ya yb = [jmacro|
  c0 = (`xa` & 0xFFFFFF) + (`ya` & 0xFFFFFF);
  c1 = (c0 >>> 24) + (`xa` >>> 24) + (`ya` >>> 24) +
                       ((`xb` & 0xFFFF)<<8) + ((`yb` & 0xFFFF) << 8);
  `tb` = (((c1 >>> 24) + (`xb` >>> 16) + (`yb` >>> 16))  << 16) +
         ((c1 >> 8) & 0xFFFF);
  `r`;
|] where
     r = let v = [jmacroE| (c1 << 24) | (c0 & 0xFFFFFF) |]
         in case ta of
              Nothing -> [jmacro| return `v`; |]
              Just e  -> [jmacro| `e` = `v`;  |]

add3_64Body :: Maybe JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> JExpr
            -> JExpr -> JExpr -> JStat
add3_64Body ta tb xa xb ya yb za zb = [jmacro|
  c0 = (`xa` & 0xFFFFFF) + (`ya` & 0xFFFFFF) + (`za` & 0xFFFFFF);
  c1 = (c0 >>> 24) + (`xa` >>> 24) + (`ya` >>> 24) + (`za` >>> 24) +
        ((`xb` & 0xFFFF)<<8) + ((`yb` & 0xFFFF) << 8) + ((`zb` & 0xFFFF)<<8);
  `tb` = (((c1 >>> 24) + (`xb` >>> 16) + (`yb` >>> 16) + (`zb` >>> 16)) << 16) +
         ((c1 >> 8) & 0xFFFF);
  `r`;
|] where
     r = let v = [jmacroE| (c1 << 24) | (c0 & 0xFFFFFF) |]
         in case ta of
              Nothing -> [jmacro| return `v`; |]
              Just e  -> [jmacro| `e` = `v`;  |]

-- x = rotl(x,r)
-- uses tmp1
rotL_64 :: JInt64 -> Int -> JStat
rotL_64 x r
  | r > 63 || r < 0 = error ("rotL: invalid argument: " ++ show r)
  | r == 0          = mempty
  | r == 32         = [jmacro| tmp1   = `eB x`;
                               `eB x` = `eA x`;
                               `eA x` = tmp1;
                             |]
  | r < 32          = [jmacro| tmp1   = `eB x`;
                               `eB x` = (`eB x` << `r`) | (`eA x` >>> `32-r`);
                               `eA x` = (`eA x` << `r`) | (tmp1 >>> `32-r`);
                             |]
  | otherwise       = [jmacro| tmp1   = `eB x`;
                               `eB x` = (`eA x` << `r-32`) | (`eB x` >>> `64-r`);
                               `eA x` = (tmp1 << `r-32`) | (`eA x` >>> `64-r`);
                             |]

-- x = rotl(x,r) ^ y
-- uses tmp1
rotL_64_xor :: JInt64 -> Int -> JInt64 -> JStat
rotL_64_xor x r y
  | r > 63 || r < 0 = error ("rotL: invalid argument: " ++ show r)
  | r == 0          = [jmacro| `eA x` ^= `eA y`;
                               `eB x` ^= `eB y`;
                             |]
  | r == 32         = [jmacro| tmp1   = `eB x`;
                               `eB x` = `eA x` ^ `eB y`;
                               `eA x` = tmp1 ^ `eA y`;
                             |]
  | r < 32          = [jmacro| tmp1   = `eB x`;
                               `eB x` = ((`eB x` << `r`) | (`eA x` >>> `32-r`)) ^ `eB y`;
                               `eA x` = ((`eA x` << `r`) | (tmp1 >>> `32-r`)) ^ `eA y`;
                             |]
  | otherwise       = [jmacro| tmp1   = `eB x`;
                               `eB x` = ((`eA x` << `r-32`) | (`eB x` >>> `64-r`)) ^ `eB y`;
                               `eA x` = ((tmp1 << `r-32`) | (`eA x` >>> `64-r`)) ^ `eA y`;
                             |]

(.=) :: JInt64 -> JInt64 -> JStat
x .= y = [jmacro| `lvA x` = `eA y`;
                  `lvB x` = `eB y`;
                |]

u32Ptr :: String -> Int -> JInt64
u32Ptr name n
  | byteArray = JInt64Arr (StrI name) n Nothing
  | otherwise = JInt64Arr (StrI name) n (Just $ ValExpr . JVar . StrI $ name ++ "_o")

keyPtr, blkPtr, cryptPtr :: Int -> JInt64
keyPtr   = u32Ptr "keyPtrI3"
blkPtr   = u32Ptr "blkPtrI3"
cryptPtr = u32Ptr "cryptPtrI3"

--------------------------------------------------------------------------------
-- the algorithm

rounds :: JStat
rounds
  | unroll = mconcat $ map round8u [0..nRounds `div` 8-1]
  | otherwise = [jmacro| var rnd;
                         for(rnd=1;rnd<=`2*div nRounds 8`;rnd+=2) {
                            `round8l rnd`;
                          }
                       |]

-- loop variant
round8l :: JExpr -> JStat
round8l rnd = round4a <> i256l rnd 0 <> round4b <> i256l rnd 1

i256l :: JExpr -> Int -> JStat
i256l rnd r = addTo_64    (x 0) (ks' r     rnd) <>
              addTo2_64   (x 1) (ks' (r+1) rnd) (ts' r     rnd) <>
              addTo2_64   (x 2) (ks' (r+2) rnd) (ts' (r+1) rnd) <>
              addTo_64    (x 3) (ks' (r+3) rnd) <>
              addNumTo_64 (x 3) r (Just rnd) <>
              ks' (r+4) rnd .= ks' (r-1) rnd <>
              ts' (r+2) rnd .= ts' (r-1) rnd

-- unrolled variant
round8u :: Int -> JStat
round8u n = round4a <> i256u (2*n) <> round4b <> i256u (2*n+1)

i256u :: Int -> JStat
i256u r = addTo_64    (x 0) (ks ((r+1)`mod`5)) <>
          addTo2_64   (x 1) (ks ((r+2)`mod`5)) (ts ((r+1)`mod`3)) <>
          addTo2_64   (x 2) (ks ((r+3)`mod`5)) (ts ((r+2)`mod`3)) <>
          addTo2_64_c (x 3) (ks ((r+4)`mod`5)) (r+1)

-- common to both loop and unrolled
r256 :: Int -> Int -> Int -> Int -> Int -> JStat
r256 r p0 p1 p2 p3 = addTo_64    (x p0) (x p1) <>
                     rotL_64_xor (x p1) rot1 (x p0) <>
                     addTo_64    (x p2) (x p3) <>
                     rotL_64_xor (x p3) rot2 (x p2)
  where
    (rot1, rot2) = threefish_rotation !! (r-1)


round4a, round4b :: JStat
round4a = r256 1 0 1 2 3 <>
          r256 2 0 3 2 1 <>
          r256 3 0 1 2 3 <>
          r256 4 0 3 2 1
round4b = r256 5 0 1 2 3 <>
          r256 6 0 3 2 1 <>
          r256 7 0 1 2 3 <>
          r256 8 0 3 2 1

processBlock
  | byteArray =
      [jmacro|
         function !h$Threefish_256_Process_Block(keyPtr_d, keyPtr_o_zero, blkPtr_d, blkPtr_o_zero, cryptPtr_d, cryptPtr_o_zero, w32out) {
           var !keyPtrI3   = keyPtr_d.i3;
           var !blkPtrI3   = blkPtr_d.i3;
           var !cryptPtrI3 = cryptPtr_d.i3;
           `body w32out`;
         }
      |]
  | otherwise =
      [jmacro|
         function !h$Threefish_256_Process_Block(keyPtr_d, keyPtr_o, blkPtr_d, blkPtr_o, cryptPtr_d, cryptPtr_o, w32out) {
          if(keyPtr_o & 3 || blkPtr_o & 3 || cryptPtr_o & 3)
            throw new Error("h$Threefish_256_Process_Block: unaligned pointer");
           var !keyPtrI3   = keyPtr_d.i3,   !keyPtrI3_o   = keyPtr_o   >> 2;
           var !blkPtrI3   = blkPtr_d.i3,   !blkPtrI3_o   = blkPtr_o   >> 2;
           var !cryptPtrI3 = cryptPtr_d.i3, !cryptPtrI3_o = cryptPtr_o >> 2;
           `body w32out`;
         }
      |]
  where
    --  todo: implement word swapping for w32out
    body w32out = [jmacro|
  `add64Decl`;
  // context vars
  var !x0a,!x0b,!x1a,!x1b,!x2a,!x2b,!x3a,!x3b;
  var !tmp1;
  `declWLocal`;
  `declKwLocal`;

  `ks 0 .= keyPtr 0`;
  `ks 1 .= keyPtr 1`;
  `ks 2 .= keyPtr 2`;
  `ks 3 .= keyPtr 3`;

  // ks[4] = ks[0] ^ ks[1] ^ ks[2] ^ ks[3] ^ SKEIN_KS_PARITY;
  `lvA (ks 4)` = `eA (ks 0)` ^ `eA (ks 1)` ^ `eA (ks 2)` ^ `eA (ks 3)` ^ `skein_ks_parity_a`;
  `lvB (ks 4)` = `eB (ks 0)` ^ `eB (ks 1)` ^ `eB (ks 2)` ^ `eB (ks 3)` ^ `skein_ks_parity_b`;

  `ts 0 .= val 0`;
  `ts 1 .= val 0`;
  `ts 2 .= val 0`;

  `w 0 .= blkPtr 0`;
  `w 1 .= blkPtr 1`;
  `w 2 .= blkPtr 2`;
  `w 3 .= blkPtr 3`;

  `add_64  (x 0) (w 0) (ks 0)`;
  `add3_64 (x 1) (w 1) (ks 1) (ts 0)`;
  `add3_64 (x 2) (w 2) (ks 2) (ts 1)`;
  `add_64  (x 3) (w 3) (ks 3)`;

  `rounds`;

  `cryptPtr 0 .= x 0`;
  `cryptPtr 1 .= x 1`;
  `cryptPtr 2 .= x 2`;
  `cryptPtr 3 .= x 3`;

|]
main = do
  withFile "threefish_block.js" WriteMode $ \h -> do
    hPutStrLn h "// generated by generate-threefish-block"
    displayIO h . renderPretty 0.8 120 . renderJs $
      declWGlobal <> declKwGlobal <> processBlock <>
      [jmacro|
       if(typeof exports !== 'undefined')
         exports.h$Threefish_256_Process_Block = h$Threefish_256_Process_Block;
      |]
  callProcess "npx" [ "google-closure-compiler"
                    , "--js=threefish_block.js"
                    , "--js_output_file=threefish_block.min.js"
                    ]
    `E.onException`
       hPutStrLn stderr "exception not running minifier. perhaps you need to install a JVM or npx by running:\nnpm install -g npx"

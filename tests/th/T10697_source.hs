{-# LANGUAGE StrictData, TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import T10697_sourceUtil

$([d|data A1 = A1                   Int {- No unpackedness, no strictness -}|])
$([d|data A2 = A2                  !Int {- No unpackedness, strict        -}|])
$([d|data A3 = A3                  ~Int {- No unpackedness, lazy          -}|])
$([d|data A4 = A4 {-# NOUNPACK #-}  Int {- NOUNPACK, no strictness        -}|])
$([d|data A5 = A5 {-# NOUNPACK #-} !Int {- NOUNPACK, strict               -}|])
$([d|data A6 = A6 {-# NOUNPACK #-} ~Int {- NOUNPACK, lazy                 -}|])
$([d|data A7 = A7 {-#   UNPACK #-}  Int {- UNPACK, no strictness          -}|])
$([d|data A8 = A8 {-#   UNPACK #-} !Int {- UNPACK, strict                 -}|])
$([d|data A9 = A9 {-#   UNPACK #-} ~Int {- UNPACK, lazy                   -}|])

$(do b1 <- newName "B1"
     b2 <- newName "B2"
     b3 <- newName "B3"
     b4 <- newName "B4"
     b5 <- newName "B5"
     b6 <- newName "B6"
     b7 <- newName "B7"
     b8 <- newName "B8"
     b9 <- newName "B9"
     c1 <- newName "C1"
     c2 <- newName "C2"
     c3 <- newName "C3"
     c4 <- newName "C4"
     c5 <- newName "C5"
     c6 <- newName "C6"
     c7 <- newName "C7"
     c8 <- newName "C8"
     c9 <- newName "C9"

     d1 <- makeSimpleDatatype b1 c1 noSourceUnpackedness noSourceStrictness
     d2 <- makeSimpleDatatype b2 c2 noSourceUnpackedness sourceStrict
     d3 <- makeSimpleDatatype b3 c3 noSourceUnpackedness sourceLazy
     d4 <- makeSimpleDatatype b4 c4 sourceNoUnpack       noSourceStrictness
     d5 <- makeSimpleDatatype b5 c5 sourceNoUnpack       sourceStrict
     d6 <- makeSimpleDatatype b6 c6 sourceNoUnpack       sourceLazy
     d7 <- makeSimpleDatatype b7 c7 sourceUnpack         noSourceStrictness
     d8 <- makeSimpleDatatype b8 c8 sourceUnpack         sourceStrict
     d9 <- makeSimpleDatatype b9 c9 sourceUnpack         sourceLazy
     return [d1, d2, d3, d4, d5, d6, d7, d8, d9])

main :: IO ()
main = mapM_ print [ $(checkBang ''E1 noSourceUnpackedness noSourceStrictness)
                   , $(checkBang ''E2 noSourceUnpackedness sourceStrict)
                   , $(checkBang ''E3 noSourceUnpackedness sourceLazy)
                   , $(checkBang ''E4 sourceNoUnpack       noSourceStrictness)
                   , $(checkBang ''E5 sourceNoUnpack       sourceStrict)
                   , $(checkBang ''E6 sourceNoUnpack       sourceLazy)
                   , $(checkBang ''E7 sourceUnpack         noSourceStrictness)
                   , $(checkBang ''E8 sourceUnpack         sourceStrict)
                   , $(checkBang ''E9 sourceUnpack         sourceLazy)
                   ]

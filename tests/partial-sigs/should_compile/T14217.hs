{- # LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module T14217 where

data Foo  a1  a2  a3  a4  a5  a6  a7  a8  a9 a10
         a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
         a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
         a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
         a41 a42 a43 a44 a45 a46 a47 a48 a49 a50
         a51 a52 a53 a54 a55 a56 a57 a58 a59 a60
         a61 a62 a63
 = MkFoo

eq :: ( Eq a1,  Eq a2 , Eq a3 , Eq a4 , Eq a5 , Eq a6 , Eq a7 , Eq a8 , Eq a9, Eq a10
      , Eq a11, Eq a12, Eq a13, Eq a14, Eq a15, Eq a16, Eq a17, Eq a18, Eq a19, Eq a20
      , Eq a21, Eq a22, Eq a23, Eq a24, Eq a25, Eq a26, Eq a27, Eq a28, Eq a29, Eq a30
      , Eq a31, Eq a32, Eq a33, Eq a34, Eq a35, Eq a36, Eq a37, Eq a38, Eq a39, Eq a40
      , Eq a41, Eq a42, Eq a43, Eq a44, Eq a45, Eq a46, Eq a47, Eq a48, Eq a49, Eq a50
      , Eq a51, Eq a52, Eq a53, Eq a54, Eq a55, Eq a56, Eq a57, Eq a58, Eq a59, Eq a60
      , Eq a61, Eq a62, Eq a63)
  => Foo a1  a2  a3  a4  a5  a6  a7  a8  a9 a10
         a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
         a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
         a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
         a41 a42 a43 a44 a45 a46 a47 a48 a49 a50
         a51 a52 a53 a54 a55 a56 a57 a58 a59 a60
         a61 a62 a63
  -> Bool
eq = error "urk"

eqFoo :: (_)
      => Foo  a1  a2  a3  a4  a5  a6  a7  a8  a9 a10
             a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
             a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
             a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
             a41 a42 a43 a44 a45 a46 a47 a48 a49 a50
             a51 a52 a53 a54 a55 a56 a57 a58 a59 a60
             a61 a62 a63
      -> Bool
eqFoo x = eq x

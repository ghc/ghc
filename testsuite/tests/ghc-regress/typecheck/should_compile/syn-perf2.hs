-- Another type-synonym performance test

module ShouldCompile where

type S = Maybe
type S2 n = S (S n)
type S4 n = S2 (S2 n)
type S8 n = S4 (S4 n)
type S16 n = S8 (S8 n)
type S32 n = S16 (S16 n)

type N64 n = S32 (S32 n)

type N64' =
   S ( S ( S ( S ( S ( S ( S ( S (
   S ( S ( S ( S ( S ( S ( S ( S (
   S ( S ( S ( S ( S ( S ( S ( S (
   S ( S ( S ( S ( S ( S ( S ( S (
   S ( S ( S ( S ( S ( S ( S ( S (
   S ( S ( S ( S ( S ( S ( S ( S (
   S ( S ( S ( S ( S ( S ( S ( S (
   S ( S ( S ( S ( S ( S ( S ( S (
   Int
   ))))))))
   ))))))))
   ))))))))
   ))))))))
   ))))))))
   ))))))))
   ))))))))
   ))))))))

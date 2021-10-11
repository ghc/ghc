module DumpSemis where

-- Make sure we get all the semicolons in statements
;;;;  ;;
import Data.List
; ; ;
import Data.Kind
   ; ;;
foo :: IO ()
foo = do
  do { ;;;; a }
  a
; ;;
bar :: IO ()
bar = do
  { ;  ;
    a ;;
    b
  }
 ; ;;  ;
baz :: IO ()
baz = do { ;; s ; s ; ; s ;; }
;
a = undefined
b = undefined
s = undefined
;
class LiftingMonad2  ((trans :: Type)) where
  proof :: trans -> Int
;
f :: ((Eq a, Ord a)) => a -> a
f x = x
;

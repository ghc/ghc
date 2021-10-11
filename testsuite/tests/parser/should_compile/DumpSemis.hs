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
     ;;x=let{;;;;;y=2;;z=3;;;;}in y;
;
fot x =
  case x of
   { ;;; -- leading
     0 -> 'a';  -- case 0
     1 -> 'b'   -- case 1
   ; 2 -> 'c' ; -- case 2
   ; 3 -> 'd'   -- case 3
          ;;;   -- case 4
   }
;

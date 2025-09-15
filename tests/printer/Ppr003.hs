module Ppr003 where

foo x =
  case x of
   { ;;; -- leading
     0 -> 'a';  -- case 0
     1 -> 'b'   -- case 1
   ; 2 -> 'c' ; -- case 2
   ; 3 -> 'd'   -- case 3
          ;;;   -- case 4
   }

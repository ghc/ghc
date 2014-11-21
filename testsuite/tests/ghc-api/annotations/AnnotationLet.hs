module AnnotationLet (foo) where

{
import qualified Data.List as DL
;
foo = let
        a 0 = 1
        a _ = 2
        b = 2
      in a b

}

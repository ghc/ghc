{-# OPTIONS_GHC -fglasgow-exts -O -ddump-simpl #-}
module Roman where

foo :: Int -> Maybe Int -> Int
foo 0 (Just n) = n
foo 0 Nothing  = 1
foo n p = let f = foo (n-1+n-1+n-1+n-1+n-1+n-1+n-1+n-1)
           in
           case p of { Just m -> f (Just m); Nothing -> f Nothing }


{-  At one time this oddly produced;

     foo = \n p ->
          case n of ds {
            __DEFAULT ->
              let
                x = ds -# 1 +# ds -# 1 +# ds -# 1 +# ds -# 1 +# ds -# 1
              in
              case p of {
                Nothing ->
                  foo (x +# ds -# 1 +# ds -# 1 +# ds -# 1) Nothing
                ;
                Just m ->
                  foo (x +# ds -# 1 +# ds -# 1 +# ds -# 1) (Just m)
              };
            0 ->
              case p of {
                Nothing -> lvl_sbC; Just n -> n
              }
          }

But it shouldn't; and doesn't now.

-}

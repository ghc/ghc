{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module T10438 where

foo f = g
  where g r = x
          where x :: _
                x = r

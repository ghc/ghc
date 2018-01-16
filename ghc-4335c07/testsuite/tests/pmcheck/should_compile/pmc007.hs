{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module PMC007 where

-- overloaded
f "ab" = ()
f "ac" = ()

-- non-overloaded
g :: String -> ()
g "ab" = ()
g "ac" = ()

-- non-overloaded due to type inference
h :: String -> ()
h s = let s' = s
      in  case s' of
            "ab" -> ()
            "ac" -> ()

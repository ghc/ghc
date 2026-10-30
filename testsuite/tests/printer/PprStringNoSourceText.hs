{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.String

main :: IO ()
main = pure ()

-- This should exercise `pprHsString` in the incomplete-patterns warning
x :: (IsString s, Eq s) => s -> ()
x "line 1\nline 2\nline 3" = ()

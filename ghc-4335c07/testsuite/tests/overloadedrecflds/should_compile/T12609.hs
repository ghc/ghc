{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror -Wunused-top-binds #-}
module Main (main, T(MkT)) where

data T = MkT { _x :: Int }

main :: IO ()
main = return ()

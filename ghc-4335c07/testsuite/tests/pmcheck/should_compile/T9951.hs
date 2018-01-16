{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module T9951 where

f :: [a] -> ()
f x = case x of
  [] -> ()
  (_:_) -> ()


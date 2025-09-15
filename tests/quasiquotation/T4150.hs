{-# LANGUAGE CPP, QuasiQuotes #-}

module Main (main) where

import T4150A

myHtmlsTemplate = [multiLineStr|
#include "T4150template.txt"
|]

somethingElse :: NoSuchType
somethingElse = undefined

main :: IO ()
main = print myHtmlsTemplate


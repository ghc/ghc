{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprefer-byte-code #-}
module FatTH1 where

import FatQuote1

top = $(a)

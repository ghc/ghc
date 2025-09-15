{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC #-}
module FatTH2 where

import FatQuote2

top = $(a)

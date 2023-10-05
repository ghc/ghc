{-# LANGUAGE TemplateHaskell #-}
module FatTH where

import FatQuote

top = $(a)

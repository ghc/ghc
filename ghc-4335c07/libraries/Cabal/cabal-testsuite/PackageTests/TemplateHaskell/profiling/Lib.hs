{-# LANGUAGE TemplateHaskell #-}
module Lib where

import TH

val = $(splice)

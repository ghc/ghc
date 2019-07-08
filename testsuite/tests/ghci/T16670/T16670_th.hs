{-# LANGUAGE TemplateHaskell #-}

module T16670_th where

import TH

x = $(th)

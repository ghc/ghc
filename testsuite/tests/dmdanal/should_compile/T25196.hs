{-# LANGUAGE TemplateHaskell #-}

module T25196 where

import T25196_aux

bar = $(gen 10000)

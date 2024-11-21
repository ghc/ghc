{-# LANGUAGE TemplateHaskell #-}

module T25510B where

import T25510A

b = $(a)

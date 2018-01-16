{-# LANGUAGE TemplateHaskell #-}

module Main where

import T7064a

$(decls)

main = hsToTh

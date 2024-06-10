{-# LANGUAGE TemplateHaskell, PartialTypeSignatures #-}
module T24769c where

import T24769c_aux
h = $$g

{-# LANGUAGE TemplateHaskell #-}

module T25155 where

import Language.Haskell.TH (runIO)
import T25155_TH

$(runIO (foobar 7) >> pure [])

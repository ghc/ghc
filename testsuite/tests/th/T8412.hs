{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

type T = $(return $ LitT $ NumTyLit (-1))

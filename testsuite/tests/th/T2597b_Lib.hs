{-# LANGUAGE TemplateHaskell #-}
-- Library module for T2597b

module T2597b_Lib where
import Language.Haskell.TH


mkBug2 :: ExpQ
mkBug2 = return $ DoE []

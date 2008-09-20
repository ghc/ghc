{-# LANGUAGE TemplateHaskell #-}
-- Library module for T2597a

module T2597a_Lib where
import Language.Haskell.TH


mkBug :: ExpQ
mkBug = return $ CompE [BindS (VarP $ mkName "p") (ListE []), NoBindS
			 (VarE $ mkName "p")]



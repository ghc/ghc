-- The public face of Template Haskell

module Language.Haskell.TH(
	-- The monad and its operations
	Q, runQ, 
	report,		-- :: Bool -> String -> Q ()
	recover, 	-- :: Q a -> Q a -> Q a
	reify, 		-- :: Name -> Q Decl
	currentModule, 	-- :: Q String
	runIO, 		-- :: IO a -> Q a

	-- Names
	Name, 
	mkName,  	-- :: String -> Name
	newName, 	-- :: String -> Q Name
	nameBase,	-- :: Name -> String
	
	-- The algebraic data types
	Dec(..), Exp(..), Con(..), Type(..), Cxt, Match(..), 
	Clause(..), Body(..), Stmt(..), Range(..),
	Lit(..), Pat(..), FieldExp, FieldPat, 
	Strict(..), Foreign(..), Callconv(..), Safety(..),
	Info(..), 
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,

	-- Library functions
	module Language.Haskell.TH.Lib,
    -- Pretty-printer
	module Language.Haskell.TH.Ppr,
	
   ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ActionsAndObservations
  ( Stmt, Expr
  , stmt, expr
  )
where

-- used to represent computations (in translation testing)
--
--   * An action changes the state of a machine.
--   * An expression inspects the state of a machine and observes a value.


import GHC.Cmm
import GHC.Cmm.Ppr ()
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Block
import GHC.Platform
import GHC.Utils.Outputable


data Stmt = Stmt { s_label :: Label
                 , s_rendering :: String
                 }

data Expr = Expr { e_label :: Label
                 , e_rendering :: String
                 }

stmt :: Label -> Block CmmNode O O -> Stmt
stmt lbl body = Stmt lbl (showSDocUnsafe $ pdoc genericPlatform $ body)

expr :: Label -> CmmExpr -> Expr
expr lbl e = Expr lbl (showSDocUnsafe $ pdoc genericPlatform $ e)

instance Eq Stmt where
  s == s' = s_label s == s_label s' || s_rendering s == s_rendering s'

instance Eq Expr where
  e == e' = e_label e == e_label e' || e_rendering e == e_rendering e'

instance Show Stmt where
  show = showSDocUnsafe . ppr . s_label

instance Show Expr where
  show = showSDocUnsafe . ppr . e_label

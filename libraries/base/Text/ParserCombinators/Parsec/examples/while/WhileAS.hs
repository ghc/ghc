module WhileAS where

type  VarIdent = String
type  Label = Int
-- type  Selector = String
  
type Prog = Stat
-- type Prog = Prog [Dec] [Stat]

-- Contains name, a list of input vars, output var, body respectively and of course
-- the two labels ln and lx
data Dec = Proc [VarIdent] VarIdent VarIdent Label Stat Label

data AExp 
  = Var VarIdent 
  | IntLit Integer
  | AOp String AExp AExp
-- | Var  VarIdent (Maybe Selector)
-- | Nil
  | Dummy
  deriving (Eq, Show)
  
data BExp 
  = BUnOp String BExp
  | BoolLit Bool
  | BOp String BExp BExp
  | RelOp String AExp AExp
-- | POp VarIdent (Maybe Selector)
  deriving (Eq, Show)

data Stat
  = Assign VarIdent AExp Label
  | Skip Label
  | Seq [Stat]
  | If BExp Label Stat Stat
  | While BExp Label Stat
-- | Call VarIdent [AExp] VarIdent Label Label
-- | Malloc VarIdent (Maybe Selector) Label
  deriving (Show, Eq)

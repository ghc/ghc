bstract syntax for Java subset that is the target of Mondrian.
The syntax has been taken from "The Java Language Specification".

(c) Erik Meijer & Arjan van IJzendoorn

November 1999

\begin{code}
module Java where

\end{code}

%************************************************************************
%*									*
\subsection{Java type declararations}
%*									*
%************************************************************************

\begin{code}
data CompilationUnit
  = Package Name [Decl]
    deriving (Show)
    
data Decl
 = Import [Name]
 | Field [Modifier] Type Name (Maybe Expr)   
 | Constructor [Modifier] Name [Parameter] [Statement]
				-- Add Throws (list of Names)
				-- to Method
 | Method [Modifier] Type Name [Parameter] [Statement]
 | Comment [String]
 | Interface [Modifier] Name [Name] [Decl]
 | Class [Modifier] Name [Name] [Name] [Decl]
   deriving (Show)
   
data Parameter
 = Parameter [Modifier] Type Name
   deriving (Show)
   
data Statement
  = Skip
  | Return Expr
  | Block [Statement]
  | ExprStatement Expr
  | Declaration Decl -- variable = inner Field, Class = innerclass
  | IfThenElse [(Expr,Statement)] (Maybe Statement)
  | Switch Expr [(Expr, [Statement])] (Maybe [Statement])
    deriving (Show)

data Expr 
  = Var Name
  | Literal Lit
  | Cast Type Expr
  | Access Expr Name
  | Assign Expr Expr
  | InstanceOf Expr Type
  | Call Expr Name [Expr]
  | Op Expr String Expr
  | New Type [Expr] (Maybe [Decl]) -- anonymous innerclass
  | NewArray Type [Expr]
    deriving (Show)
    
data Modifier 
  = Public | Protected | Private
  | Static
  | Abstract | Final | Native | Synchronized | Transient | Volatile
  deriving (Show, Eq, Ord)
  
data Type 
  = PrimType String
  | ArrayType Type
  | Type [Name]
    deriving (Show)

-- If you want qualified names, use Access <expr> <name> 
-- Type's are already qualified.
type Name = String

data Lit
  = IntLit Int		-- Boxed
  | UIntLit Int		-- Unboxed
  | CharLit Char	-- Boxed
  | UCharLit Char	-- Unboxed
  | StringLit String
  deriving Show

data OType 
  = ObjectType		-- Object *
  | UnboxedIntType	-- int
  | UnboxedCharType	-- char

data OVar = OVar Name OType
			-- Object x.y

addModifier :: Modifier -> Decl -> Decl
addModifier = \m -> \d ->
 case d of
   { Import n -> Import n
   ; Field ms t n e -> Field (m:ms) t n e  
   ; Constructor ms n as ss -> Constructor (m:ms) n as ss
   ; Method ms t n as ss -> Method (m:ms) t n as ss
   ; Comment ss -> Comment ss
   ; Interface ms n xs ds -> Interface (m:ms) n xs ds
   ; Class ms n xs is ds -> Class (m:ms) n xs is ds
   }
   
areSimple :: [Expr] -> Bool
areSimple = \es -> all isSimple es

isSimple :: Expr -> Bool
isSimple = \e ->
  case e of
   { Cast t e -> isSimple e
   ; Access e n -> isSimple e
   ; Assign l r -> isSimple l && isSimple r
   ; InstanceOf e t -> isSimple e
   ; Call e n es -> isSimple e && areSimple es
   ; Op e1 o e2 -> False
   ; New n es Nothing -> areSimple es
   ; New n es (Just ds) -> False
   ; otherwise -> True
   }
\end{code}

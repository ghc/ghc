Anbstract syntax for Java subset that is the target of Mondrian.
The syntax has been taken from "The Java Language Specification".

(c) Erik Meijer & Arjan van IJzendoorn

November 1999

Major reworking to be usable for the intermeduate (GOO) language
for the backend of GHC and to target languauges like Java sucessfully.
-- Andy Gill

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
  = Package PackageName [Decl]
    deriving (Show)
    
data Decl
 = Import PackageName
 | Field [Modifier] Name (Maybe Expr)
 | Constructor [Modifier] TypeName [Parameter] [Statement]
 | Method [Modifier] Name [Parameter] [Exception] [Statement]
 | Comment [String]
 | Interface [Modifier] TypeName [TypeName] [Decl]
 | Class [Modifier] TypeName [TypeName] [TypeName] [Decl]
   deriving (Show)

data Parameter
 = Parameter [Modifier] Name
   deriving (Show)
   
data Statement
  = Skip
  | Return Expr		-- This always comes last in a list
			-- of statements, and it is understood
			-- you might change this to something
			-- else (like a variable assignment)
			-- if this is not top level statements.
  | Block [Statement]
  | ExprStatement Expr	-- You are never interested in the result
			-- of an ExprStatement
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
  | Raise TypeName [Expr]
  | New Type [Expr] (Maybe [Decl]) -- anonymous innerclass
    deriving (Show)
    
data Modifier 
  = Public | Protected | Private
  | Static
  | Abstract | Final | Native | Synchronized | Transient | Volatile
  deriving (Show, Eq, Ord)

-- A type is used to refer in general to the shape of things,
-- or a specific class. Never use a name to refer to a class,
-- always use a type.

data Type 
  = PrimType  PrimType
  | ArrayType Type
  | Type      TypeName
    deriving (Show, Eq)

data PrimType 
  = PrimInt 
  | PrimBoolean
  | PrimChar
  | PrimLong
  | PrimFloat
  | PrimDouble
  | PrimByte
  | PrimVoid
    deriving (Show, Eq)

type PackageName = String	-- A package name
				-- like "java.awt.Button"

type Exception   = TypeName	-- A class name that must be an exception.

type TypeName    = String	-- a fully qualified type name
				-- like "java.lang.Object".
				-- has type "Type <the name>"

data Name        = Name String Type
	deriving Show		-- A class name or method etc, 
				-- at defintion time,
				-- this generally not a qualified name.

				-- The type is shape of the box require
				-- to store an access to this thing.
				-- So variables might be Int or Object.

				--  ** method calls store the returned
				--  ** type, not a complete arg x result type.
				--
				-- Thinking:
				-- ... foo1.foo2(...).foo3 ...
				-- here you want to know the *result*
				-- after calling foo1, then foo2,
				-- then foo3.

instance Eq Name where
   (Name nm _) == (Name nm' _) = nm == nm'


instance Ord Name where
   (Name nm _) `compare` (Name nm' _) = nm `compare` nm'


data Lit
  = IntLit Integer	-- unboxed
  | CharLit Int 	-- unboxed
  | StringLit String	-- java string
  deriving Show

addModifier :: Modifier -> Decl -> Decl
addModifier = \m -> \d ->
 case d of
   { Import n -> Import n
   ; Field ms n e -> Field (m:ms) n e  
   ; Constructor ms n as ss -> Constructor (m:ms) n as ss
   ; Method ms n as ts ss -> Method (m:ms) n as ts ss
   ; Comment ss -> Comment ss
   ; Interface ms n xs ds -> Interface (m:ms) n xs ds
   ; Class ms n xs is ds -> Class (m:ms) n xs is ds
   }

changeNameType :: Type -> Name -> Name
changeNameType ty (Name n _) = Name n ty
   
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

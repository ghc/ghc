
module CmdSyntax ( Var, MacroName, MacroDef(..),
                   TopDef(..), Stmt(..), Expr(..),
                   Op(..), Result(..),
                   panic, officialMsg, my_system
--                   isExpect, isWhen, isSkipWhen, isCompileOnly, 
--                   isAssign 
                 )
where

import IO		( stdout, hPutStrLn )
import System		( system )

---------------------------------------------------------------------
-- misc
panic str
   = error ("\nruntests: the `impossible' happened:\n\t" ++ str ++ "\n")

officialMsg str
   = hPutStrLn stdout ("runtests: " ++ str)

my_system s
   = do putStrLn s
        exit_code <- system s
        --putStrLn (show exit_code)
        return exit_code

---------------------------------------------------------------------
-- command abs syntax

------------------
type TestName  = String
type Var       = String
type MacroName = String
data MacroDef  = MacroDef [Var] [Stmt]
                 deriving Show

------------------

data Expr
   = EOp        Op Expr Expr
   | EVar       Var
   | EString    String
   | EBool	Bool
   | EContents  Expr
   | EExists    Expr
   | EMacro     MacroName [Expr]
   | ECond      Expr Expr (Maybe Expr)
   | EOtherwise
   | EHasValue  Expr
   | EFFail     Expr
     deriving Show

data Stmt
   = SAssign    Var Expr
   | SPrint     Expr
   | SCond      Expr [Stmt] (Maybe [Stmt])
   | SRun       Var Expr
   | SReturn    Expr
   | SMacro     MacroName [Expr]
   | SFFail     Expr
   | SSkip      Expr
   | SResult    Result Expr
   | SExpect    Result
     deriving Show

data TopDef
   = TStmt      Stmt
   | TInclude   Expr
   | TMacroDef  MacroName MacroDef
   | TTest      TestName [Stmt]
     deriving Show

data Op
   = OpAnd | OpOr | OpAppend | OpEq | OpNEq | OpContains | OpLacks
     deriving (Eq, Show)
{-
isExpect (Expect _)     = True ; isExpect other = False
isWhen   (When _ _)     = True ; isWhen other = False
isSkipWhen (SkipWhen _) = True ; isSkipWhen other = False
isCompileOnly CompileOnly = True ; isCompileOnly other = False
isAssign (Assign _ _) = True; isAssign other = False
-}

data Result
   = Pass 		-- test passed
   | Fail 		-- test failed
   | Unknown		-- test might have run, but outcome undetermined
   | Skipped		-- skip-when clause indicated this test to be skipped
     deriving (Eq, Show)


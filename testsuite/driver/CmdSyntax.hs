
module CmdSyntax ( Var, MacroName, TestName, MacroDef(..),
                   TopDef(..), Stmt(..), Expr(..), freeVars,
                   Op(..), Result(..), TestID(..),
                   panic, officialMsg, my_system,
                   isJust, isNothing, unJust,
                   isTInclude, isTTest, isTMacroDef, isTAssign,
                   isLeft, isRight, unLeft, unRight
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

isJust (Just _) = True
isJust Nothing  = False
isNothing = not . isJust

unJust (Just x) = x

isLeft (Left _)  = True
isLeft (Right _) = False
isRight = not . isLeft

unLeft  (Left x)  = x
unRight (Right x) = x


---------------------------------------------------------------------
-- command abs syntax

------------------
type TestName  = String
type Var       = String
type MacroName = String
data MacroDef  = MacroDef [Var] [Stmt]
                 deriving Show

data TestID = TestID FilePath{-for the .T file-} 
                     TestName{-name within the .T file-}
              deriving Eq

instance Show TestID where
   show (TestID tfilepath tname) = tfilepath ++ " " ++ tname

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
   | EDefined   Var
   | EFFail     Expr
     deriving Show

freeVars :: Expr -> [Var]
freeVars (EOp op l r)  = freeVars l ++ freeVars r
freeVars (EVar v)      = [v]
freeVars (EString _)   = []
freeVars (EBool _)     = []
freeVars (EContents e) = freeVars e
freeVars (EExists e)   = freeVars e
freeVars (EMacro _ es) = concatMap freeVars es
freeVars (EDefined v)  = []	-- we don't actually use v here
freeVars (EFFail e)    = freeVars e

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
   = TInclude   Expr
   | TMacroDef  MacroName MacroDef
   | TTest      TestName [Stmt]
   | TAssign    Var Expr
     deriving Show

data Op
   = OpAnd | OpOr | OpAppend | OpEq | OpNEq | OpContains | OpLacks
     deriving (Eq, Show)

isTInclude (TInclude _) = True
isTInclude other        = False

isTTest (TTest _ _) = True
isTTest other       = False

isTMacroDef (TMacroDef _ _) = True
isTMacroDef other           = False

isTAssign (TAssign _ _) = True
isTAssign other         = False

data Result
   = Pass 		-- test passed
   | Fail 		-- test failed
   | Unknown		-- test might have run, but outcome undetermined
   | Skipped		-- skip-when clause indicated this test to be skipped
     deriving (Eq, Show)


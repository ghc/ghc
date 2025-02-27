module GHC.Parser.PreProcess.State where

import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import GHC.Base
import GHC.Data.StringBuffer
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..))
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.SrcLoc
import GHC.Parser.PreProcess.ParserM (Token (..))

import GHC.Prelude

-- ---------------------------------------------------------------------

type PP = P PpState

data CppState
    = CppIgnoring
    | CppNormal
    deriving (Show)

-- ---------------------------------------------------------------------

initPpState :: PpState
initPpState =
    PpState
        { pp_includes = Map.empty
        , pp_include_stack = []
        , pp_continuation = []
        , pp_defines = Map.empty
        , pp_scope = (PpScope True) :| []
        }

data PpState = PpState
    { pp_includes :: !(Map String StringBuffer)
    , pp_include_stack :: ![Lexer.AlexInput]
    , pp_continuation :: ![Located Lexer.Token]
    , pp_defines :: !MacroDefines
    , pp_scope :: !(NonEmpty PpScope)
    }
    deriving (Show)

data PpScope = PpScope
    { pp_accepting :: !Bool
    }
    deriving (Show)

-- ---------------------------------------------------------------------

data CppDirective
    = CppInclude String
    | -- | name, optional args, replacement
      CppDefine String (Maybe [String]) MacroDef
    | CppIfdef String
    | CppIfndef String
    | CppIf String
    | CppElse
    | CppEndif
    | CppDumpState
    deriving (Show, Eq)

-- ---------------------------------------------------------------------

type MacroArgs = [String]
data MacroName = MacroName String (Maybe MacroArgs)
    deriving (Show, Eq, Ord)
type MacroDef = [Token]

-- Indexed by name, and then arity
type MacroDefines = Map String (Map (Maybe Int) ((Maybe MacroArgs), MacroDef))

type Input = String
type Output = CppDirective

-- ---------------------------------------------------------------------
-- Expression language
-- NOTE: need to take care of macro expansion while parsing. Or perhaps before?

data Expr
    = Parens Expr
    | Var String
    | IntVal Int
    | Plus Expr Expr
    | Minus Expr Expr
    | Times Expr Expr
    | Logic LogicOp Expr Expr
    | Comp CompOp Expr Expr
    deriving (Show, Eq)

data LogicOp
    = LogicalOr
    | LogicalAnd
    deriving (Show, Eq)

data CompOp
    = CmpEqual
    | CmpNotEqual
    | CmpGt
    | CmpGtE
    | CmpLt
    | CmpLtE
    deriving (Show, Eq)

-- ---------------------------------------------------------------------
-- Preprocessor state functions

getCppState :: PP CppState
getCppState = do
    accepting <- getAccepting
    if accepting
        then return CppNormal
        else return CppIgnoring

-- pp_scope stack start -----------------

pushScope :: PpScope -> PP ()
pushScope new =
    P $ \s -> POk s{pp = (pp s){pp_scope = new <| (pp_scope (pp s))}} ()

pushScope' :: PpState -> PpScope -> PpState
pushScope' s new = s{pp_scope = new <| (pp_scope s)}

popScope :: PP ()
popScope =
    P $ \s ->
        let
            new_scope = case pp_scope (pp s) of
                c :| [] -> c :| []
                _ :| (h : t) -> h :| t
         in
            POk s{pp = (pp s){pp_scope = new_scope}} ()

popScope' :: PpState -> PpState
popScope' s =
    let
        new_scope = case pp_scope s of
            c :| [] -> c :| []
            _ :| (h : t) -> h :| t
     in
        s{pp_scope = new_scope}

getScope :: PP PpScope
getScope =
    P $ \s -> POk s (getScope' (pp s))

getScope' :: PpState -> PpScope
getScope' s = NonEmpty.head $ pp_scope s

setScope :: PpScope -> PP ()
setScope scope =
    P $ \s ->
        let
            new_scope = case pp_scope (pp s) of
                _ :| rest -> scope :| rest
         in
            POk s{pp = (pp s){pp_scope = new_scope}} ()

setScope' :: PpState -> PpScope -> PpState
setScope' s scope =
    let
        new_scope = case pp_scope s of
            _ :| rest -> scope :| rest
     in
        s{pp_scope = new_scope}

setAccepting :: Bool -> PP ()
setAccepting on = do
    scope <- getScope
    setScope (scope{pp_accepting = on})

pushAccepting :: Bool -> PP ()
pushAccepting on = pushScope (PpScope on)

pushAccepting' :: PpState -> Bool -> PpState
pushAccepting' s on = pushScope' s (PpScope on)

setAccepting' :: PpState -> Bool -> PpState
setAccepting' s on = setScope' s (scope{pp_accepting = on})
  where
    scope = getScope' s

getAccepting :: PP Bool
getAccepting = P $ \s -> POk s (pp_accepting (NonEmpty.head $ pp_scope (pp s)))

getAccepting' :: PpState -> Bool
getAccepting' s = pp_accepting (NonEmpty.head $ pp_scope s)

ghcCppEnabled :: PP Bool
ghcCppEnabled = P $ \s -> POk s (Lexer.ghcCppEnabled (options s))

addDefine :: MacroName -> MacroDef -> PP ()
addDefine name def = do
    accepting <- getAccepting
    when accepting $ do
        s <- getPpState
        setPpState $ addDefine' s name def

addDefine' :: PpState -> MacroName -> MacroDef -> PpState
addDefine' s name def =
    s{pp_defines = insertMacroDef name def (pp_defines s)}

ppDefine :: MacroName -> MacroDef -> PP ()
ppDefine name val = addDefine name val

ppIsDefined :: MacroName -> PP Bool
ppIsDefined name = do
    s <- getPpState
    return $ ppIsDefined' s name

ppIsDefined' :: PpState -> MacroName -> Bool
ppIsDefined' s (MacroName name _args) =
    isJust $ Map.lookup name (pp_defines s)

ppDefinition' :: PpState -> String -> Maybe (Map (Maybe Int) ((Maybe MacroArgs), MacroDef))
ppDefinition' s name = Map.lookup name (pp_defines s)

getPpState :: PP PpState
getPpState = P $ \s -> POk s (pp s)

setPpState :: PpState -> PP ()
setPpState pp' = P $ \s -> POk s{pp = pp'} ()

-- ---------------------------------------------------------------------

pushContinuation :: Located Lexer.Token -> PP ()
pushContinuation new =
    P $ \s -> POk s{pp = (pp s){pp_continuation = new : pp_continuation (pp s)}} ()

popContinuation :: PP [Located Lexer.Token]
popContinuation =
    P $ \s -> POk s{pp = (pp s){pp_continuation = []}} (pp_continuation (pp s))

-- ---------------------------------------------------------------------
-- Dealing with MacroDefines

arg_arity :: Maybe [t] -> Maybe Int
arg_arity args = case args of
    Nothing -> Nothing
    Just as -> Just (length as)

insertMacroDef :: MacroName -> MacroDef -> MacroDefines -> MacroDefines
insertMacroDef (MacroName name args) def md =
    let arity = arg_arity args
     in case Map.lookup name md of
            Nothing -> Map.insert name (Map.singleton arity (args, def)) md
            Just dm -> Map.insert name (Map.insert arity (args, def) dm) md

-- ---------------------------------------------------------------------

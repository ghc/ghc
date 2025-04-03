module GHC.Parser.PreProcess.State (
    Expr (..),
    CompOp (..),
    LogicOp (..),
    CppDirective (..),
    Input,
    Output,
    PpState (..),
    initPpState,
    PP,
    PpScope (..),
    MacroDefines,
    MacroDef,
    MacroName (..),
    MacroArgs,
    CppState (..),
    arg_arity,

    getPpState, setPpState,
    AcceptingResult(..),
    getAccepting,
    setAccepting,
    pushAccepting,
    popAccepting,
    pushContinuation,
    popContinuation,
    ppDefine,
    ppIsDefined,
    getCppState,
    ghcCppEnabled,
) where

import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import GHC.Base
import GHC.Data.StringBuffer
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..))
import GHC.Parser.Lexer qualified as Lexer
import GHC.Parser.PreProcess.ParserM (Token (..))
import GHC.Types.SrcLoc
import Debug.Trace

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
        , pp_alr_state = Nothing
        }

data PpState = PpState
    { pp_includes :: !(Map String StringBuffer)
    , pp_include_stack :: ![Lexer.AlexInput]
    , pp_continuation :: ![Located Lexer.Token]
    , pp_defines :: !MacroDefines
    , pp_scope :: !(NonEmpty PpScope)
    , pp_alr_state :: Maybe Lexer.PSavedAlrState
    }

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
    | Not Expr
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

data AcceptingResult
    = ArNoChange
    | ArNowAccepting
    | ArNowIgnoring
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

setAccepting :: Bool -> PP AcceptingResult
setAccepting on = do
  current <- getAccepting
  PpScope parent <- parentScope
  setScope (PpScope (parent && on))
  return $ acceptingStateChange current (parent && on)

getAccepting :: PP Bool
getAccepting = P $ \s -> POk s (scopeValue $ pp_scope (pp s))

-- Start a new scope, ensuring it is consistent with the existing one
pushAccepting :: Bool -> PP AcceptingResult
pushAccepting on = do
  current <- getAccepting
  PpScope current_scope <- getScope
  pushScope (PpScope (current_scope && on))
  return $ acceptingStateChange current (current_scope && on)

-- Have we just changed the accepting state?
acceptingStateChange :: Bool -> Bool -> AcceptingResult
acceptingStateChange old new =
  -- let (old, new) = trace ("acceptStateChange:" ++ show (old',new')) (old',new')
  -- in
  case (old, new) of
    (True, False) -> ArNowIgnoring
    (False, True) -> ArNowAccepting
    _             -> ArNoChange

popAccepting :: PP AcceptingResult
popAccepting =
    P $ \s ->
        let
            current = scopeValue $ pp_scope (pp s)
            new_scope = case pp_scope (pp s) of
                c :| [] -> c :| []
                -- c :| [] -> (trace ("popAccepting:keeping old:" ++ show c) c) :| []
                _ :| (h : t) -> h :| t
         in
            POk s{pp = (pp s){pp_scope = new_scope}}
                (acceptingStateChange current (scopeValue new_scope))

scopeValue :: NonEmpty PpScope -> Bool
scopeValue s =  pp_accepting $ NonEmpty.head s

-- Deal with the mechanics of pushing a pre-calculated scope
pushScope :: PpScope -> PP ()
pushScope new =
    P $ \s -> POk s{pp = (pp s){pp_scope = new <| (pp_scope (pp s))}} ()

-- Get the parent scope value, or current if at the top
-- Effectively answers "what is the enclosing scope"
parentScope :: PP PpScope
parentScope =
    P $ \s ->
        let
            new_scope = case pp_scope (pp s) of
                c :| [] -> c -- Perhaps should return enabled instead
                _ :| (h : _t) -> h
         in
            POk s new_scope

-- Get the current scope value
getScope :: PP PpScope
getScope =
    P $ \s -> POk s (NonEmpty.head $ pp_scope (pp s))

-- Replace the current scope value
setScope :: PpScope -> PP ()
setScope scope =
    P $ \s ->
        let
            new_scope = case pp_scope (pp s) of
                _ :| rest -> scope :| rest
         in
            POk s{pp = (pp s){pp_scope = new_scope}} ()

{-
Note [PpScope stack]
~~~~~~~~~~~~~~~~~~~~

The preprocessor does fundamentally one thing: tt tracks for a
given region of code if we are processing it, or ignoring it.

To do that is tracks directives, and when it finds a conditional one,
evaluates it, and decides if the state should be processing or
ignoring.

The complexity comes because these can be nested. so, in

    #if 0
      #if 1
      -- first
      #else
      -- second
      #endif
    #else
    -- only part on
    #endif

The only part processed by the Parser is `-- only part on`, even
though the immediately containing `#if` for `-- first` ostensibly
turns it on.

The PpScope stack tracks the nested scopes, and answers the
fundamental question based on it.

The key point is that for a given location, with an associated scope
stack, it is only enabled if all the scopes are enabled.

When doing an update, if it is already off, in the enclosing scope, it
can only remain off.

-}

-- pp_scope stack end -----------------

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

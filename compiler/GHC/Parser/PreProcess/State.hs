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
    PpGroupState (..),
    MacroDefines,
    MacroDef,
    MacroName (..),
    MacroArgs,
    CppState (..),
    arg_arity,
    getPpState,
    setPpState,
    AcceptingResult (..),
    getAccepting,
    setAccepting,
    pushAccepting,
    popAccepting,
    pushContinuation,
    popContinuation,
    ppDefine,
    ppIsDefined,
    ppUndef,
    getCppState,
    ghcCppEnabled,
    setInLinePragma,
    getInLinePragma,
    addGhcCPPError',
) where

import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import GHC.Base
import GHC.Data.StringBuffer
import GHC.Parser.Errors.Types (PsMessage (PsErrGhcCpp))
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..))
import GHC.Parser.Lexer qualified as Lexer
import GHC.Parser.PreProcess.ParserM (Token (..))
import GHC.Types.SrcLoc
import GHC.Utils.Error

import GHC.Prelude
import GHC.Utils.Outputable (hang, text, (<+>))

-- ---------------------------------------------------------------------

type PP = P PpState

-- ---------------------------------------------------------------------

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
        , pp_scope = (PpScope True PpNoGroup) :| []
        , pp_in_line_pragma = False
        }

data PpState = PpState
    { pp_includes :: !(Map String StringBuffer)
    , pp_include_stack :: ![Lexer.AlexInput]
    , pp_continuation :: ![Located Lexer.Token]
    , pp_defines :: !MacroDefines
    , pp_scope :: !(NonEmpty PpScope)
    , pp_in_line_pragma :: !Bool
    }

data PpScope = PpScope
    { pp_accepting :: !Bool
    , pp_group_state :: !PpGroupState
    }
    deriving (Show)

data PpGroupState
    = PpNoGroup
    | PpInGroupStillInactive
    | PpInGroupHasBeenActive
    deriving (Show)

-- ---------------------------------------------------------------------

data CppDirective
    = CppInclude String
    | -- | name, optional args, replacement
      CppDefine String (Maybe [String]) MacroDef
    | CppUndef String
    | CppIfdef String
    | CppIfndef String
    | CppIf String
    | CppElse
    | CppElIf String
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

-- Start a new scope group, ensuring it is consistent with the
-- existing one
pushAccepting :: Bool -> PP AcceptingResult
pushAccepting on = do
    current <- getAccepting
    current_scope <- getScope
    let scope_on = pp_accepting current_scope
    let group_state =
            if scope_on && on
                then PpInGroupHasBeenActive
                else PpInGroupStillInactive
    pushScope
        ( PpScope
            { pp_accepting = scope_on && on
            , pp_group_state = group_state
            }
        )
    return $ acceptingStateChange current (scope_on && on)

-- Note: this is only ever called in the context of a pp group (i.e.
-- after pushAccepting) from processing #else or #elif
setAccepting :: SrcSpan -> SDoc -> Bool -> PP AcceptingResult
setAccepting loc ctx on = do
    current <- getAccepting
    parent_scope <- parentScope
    let parent_on = pp_accepting parent_scope
    current_scope <- getScope
    let group_state = pp_group_state current_scope
    let possible_accepting = parent_on && on
    (new_group_state, accepting) <-
        case (group_state, possible_accepting) of
            (PpNoGroup, _) -> do
                addGhcCPPError loc (ctx <+> text "without #if")
                return (PpNoGroup, True)
            (PpInGroupStillInactive, True) -> return (PpInGroupHasBeenActive, True)
            (PpInGroupStillInactive, False) -> return (PpInGroupStillInactive, False)
            (PpInGroupHasBeenActive, _) -> return (PpInGroupHasBeenActive, False)

    -- let (new_group_state, accepting)
    --       = trace ("setAccepting:" ++ show ((group_state, possible_accepting),  (new_group_state', accepting'))) (new_group_state', accepting')

    setScope
        ( PpScope
            { pp_accepting = accepting
            , pp_group_state = new_group_state
            }
        )
    return $ acceptingStateChange current accepting

getAccepting :: PP Bool
getAccepting = P $ \s -> POk s (scopeValue $ pp_scope (pp s))

-- Have we just changed the accepting state?
acceptingStateChange :: Bool -> Bool -> AcceptingResult
acceptingStateChange old new =
    -- let (old, new) = trace ("acceptStateChange:" ++ show (old',new')) (old',new')
    -- in
    case (old, new) of
        (True, False) -> ArNowIgnoring
        (False, True) -> ArNowAccepting
        _ -> ArNoChange

-- Exit a scope group
popAccepting :: SrcSpan -> PP AcceptingResult
popAccepting loc = do
    scopes <- getScopes
    new_scope <- case scopes of
        c :| [] -> do
            addGhcCPPError loc (text "#endif without #if")
            return (c :| [])
        _ :| (h : t) -> return (h :| t)
    setScopes new_scope
    let current = scopeValue scopes
    return (acceptingStateChange current (scopeValue new_scope))

scopeValue :: NonEmpty PpScope -> Bool
scopeValue s = pp_accepting $ NonEmpty.head s

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
            POk s (new_scope)

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

getScopes :: PP (NonEmpty PpScope)
getScopes = P $ \s -> POk s (pp_scope (pp s))

setScopes :: (NonEmpty PpScope) -> PP ()
setScopes new_scope =
    P $ \s -> POk s{pp = (pp s){pp_scope = new_scope}} ()

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

removeDefine :: String -> PP ()
removeDefine name = do
    accepting <- getAccepting
    when accepting $ do
        s <- getPpState
        setPpState $ removeDefine' s name

removeDefine' :: PpState -> String -> PpState
removeDefine' s name =
    s{pp_defines = Map.delete name (pp_defines s)}

-- -------------------------------------

ppDefine :: MacroName -> MacroDef -> PP ()
ppDefine name val = addDefine name val

ppUndef :: String -> PP ()
ppUndef name = removeDefine name

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

setInLinePragma :: Bool -> PP ()
setInLinePragma val =
    P $ \s -> POk s{pp = (pp s){pp_in_line_pragma = val}} ()

getInLinePragma :: PP Bool
getInLinePragma =
    P $ \s -> POk s (pp_in_line_pragma (pp s))

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

mkGhcCPPError' :: SrcSpan -> String -> SDoc -> MsgEnvelope PsMessage
mkGhcCPPError' loc title detail =
    mkGhcCPPError
        loc
        ( hang
            (text title)
            2
            detail
        )

addGhcCPPError' :: SrcSpan -> String -> SDoc -> PP ()
addGhcCPPError' loc title detail = Lexer.addError $ mkGhcCPPError' loc title detail

mkGhcCPPError :: SrcSpan -> SDoc -> MsgEnvelope PsMessage
mkGhcCPPError loc err = mkPlainErrorMsgEnvelope loc $ PsErrGhcCpp err

addGhcCPPError :: SrcSpan -> SDoc -> PP ()
addGhcCPPError loc err = Lexer.addError $ mkGhcCPPError loc err

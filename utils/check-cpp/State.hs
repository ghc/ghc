module State where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Maybe
import Data.Map qualified as Map
import GHC.Data.StringBuffer
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), Token (..))
import GHC.Parser.Lexer qualified as Lexer
import GHC.Prelude
import GHC.Types.SrcLoc

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
        , pp_scope = (PpScope Map.empty True) :| []
        }

data PpState = PpState
    { pp_includes :: !(Map String StringBuffer)
    , pp_include_stack :: ![Lexer.AlexInput]
    , pp_continuation :: ![Located Token]
    , pp_scope :: !(NonEmpty PpScope)
    }
    deriving (Show)

data PpScope = PpScope
    { pp_defines :: !(Map MacroName MacroDef)
    , pp_accepting :: !Bool
    }
    deriving (Show)

-- ---------------------------------------------------------------------

data CppDirective
    = CppInclude String
    | CppDefine String String
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
type MacroDef = String

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
pushAccepting on = pushScope (PpScope Map.empty on)

pushAccepting' :: PpState -> Bool -> PpState
pushAccepting' s on = pushScope' s (PpScope Map.empty on)

setAccepting' :: PpState -> Bool -> PpState
setAccepting' s on = setScope' s (scope{pp_accepting = on})
  where
    scope = getScope' s

getAccepting :: PP Bool
getAccepting = P $ \s -> POk s (pp_accepting (NonEmpty.head $ pp_scope (pp s)))

getAccepting' :: PpState -> Bool
getAccepting' s = pp_accepting (NonEmpty.head $ pp_scope s)

addDefine :: MacroName -> MacroDef -> PP ()
addDefine name def = do
    scope <- getScope
    setScope (scope{pp_defines = Map.insert name def (pp_defines scope)})

addDefine' :: PpState -> MacroName -> MacroDef -> PpState
addDefine' s name def = r
  where
    scope = getScope' s
    r = setScope' s (scope{pp_defines = Map.insert name def (pp_defines scope)})

ppDefine :: MacroName -> MacroDef -> PP ()
ppDefine name val = addDefine name val

ppIsDefined :: MacroName -> PP Bool
ppIsDefined name = do
    -- Look up the chain of scopes, until we find one that works, or end
    let
        lookup [] = False
        lookup (h : t) =
            if Map.member name (pp_defines h)
                then True
                else lookup t
    pp <- getPpState
    let scopes = NonEmpty.toList (pp_scope pp)
    return $ lookup scopes

ppIsDefined' :: PpState -> MacroName -> Bool
ppIsDefined' s name = lookup scopes
  where
    -- Look up the chain of scopes, until we find one that works, or end
    lookup [] = False
    lookup (h : t) =
        if Map.member name (pp_defines h)
            then True
            else lookup t
    scopes = NonEmpty.toList (pp_scope s)

ppDefinition' :: PpState -> MacroName -> Maybe MacroDef
ppDefinition' s name = lookup scopes
  where
    -- Look up the chain of scopes, until we find one that works, or end
    lookup [] = Nothing
    lookup (h : t) =
        if Map.member name (pp_defines h)
            then Map.lookup name (pp_defines h)
            else lookup t
    scopes = NonEmpty.toList (pp_scope s)

getPpState :: PP PpState
getPpState = P $ \s -> POk s (pp s)

-- ---------------------------------------------------------------------

pushContinuation :: Located Token -> PP ()
pushContinuation new =
    P $ \s -> POk s{pp = (pp s){pp_continuation = new : pp_continuation (pp s)}} ()

popContinuation :: PP [Located Token]
popContinuation =
    P $ \s -> POk s{pp = (pp s){pp_continuation = []}} (pp_continuation (pp s))

-- ---------------------------------------------------------------------

module Types where

import GHC.Parser.Lexer (Token (..))
import GHC.Types.SrcLoc
import qualified GHC.Parser.Lexer as Lexer
import GHC.Data.StringBuffer

import Data.Map (Map)
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

initPpState :: PpState
initPpState =
    PpState
        { pp_defines = Map.empty
        , pp_includes = Map.empty
        , pp_include_stack = []
        , pp_continuation = []
        , pp_context = []
        , pp_accepting = True
        }

data PpState = PpState
    { pp_defines :: !(Map MacroName MacroDef)
    , pp_includes :: !(Map String StringBuffer)
    , pp_include_stack :: ![Lexer.AlexInput]
    , pp_continuation :: ![Located Token]
    , pp_context :: ![Token] -- What preprocessor directive we are currently processing
    , pp_accepting :: !Bool
    }
    deriving (Show)

-- ---------------------------------------------------------------------

data CppDirective
    = CppInclude String
    | CppDefine String [String]
    | CppIfdef String
    | CppIfndef String
    | CppIf [String]
    | CppElse
    | CppEndif
    deriving (Show, Eq)

-- ---------------------------------------------------------------------

type MacroArgs = [String]
data MacroName = MacroName String (Maybe MacroArgs)
    deriving (Show, Eq, Ord)
type MacroDef = [String]

-- data PpState = PpState
--     { pp_defines :: !(Map MacroName MacroDef)
--     , pp_accepting :: !Bool
--     }
--     deriving (Show, Eq)

-- initPpState :: PpState
-- initPpState = PpState{pp_defines = Map.empty, pp_accepting = True}

type Input = String
type Output = CppDirective

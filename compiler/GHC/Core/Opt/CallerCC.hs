{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

-- | Adds cost-centers to call sites selected with the @-fprof-caller=...@
-- flag.
module GHC.Core.Opt.CallerCC
    ( addCallerCostCentres
    , CallerCcFilter(..)
    , NamePattern(..)
    , parseCallerCcFilter
    ) where

import Data.Word (Word8)
import Data.Maybe

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Either
import Control.Monad
import qualified Text.ParserCombinators.ReadP as P

import GHC.Prelude
import GHC.Utils.Outputable as Outputable
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Types.Name hiding (varName)
import GHC.Types.Tickish
import GHC.Unit.Module.Name
import GHC.Unit.Module.ModGuts
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Core
import GHC.Core.Opt.Monad
import GHC.Utils.Panic
import qualified GHC.Utils.Binary as B
import Data.Char

addCallerCostCentres :: ModGuts -> CoreM ModGuts
addCallerCostCentres guts = do
  dflags <- getDynFlags
  let filters = callerCcFilters dflags
  let env :: Env
      env = Env
        { thisModule = mg_module guts
        , ccState = newCostCentreState
        , dflags = dflags
        , revParents = []
        , filters = filters
        }
  let guts' = guts { mg_binds = doCoreProgram env (mg_binds guts)
                   }
  return guts'

doCoreProgram :: Env -> CoreProgram -> CoreProgram
doCoreProgram env binds = flip evalState newCostCentreState $ do
    mapM (doBind env) binds

doBind :: Env -> CoreBind -> M CoreBind
doBind env (NonRec b rhs) = NonRec b <$> doExpr (addParent b env) rhs
doBind env (Rec bs) = Rec <$> mapM doPair bs
  where
    doPair (b,rhs) = (b,) <$> doExpr (addParent b env) rhs

doExpr :: Env -> CoreExpr -> M CoreExpr
doExpr env e@(Var v)
  | needsCallSiteCostCentre env v = do
    let nameDoc :: SDoc
        nameDoc = withUserStyle alwaysQualify DefaultDepth $
          hcat (punctuate dot (map ppr (parents env))) <> parens (text "calling:" <> ppr v)

        ccName :: CcName
        ccName = mkFastString $ showSDoc (dflags env) nameDoc
    ccIdx <- getCCIndex' ccName
    let span = case revParents env of
          top:_ -> nameSrcSpan $ varName top
          _     -> noSrcSpan
        cc = NormalCC (ExprCC ccIdx) ccName (thisModule env) span
        tick :: CoreTickish
        tick = ProfNote cc True True
    pure $ Tick tick e
  | otherwise = pure e
doExpr _env e@(Lit _)       = pure e
doExpr env (f `App` x)      = App <$> doExpr env f <*> doExpr env x
doExpr env (Lam b x)        = Lam b <$> doExpr env x
doExpr env (Let b rhs)      = Let <$> doBind env b <*> doExpr env rhs
doExpr env (Case scrut b ty alts) =
    Case <$> doExpr env scrut <*> pure b <*> pure ty <*> mapM doAlt alts
  where
    doAlt (Alt con bs rhs)  = Alt con bs <$> doExpr env rhs
doExpr env (Cast expr co)   = Cast <$> doExpr env expr <*> pure co
doExpr env (Tick t e)       = Tick t <$> doExpr env e
doExpr _env e@(Type _)      = pure e
doExpr _env e@(Coercion _)  = pure e

type M = State CostCentreState

getCCIndex' :: FastString -> M CostCentreIndex
getCCIndex' name = state (getCCIndex name)

data Env = Env
  { thisModule  :: Module
  , dflags      :: DynFlags
  , ccState     :: CostCentreState
  , revParents  :: [Id]
  , filters     :: [CallerCcFilter]
  }

addParent :: Id -> Env -> Env
addParent i env = env { revParents = i : revParents env }

parents :: Env -> [Id]
parents env = reverse (revParents env)

needsCallSiteCostCentre :: Env -> Id -> Bool
needsCallSiteCostCentre env i =
    any matches (filters env)
  where
    matches :: CallerCcFilter -> Bool
    matches ccf =
        checkModule && checkFunc
      where
        checkModule =
          case ccfModuleName ccf of
            Just modFilt
              | Just iMod <- nameModule_maybe (varName i)
              -> moduleName iMod == modFilt
              | otherwise -> False
            Nothing -> True
        checkFunc =
            occNameMatches (ccfFuncName ccf) (getOccName i)

data NamePattern
    = PChar Char NamePattern
    | PWildcard NamePattern
    | PEnd

instance Outputable NamePattern where
  ppr (PChar c rest) = char c <> ppr rest
  ppr (PWildcard rest) = char '*' <> ppr rest
  ppr PEnd = Outputable.empty

instance B.Binary NamePattern where
  get bh = do
    tag <- B.get bh
    case tag :: Word8 of
      0 -> PChar <$> B.get bh <*> B.get bh
      1 -> PWildcard <$> B.get bh
      2 -> pure PEnd
      _ -> panic "Binary(NamePattern): Invalid tag"
  put_ bh (PChar x y) = B.put_ bh (0 :: Word8) >> B.put_ bh x >> B.put_ bh y
  put_ bh (PWildcard x) = B.put_ bh (1 :: Word8) >> B.put_ bh x
  put_ bh PEnd = B.put_ bh (2 :: Word8)

occNameMatches :: NamePattern -> OccName -> Bool
occNameMatches pat = go pat . occNameString
  where
    go :: NamePattern -> String -> Bool
    go PEnd "" = True
    go (PChar c rest) (d:s)
      = d == c && go rest s
    go (PWildcard rest) s
      = go rest s || go (PWildcard rest) (tail s)
    go _ _  = False

type Parser = P.ReadP

parseNamePattern :: Parser NamePattern
parseNamePattern = pattern
  where
    pattern = star P.<++ wildcard P.<++ char P.<++ end
    star = PChar '*' <$ P.string "\\*" <*> pattern
    wildcard = do
      void $ P.char '*'
      PWildcard <$> pattern
    char = PChar <$> P.get <*> pattern
    end = PEnd <$ P.eof

data CallerCcFilter
    = CallerCcFilter { ccfModuleName  :: Maybe ModuleName
                     , ccfFuncName    :: NamePattern
                     }

instance Outputable CallerCcFilter where
  ppr ccf =
    maybe (char '*') ppr (ccfModuleName ccf)
    <> char '.'
    <> ppr (ccfFuncName ccf)

instance B.Binary CallerCcFilter where
  get bh = CallerCcFilter <$> B.get bh <*> B.get bh
  put_ bh (CallerCcFilter x y) = B.put_ bh x >> B.put_ bh y

parseCallerCcFilter :: String -> Either String CallerCcFilter
parseCallerCcFilter inp =
    case P.readP_to_S parseCallerCcFilter' inp of
      ((result, ""):_) -> Right result
      _ -> Left $ "parse error on " ++ inp

parseCallerCcFilter' :: Parser CallerCcFilter
parseCallerCcFilter' =
  CallerCcFilter
    <$> moduleFilter
    <*  P.char '.'
    <*> parseNamePattern
  where
    moduleFilter :: Parser (Maybe ModuleName)
    moduleFilter =
      (Just . mkModuleName <$> moduleName)
      <|>
      (Nothing <$ P.char '*')

    moduleName :: Parser String
    moduleName = do
      c <- P.satisfy isUpper
      cs <- P.munch1 (\c -> isUpper c || isLower c || isDigit c || c == '_')
      rest <- optional $ P.char '.' >> fmap ('.':) moduleName
      return $ c : (cs ++ fromMaybe "" rest)


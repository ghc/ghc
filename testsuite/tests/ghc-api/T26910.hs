module Main where

-- base
import Control.Applicative
import Control.Monad.IO.Class
  ( liftIO )
import Data.List.NonEmpty
  ( NonEmpty (..) )
import System.Environment
  ( getArgs )

-- directory
import System.Directory
  ( removeFile )

-- ghc
import GHC
import GHC.Data.Bag
  ( bagToList )
import GHC.Driver.Ppr
  ( showSDoc )
import GHC.Driver.Session
import GHC.Hs
import GHC.Hs.Syn.Type
  ( hsExprType )
import GHC.Types.Name
  ( nameOccName, occNameString )
import GHC.Types.Var
  ( varName )
import GHC.Unit.Types
  ( GenUnit (..), Definite (..) )
import GHC.Utils.Outputable
  ( ppr )

--------------------------------------------------------------------------------

findBindBody :: String -> LHsBinds GhcTc -> Maybe (HsExpr GhcTc)
findBindBody name = asum . map go
  where
    go (L _ FunBind { fun_id = L _ fid
                    , fun_matches = MG { mg_alts = L _ (m:_) } })
      | occNameString (nameOccName (varName fid)) == name
      = case m_grhss (unLoc m) of
          GRHSs { grhssGRHSs = L _ (GRHS _ _ bodyL) :| _ } -> Just (unLoc bodyL)
    go (L _ (XHsBindsLR AbsBinds { abs_binds })) = findBindBody name abs_binds
    go _                                         = Nothing

checkBinding :: DynFlags -> String -> LHsBinds GhcTc -> IO ()
checkBinding dflags name tcSrc =
  case findBindBody name tcSrc of
    Nothing   ->
      putStrLn $ name ++ " NOT FOUND"
    Just body ->
      putStrLn $
        "(<body of " ++ name ++ ">) :: " ++ showSDoc dflags (ppr (hsExprType body))

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    logger <- getLogger

    -- Add 'template-haskell' dependency
    (dflags, _, _) <- parseDynamicFlags logger dflags [noLoc "-package template-haskell"]
    setSessionDynFlags dflags

    let modName = mkModuleName "T26910_Input"
        m = mkModule (RealUnit (Definite (homeUnitId_ dflags))) modName
    addTarget Target
      { targetId           = TargetModule modName
      , targetAllowObjCode = True
      , targetUnitId       = homeUnitId_ dflags
      , targetContents     = Nothing
      }
    _ <- load LoadAllTargets
    modSum <- getModSummary m
    parsed <- parseModule modSum
    tc     <- typecheckModule StartAndStopTcMPlugins parsed

    let tcSrc = tm_typechecked_source tc
        check name = liftIO $ checkBinding dflags name tcSrc

    check "e_reccon"    -- RecordCon
    check "e_negapp"    -- NegApp
    check "e_proc"      -- HsProc
    check "e_arith_ol"  -- ArithSeq

    check "e_var"       -- ConLikeTc
    check "e_lit"       -- HsLit
    check "e_overlit"   -- HsOverLit
    check "e_lam"       -- HsLam
    check "e_app"       -- HsApp
    check "e_apptype"   -- HsAppType
    check "e_par"       -- HsPar
    check "e_tuple1"    -- ExplicitTuple
    check "e_tuple2"    -- ExplicitTuple + TupleSections
    check "e_tuple3"    -- ExplicitTuple 1-tuple (with Template Haskell)
    check "e_utuple1"   -- ExplicitTuple (unboxed)
    check "e_utuple2"   -- ExplicitTuple + TupleSections (unboxed)
    check "e_usum"      -- Unboxed sums
    check "e_case"      -- HsCase
    check "e_if"        -- HsIf
    check "e_multiif"   -- HsMultiIf
    check "e_let"       -- HsLet
    check "e_list"      -- ExplicitList + OverloadedLists
    check "e_arith"     -- ArithSeq
    check "e_tysig"     -- ExprWithTySig
    check "e_listcomp"  -- HsDo (ListComp)
    check "e_recsel"    -- HsRecSelTc
    check "e_ubracket"  -- HsUntypedBracket
    check "e_tbracket"  -- HsTypedBracket
    check "e_static"    -- HsStatic

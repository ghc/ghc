{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module LintCodes.Static
  ( FamEqnIndex, Use(..), used, outdated
  , LibDir(..), getFamEqnCodes
  , staticallyUsedCodes
  )
  where

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( fromList )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- ghc
import GHC.Driver.Errors.Types
  ( GhcMessage )
import GHC.Types.Error
  ( DiagnosticCode(..) )
import GHC.Types.Error.Codes
  ( GHC, constructorCodes )

-- ghc (API usage)
import GHC
  ( runGhc, parseDynamicFlags
  , getSessionDynFlags, setSessionDynFlags
  , getSession, getLogger
  , noLoc
  )
import GHC.Core.Coercion.Axiom
  ( CoAxBranch(..), coAxiomBranches, fromBranches )
import GHC.Core.TyCon
  ( TyCon, tyConName
  , isClosedSynFamilyTyConWithAxiom_maybe
  )
import qualified GHC.Core.TyCo.Rep as GHC
  ( Type )
import GHC.Core.Type
  ( isNumLitTy, isStrLitTy
  , splitTyConAppNoView_maybe
  )
import GHC.Data.FastString
  ( unpackFS )
import GHC.Driver.Env
  ( lookupType )
import GHC.Iface.Env
  ( lookupOrig )
import GHC.Iface.Load
  ( WhereFrom(..), loadInterface )
import GHC.Types.Name
  ( nameOccName, occNameFS )
import GHC.Types.Name.Occurrence
  ( mkTcOcc )
import GHC.Types.TyThing
  ( TyThing(..) )
import GHC.Types.PkgQual
  ( PkgQual(..) )
import GHC.Tc.Utils.Monad
  ( initIfaceLoad )
import GHC.Unit.Finder
  ( FindResult(..), findImportedModule )
import GHC.Utils.Outputable
  ( text )
import Language.Haskell.Syntax.Module.Name
  ( mkModuleName )

--------------------------------------------------------------------------------

-- | The diagnostic codes that are statically reachable from the
-- 'GhcMessage' datatype.
staticallyUsedCodes :: Map DiagnosticCode String
staticallyUsedCodes = constructorCodes @GHC @GhcMessage

--------------------------------------------------------------------------------

-- | The index of an equation of the 'GhcDiagnosticCode' type family,
-- starting from '1'.
newtype FamEqnIndex = FamEqnIndex Int
  deriving newtype ( Eq, Ord )
  deriving stock Show
-- | Whether an equation of the 'GhcDiagnosticCode' type family is still
-- statically used, or whether it corresponds to an outdated diagnostic code
-- that GHC previously emitted but no longer does.
data Use = Used | Outdated
  deriving stock ( Eq, Show )

used, outdated :: ( FamEqnIndex, String, Use ) -> Maybe ( FamEqnIndex, String )
used ( i, con, Used ) = Just ( i, con )
used _ = Nothing
outdated ( i, con, Outdated ) = Just ( i, con )
outdated _ = Nothing

--------------------------------------------------------------------------------
-- Use the GHC API to obtain the 'TyCon' for the 'GhcDiagnosticCode' type
-- family, and inspect its equations.
-- It would also be possible to use Template Haskell reification, but usage
-- of Template Haskell at compile-time is problematic for Hadrian.

-- | The diagnostic codes returned by the 'GhcDiagnosticCode' type family.
getFamEqnCodes :: Maybe LibDir -> IO ( Map DiagnosticCode ( FamEqnIndex, String, Use ) )
getFamEqnCodes mb_libDir =
  do { tc <- ghcDiagnosticCodeTyCon mb_libDir
     ; return $ case isClosedSynFamilyTyConWithAxiom_maybe tc of
     { Nothing -> error "can't find equations for 'GhcDiagnosticCode'"
     ; Just ax -> Map.fromList
                $ zipWith parseBranch [1..]
                $ fromBranches $ coAxiomBranches ax
     } }

parseBranch :: Int -> CoAxBranch -> ( DiagnosticCode, ( FamEqnIndex, String, Use ) )
parseBranch i ( CoAxBranch { cab_lhs = lhs, cab_rhs = rhs })
  | [ con ] <- lhs
  , Just con_fs <- isStrLitTy con
  , let con_str = unpackFS con_fs
        (code, use) = parseBranchRHS rhs
  = ( DiagnosticCode "GHC" ( fromInteger code ), ( FamEqnIndex i, con_str, use ) )
  | otherwise
  = error "couldn't parse equation of 'GhcDiagnosticCode'"

parseBranchRHS :: GHC.Type -> ( Integer, Use )
parseBranchRHS rhs
  | Just code <- isNumLitTy rhs
  = ( code, use )
  | otherwise
  = error "couldn't parse equation RHS of 'GhcDiagnosticCode'"
  where
    use
      | Just (tc,_) <- splitTyConAppNoView_maybe rhs
      , unpackFS (occNameFS (nameOccName (tyConName tc))) == "Outdated"
      = Outdated
      | otherwise
      = Used

newtype LibDir = LibDir { libDir :: FilePath }

-- | Look up the 'GhcDiagnosticCode' type family using the GHC API.
ghcDiagnosticCodeTyCon :: Maybe LibDir -> IO TyCon
ghcDiagnosticCodeTyCon mb_libDir =
  runGhc (libDir <$> mb_libDir)

  -- STEP 1: start a GHC API session with "-package ghc"
  do { dflags1 <- getSessionDynFlags
     ; let opts = map noLoc ["-package ghc"]
     ; logger <- getLogger
     ; (dflags2, _,_) <- parseDynamicFlags logger dflags1 opts
     ; setSessionDynFlags dflags2
     ; hsc_env <- getSession
     ; liftIO

  -- STEP 2: look up the module "GHC.Types.Error.Codes"
  do { res <- findImportedModule hsc_env (mkModuleName "GHC.Types.Error.Codes") NoPkgQual
     ; case res of
     { Found _ modl ->

  -- STEP 3: look up the 'GhcDiagnosticCode' type family.
  do { nm <- initIfaceLoad hsc_env do
               _ <- loadInterface (text "lint-codes: need 'GhcDiagnosticCode'")
                      modl ImportBySystem
               lookupOrig modl $ mkTcOcc "GhcDiagnosticCode"
     ; mb_tyThing <- lookupType hsc_env nm
     ; return $ case mb_tyThing of
        Just (ATyCon tc) -> tc
        _ -> error "lint-codes: failed to look up TyCon for 'GhcDiagnosticCode'"
     }

     ; _ -> error "lint-codes: failed to find 'GHC.Types.Error.Codes'" } } }

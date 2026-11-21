{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import GHC (getSession, getSessionDynFlags, runGhc)
import GHC.Core (Expr(Coercion))
import GHC.Core.Coercion (coercionKind)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (mkCharLitTy, mkNumLitTy, mkStrLitTy)
import GHC.Data.FastString (LexicalFastString(..), fsLit)
import GHC.Data.IOEnv (IOEnvFailure)
import GHC.Data.Pair (Pair(..))
import GHC.Driver.Env (HscEnv(..))
import GHC.Driver.Ppr (showSDoc)
import GHC.Iface.Binary (CompressionIFace(NormalCompression), TraceBinIFace(QuietBinIFace),
                         getWithUserData, putWithUserData)
import GHC.Iface.Syntax (IfaceExpr(IfaceCo))
import GHC.Iface.Type (IfaceAxiomRule(IfaceAR_X), IfaceCoercion(..), IfaceType(IfaceLitTy),
                       IfaceTyLit(IfaceCharTyLit, IfaceNumTyLit, IfaceStrTyLit), mkIfLclName)
import GHC.IfaceToCore (tcIfaceExpr)
import GHC.Tc.Utils.Monad (initIfaceLcl, initIfaceLoad)
import GHC.Unit.Types (IsBootInterface(NotBoot), mainUnit, mkModule, mkModuleName)
import GHC.Utils.Binary (openBinMem, shrinkBinBuffer)
import GHC.Utils.Logger (pushLogHook)
import GHC.Utils.Outputable (text)
import System.Environment (getArgs)

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
        env <- getSession
        flags <- getSessionDynFlags
        liftIO $ do
            messages <- newIORef []
            let logger = pushLogHook (\_ _ _ _ msg -> modifyIORef' messages (showSDoc flags msg :)) (hsc_logger env)
                mdl = mkModule mainUnit (mkModuleName "AppendSymbolDeductionIface")
                decode name proofs = do
                    writer <- openBinMem 1024
                    putWithUserData QuietBinIFace NormalCompression writer (IfaceCo (rule name proofs))
                    reader <- shrinkBinBuffer writer
                    expression <- getWithUserData (hsc_NC env) reader
                    initIfaceLoad env{hsc_logger = logger} $
                        initIfaceLcl mdl (text "AppendSymbol deduction test") NotBoot $
                            tcIfaceExpr expression
                expectError name fragment proofs = do
                    writeIORef messages []
                    result <- try @IOEnvFailure (decode name proofs)
                    case result of
                        Left _ -> do
                            diagnostics <- unlines <$> readIORef messages
                            unless (fragment `isInfixOf` diagnostics) (fail diagnostics)
                        Right _ -> fail ("Accepted invalid or ambiguous proof for " ++ name)
                expectResult name proof ty = do
                    expression <- decode name [proof]
                    case expression of
                        Coercion co -> case coercionKind co of
                            Pair lhs rhs ->
                                unless (eqType lhs ty && eqType rhs ty) (fail "Wrong deduction endpoints")
                        _ -> fail "Expected a coercion"
            expectError "AppendSymbolT3" "Ambiguous legacy AppendSymbol deduction" [premise]
            expectError "AppendSymbolT3" "Invalid AppendSymbol deduction" [symbol "prefix"]
            expectError "AppendSymbolT3" "Invalid AppendSymbol deduction" [IfaceSubCo premise]
            expectResult "AppendSymbolKnownPrefix" premise (mkStrLitTy (fsLit "fix"))
            expectResult "AppendSymbolKnownSuffix" premise (mkStrLitTy (fsLit "pre"))
            expectResult "CharToNatT1" charPremise (mkCharLitTy '🙂')
            expectResult "CharToNatT1" natPremise (mkNumLitTy 128578)
            expectResult "CharToNatDeduction" charPremise (mkCharLitTy '🙂')
            expectResult "NatToCharDeduction" natPremise (mkNumLitTy 128578)
            expectError "CharToNatDeduction" "Invalid character conversion deduction" [natPremise]
            expectError "NatToCharDeduction" "Invalid character conversion deduction" [charPremise]
            expectError "CharToNatT1" "Invalid character conversion deduction" [IfaceSubCo charPremise]
            expectError "CharToNatT1" "Invalid character conversion deduction" [symbol "prefix"]
            expectError "CharToNatT1" "Invalid character conversion deduction" []
  where
    rule name = IfaceAxiomCo (IfaceAR_X (mkIfLclName (fsLit name)))
    symbol = lit . IfaceStrTyLit . LexicalFastString . fsLit
    lit = IfaceReflCo . IfaceLitTy
    premise = rule "AppendSymbolDef" [symbol "pre", symbol "fix"]
    charPremise = rule "CharToNatDef" [lit (IfaceCharTyLit '🙂')]
    natPremise = rule "NatToCharDef" [lit (IfaceNumTyLit 128578)]

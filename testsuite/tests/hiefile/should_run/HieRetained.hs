module Main where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (isInfixOf)
import qualified Data.Map as M
import GHC.Iface.Ext.Binary
import GHC.Iface.Type (ifaceTyConName)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Cache (newNameCache)
import GHC.Types.Var (DepFunTyFlag(..))
import HieRetainedFixture ()
import TestUtils

main :: IO ()
main = do
  (df, original) <- readTestHie "HieRetainedFixture.hie"
  checkTypes df original
  writeHieFile "HieRetained.roundtrip.hie" original
  (df', decoded) <- readTestHie "HieRetained.roundtrip.hie"
  checkTypes df' decoded
  check "rendering changes after binary roundtrip"
    (rendered df original == rendered df' decoded)
  checkPreviousVersion
  putStrLn "retained HIE types, binding identity, rendering and roundtrip preserved"
  putStrLn "previous HIE version rejected by both readers"

-- | Check decoded constructors without depending on source binder spelling.
checkTypes :: DynFlags -> HieFile -> IO ()
checkTypes df hf = do
  let (visibleFlag, visibleMult, visibleName, visibleDomain, visibleBody) =
        retained (argument hf "visible")
      (invisibleFlag, invisibleMult, invisibleName, invisibleDomain, invisibleBody) =
        retained (argument hf "invisible")
      (functionFlag, functionMult, functionName, functionDomain, functionBody) =
        retained (argument hf "opaque")
      (indexFlag, indexMult, indexName, indexDomain, callBody) = retained functionBody
  check "visible foreach flag lost" (visibleFlag == DepFunVisible)
  check "invisible foreach flag lost" (invisibleFlag == DepFunInvisible)
  check "foreach types share a table key across visibilities"
    (typeIndex hf "visible" /= typeIndex hf "invisible")
  check "scalar binder multiplicity or domain lost"
    (all (isTyCon "Many") [visibleMult, invisibleMult, functionMult, indexMult]
      && all (isTyCon "Nat") [visibleDomain, invisibleDomain, indexDomain])
  check "ordinary runtime index lost its binder identity"
    (isTermVar visibleName (proxyIndex visibleBody)
      && isTermVar invisibleName (proxyIndex invisibleBody))
  check "opaque function binder domain lost" $ case functionDomain of
    Roll (HFunTy _ _ arg result) -> isTyCon "Nat" arg && isTyCon "Nat" result
    _ -> False
  check "nested foreach flags or binder identities lost"
    (functionFlag == DepFunVisible && indexFlag == DepFunVisible
      && functionName /= indexName)
  check "opaque function application lost its bound head or argument" $
    case proxyIndex callBody of
      Roll (HTermAppTy name arg) -> name == functionName && isTermVar indexName arg
      _ -> False
  let [visibleText, invisibleText, opaqueText] = rendered df hf
  check "visible foreach rendering lost" ("foreach" `isInfixOf` visibleText && " -> " `isInfixOf` visibleText)
  check "invisible foreach rendering lost" ("foreach" `isInfixOf` invisibleText && ")." `isInfixOf` invisibleText)
  check "opaque application rendering lost"
    ((getOccString functionName ++ " " ++ getOccString indexName) `isInfixOf` opaqueText)
  where
    retained (Roll (HDepFunTy flag (mult, name, domain) body)) = (flag, mult, name, domain, body)
    retained _ = error "missing retained function in decoded type"
    isTermVar expected (Roll (HTermVarTy actual)) = expected == actual
    isTermVar _ _ = False
    isTyCon expected (Roll (HTyConApp tc (HieArgs []))) = getOccString (ifaceTyConName tc) == expected
    isTyCon _ _ = False
    proxyIndex (Roll (HTyConApp tc (HieArgs [(True, index)])))
      | getOccString (ifaceTyConName tc) == "Proxy" = index
    proxyIndex _ = error "missing Proxy index in decoded type"

-- | Find the type recorded for a fixture witness.
typeIndex :: HieFile -> String -> TypeIndex
typeIndex hf label = case
  [ i | (Right name, uses) <- M.toList refs, getOccString name == label
      , (_, details) <- uses, Just i <- [identType details] ] of
  i : rest | all (== i) rest -> i
  _ -> error ("missing or inconsistent witness type: " ++ label)
  where
    refs = generateReferencesMap (getAsts (hie_asts hf))

-- | Recover the dependent argument of a witness's ordinary function type.
argument :: HieFile -> String -> HieTypeFix
argument hf label = case recoverFullType (typeIndex hf label) (hie_types hf) of
  Roll (HFunTy _ _ arg _) -> arg
  _ -> error ("unexpected witness type: " ++ label)

-- | Render both visibilities and the nested opaque application.
rendered :: DynFlags -> HieFile -> [String]
rendered df hf = [renderHieType df (argument hf label) | label <- ["visible", "invisible", "opaque"]]

-- | Change only the version header and require both readers to reject it.
checkPreviousVersion :: IO ()
checkPreviousVersion = do
  bytes <- BS.readFile "HieRetainedFixture.hie"
  let magicLength = length hieMagic
      (version, rest) = BS.break (== 10) (BS.drop magicLength bytes)
      previous = BSC.pack (show (hieVersion - 1))
      path = "HieRetained.previous.hie"
  check "unexpected current HIE header" (version == BSC.pack (show hieVersion))
  check "version replacement would change binary offsets" (BS.length version == BS.length previous)
  BS.writeFile path (BS.take magicLength bytes <> previous <> rest)
  cache <- newNameCache
  permissive <- readHieFileWithVersion (const True) cache path
  check "permissive reader accepted previous payload" $ case permissive of
    Left (version', _) -> version' == hieVersion - 1
    Right _ -> False
  strict <- try (readHieFile cache path) :: IO (Either SomeException HieFileResult)
  check "strict reader accepted previous payload" $ case strict of
    Left err -> "versions don't match" `isInfixOf` displayException err
    Right _ -> False

-- | Fail with the name of the violated codec invariant.
check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)

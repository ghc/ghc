{-# LANGUAGE DeriveDataTypeable, PatternGuards, RecordWildCards, ConstraintKinds #-}

-- | Errors seen by the user
module Development.Shake.Internal.Errors(
    ShakeException(..),
    throwM, throwImpure,
    errorInternal,
    errorStructured,
    errorNoRuleToBuildType, errorRuleDefinedMultipleTimes,
    errorMultipleRulesMatch, errorRuleRecursion, errorComplexRecursion, errorNoApply,
    errorDirectoryNotFile, errorNoHash
    ) where

import Data.Tuple.Extra
import Control.Exception.Extra
import Control.Monad.IO.Class
import General.Extra
import Data.Typeable
import Data.List.Extra
import Data.Maybe


throwM :: MonadIO m => SomeException -> m a
throwM = liftIO . throwIO

throwImpure :: SomeException -> a
throwImpure = throw


errorInternal :: Partial => String -> SomeException
errorInternal msg = toException $ ErrorCall $ unlines $
    ("Development.Shake: Internal error, please report to Neil Mitchell (" ++ msg ++ ")") : callStackFull

alternatives = let (*) = (,) in
    ["_rule_" * "oracle"
    ,"_Rule_" * "Oracle"
    ,"_key_" * "question"
    ,"_Key_" * "Question"
    ,"_result_" * "answer"
    ,"_Result_" * "Answer"
    ,"_addBuiltinRule_" * "addOracle"
    ,"_apply_" * "askOracle"]


errorStructured :: String -> [(String, Maybe String)] -> String -> SomeException
errorStructured msg args hint = toException $ ErrorCall $ unlines $
        [msg ++ (if null args then "." else ":")] ++
        ["  " ++ a ++ [':' | a /= ""] ++ replicate (as - length a + 2) ' ' ++ b | (a,b) <- args2] ++
        [hint | hint /= ""]
    where
        as = maximum $ 0 : map (length . fst) args2
        args2 = [(a,b) | (a,Just b) <- args]



structured :: Bool -> String -> [(String, Maybe String)] -> String -> SomeException
structured alt msg args hint = errorStructured (f msg) (map (first f) args) (f hint)
    where
        f = filter (/= '_') . (if alt then g else id)
        g xs | res:_ <- [to ++ g rest | (from, to) <- alternatives, Just rest <- [stripPrefix from xs]] = res
        g (x:xs) = x : g xs
        g [] = []


errorDirectoryNotFile :: FilePath -> SomeException
errorDirectoryNotFile dir = errorStructured
    "Build system error - expected a file, got a directory"
    [("Directory", Just dir)]
    "Probably due to calling 'need' on a directory. Shake only permits 'need' on files."

errorNoRuleToBuildType :: TypeRep -> Maybe String -> Maybe TypeRep -> SomeException
errorNoRuleToBuildType tk k tv = structured (specialIsOracleKey tk)
    "Build system error - no _rule_ matches the _key_ type"
    [("_Key_ type", Just $ show tk)
    ,("_Key_ value", k)
    ,("_Result_ type", fmap show tv)]
    "You are missing a call to _addBuiltinRule_, or your call to _apply_ has the wrong _key_ type"

errorRuleDefinedMultipleTimes :: TypeRep -> [String] -> SomeException
errorRuleDefinedMultipleTimes tk locations = structured (specialIsOracleKey tk)
    "Build system error - _rule_ defined twice at one _key_ type"
    (("_Key_ type", Just $ show tk) :
     [("Location " ++ show i, Just x) | (i, x) <- zipFrom 1 locations])
    "You have called _addBuiltinRule_ more than once on the same key type"

errorMultipleRulesMatch :: TypeRep -> String -> [Maybe String] -> SomeException
errorMultipleRulesMatch tk k names = errorStructured
    ("Build system error - key matches " ++ (if null names then "no" else "multiple") ++ " rules")
    ([("Key type",Just $ show tk)
    ,("Key value",Just k)
    ,("Rules matched",Just $ show $ length names)] ++
    [("Rule " ++ show i, x) | any isJust names, (i, x) <- zipFrom 1 names])
    (if null names then "Either add a rule that produces the above key, or stop requiring the above key"
    else "Modify your rules so only one can produce the above key")

errorNoHash :: SomeException
errorNoHash = errorStructured "Cannot use shakeChange=ChangeModTime with shakeShare" [] ""

errorRuleRecursion :: TypeRep -> String -> SomeException
-- may involve both rules and oracle, so report as only rules
errorRuleRecursion tk k = errorStructured
    "Build system error - recursion detected"
    [("Key type",Just $ show tk)
    ,("Key value",Just k)]
    "Rules may not be recursive"

errorComplexRecursion :: [String] -> SomeException
errorComplexRecursion ks = errorStructured
    "Build system error - indirect recursion detected"
    [("Key value " ++ show i, Just k) | (i, k) <- zipFrom 1 ks]
    "Rules may not be recursive"

errorNoApply :: TypeRep -> Maybe String -> String -> SomeException
errorNoApply tk k msg = errorStructured
    "Build system error - cannot currently introduce a dependency (e.g. calling 'apply')"
    [("Reason", Just msg)
    ,("Key type", Just $ show tk)
    ,("Key value", k)]
    "Move the call earlier/later"


-- Should be in Special, but then we get an import cycle
specialIsOracleKey :: TypeRep -> Bool
specialIsOracleKey t = con == "OracleQ"
    where con = show $ fst $ splitTyConApp t


-- | Error representing all expected exceptions thrown by Shake.
--   Problems when executing rules will be raising using this exception type.
data ShakeException = ShakeException
    {shakeExceptionTarget :: String -- ^ The target that was being built when the exception occurred.
    ,shakeExceptionStack :: [String]  -- ^ A description of the call stack, one entry per line.
    ,shakeExceptionInner :: SomeException -- ^ The underlying exception that was raised.
    }
    deriving Typeable

instance Exception ShakeException

instance Show ShakeException where
    show ShakeException{..} = unlines $
        "Error when running Shake build system:" :
        shakeExceptionStack ++
        [displayException shakeExceptionInner]

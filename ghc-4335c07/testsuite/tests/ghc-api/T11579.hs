import System.Environment
import DynFlags
import FastString
import GHC
import StringBuffer
import Lexer
import SrcLoc

main :: IO ()
main = do
    [libdir] <- getArgs

    let stringBuffer = stringToStringBuffer "-- $bar some\n-- named chunk"
        loc = mkRealSrcLoc (mkFastString "Foo.hs") 1 1

    token <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let pstate = mkPState (dflags `gopt_set` Opt_Haddock) stringBuffer loc
        case unP (lexer False return) pstate of
            POk _ token -> return (unLoc token)
            _           -> error "No token"

    -- #11579
    -- Expected:                    "ITdocCommentNamed "bar some\n named chunk"
    -- Actual (with ghc-8.0.1-rc2): "ITdocCommentNamed "bar some"
    print token

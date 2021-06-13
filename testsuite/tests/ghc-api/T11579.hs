import System.Environment
import GHC.Driver.Session
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Data.FastString
import GHC
import GHC.Run
import GHC.Data.StringBuffer
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import Data.Foldable (toList)

main :: IO ()
main = do
    [libdir] <- getArgs

    let stringBuffer = stringToStringBuffer "-- $bar some\n-- named chunk"
        loc = mkRealSrcLoc (mkFastString "Foo.hs") 1 1

    hdk_comments <- runGhcWithAbiHashes (Just libdir) $ do
        dflags <- getSessionDynFlags
        let opts   = initParserOpts (dflags `gopt_set` Opt_Haddock)
            pstate = initParserState opts stringBuffer loc
        case unP (lexer False return) pstate of
            POk s (L _ ITeof) -> return (map unLoc (toList (hdk_comments s)))
            _                 -> error "No token"

    -- #11579
    -- Expected:                    "ITdocCommentNamed "bar some\n named chunk"
    -- Actual (with ghc-8.0.1-rc2): "ITdocCommentNamed "bar some"
    mapM_ print hdk_comments

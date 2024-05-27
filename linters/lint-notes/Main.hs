import qualified Data.Set as S
import System.Process
import System.Environment

import Notes

usage :: IO a
usage = do
    putStrLn $ unlines
        [ "usage:"
        , "    lint-notes <mode>"
        , "    lint-notes <mode> @<response-file>"
        , "    lint-notes <mode> <file>"
        , ""
        , "valid modes:"
        , "  dump             dump all Note definitions and references"
        , "  defs             dump all Note definitions"
        , "  refs             dump all Note references"
        , "  unreferenced     dump all unreferenced Note definitions"
        , "  broken-refs      dump all references to missing Notes"
        ]
    fail "invalid usage"

main :: IO ()
main = do
    args <- getArgs

    let printNoteDefs = putStrLn . unlines . map showNoteDef
        printNoteRefs = putStrLn . unlines . map showNoteRef
        printNoteRefsSugg (bad, sugg) = do
          putStrLn . showNoteRef $ bad
          putStrLn $ "  >" ++ showNoteDef sugg


        parseMode :: String -> Maybe (NoteDb -> IO ())
        parseMode "dump"         = Just $ putStrLn . showNoteDb
        parseMode "unreferenced" = Just $ printNoteDefs . S.toList . unreferencedNotes
        parseMode "defs"         = Just $ printNoteDefs . allNoteDefs
        parseMode "refs"         = Just $ printNoteRefs . allNoteRefs
        parseMode "broken-refs"  = Just $ \notedb -> do
          putStrLn "Broken note references (target note not found!):"
          printNoteRefs . map fst . brokenNoteRefs $ notedb
        parseMode "broken-refs-suggest" = Just $ mapM_ printNoteRefsSugg . brokenNoteRefs
        parseMode _              = Nothing

    (mode, files) <- case args of
      [mode, "@-"] -> do
        files <- lines <$> getContents
        return (parseMode mode, files)
      [mode, '@':respFile] -> do
        files <- lines <$> readFile respFile
        return (parseMode mode, files)
      [mode] -> do
        let excludeList =
              [ "testsuite/tests/linters/notes.stdout"
              , "linters/lint-notes/test" ]
        files <- lines <$> readProcess "git" ["ls-tree", "--name-only", "-r", "HEAD"] ""
        return (parseMode mode, filter (`notElem` excludeList) files)
      _ -> usage

    case mode of
      Just run -> filesNotes files >>= run
      Nothing -> return ()


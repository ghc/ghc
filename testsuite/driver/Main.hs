
module Main where

import CmdSyntax	( Var, TestName, Result(..), 
			  Expr(..), TestID(..),
			  officialMsg, panic,
			  isJust, isNothing, unJust,
			  isLeft, isRight, unLeft, unRight )
import CmdSemantics	( parseOneTFile, processParsedTFile )
import Directory
import System
import List
import Monad		( when )

--import IOExts(trace)

findTFiles :: FilePath        -- name of root dir of tests
           -> IO [FilePath]   -- full pathnames of all .T files

findTFiles root_in
   = snarf ((reverse . dropWhile (== '/') . reverse) root_in) "."
     where
        snarf root dir
           = do --putStr "snarf: "
                --print (root,dir)
                let this_dir = root ++ "/" ++ dir

                dir_contents_raw <- getDirectoryContents this_dir
                let dir_contents = filter (`notElem` [".", ".."]) 
                                          dir_contents_raw

                let tag_subdir f = do b <- doesDirectoryExist 
                                              (this_dir ++ "/" ++ f)
                                      return (b, f)

                tagged_contents <- mapM tag_subdir dir_contents
                --print tagged_contents
                let tfiles_in_this_dir
                       =  [this_dir ++ "/" ++ f 
                          | (False, f) <- tagged_contents,
                                          f `hasSuffix` ".T"]
                --print tests_in_this_dir
                let subdir_names
                       = [ f | (True, f) <- tagged_contents ]
                subdir_tfiless
                   <- mapM (\d -> snarf root (dir++"/"++d)) subdir_names
                let all_tfiless 
                       = tfiles_in_this_dir ++ concat subdir_tfiless
                return 
                   (map scrub all_tfiless)


hasSuffix :: String -> String -> Bool
hasSuffix str suff 
   = let r_suff = reverse suff
         r_str  = reverse str
     in  r_suff == take (length r_suff) r_str
    

-- (eg)   "tests/./test1/run.stderr"  -->  "tests/test1/run.stderr"
scrub :: String -> String
scrub []               = []
scrub ('/':'.':'/':cs) = scrub ('/':cs)
scrub (c:cs)           = c : scrub cs


usage
   = "usage:\n" ++
     "runtests --tool=<compiler-to-test>\n" ++   	-- "
     "         --config=<path_to_config_file>\n" ++ 	-- "
     "         path_to_root_of_tests_directory"


main
   = getArgs >>= main_really
imain arg_str
   = main_really (words arg_str)
test
   = imain "--tool=ghc --config=../config/msrc/cam-02-unx.T ../tests/"

main_really arg_ws0
   = do { let (arg_ws1, maybe_tool) = fish arg_ws0 "--tool="
        ; let (arg_ws2, maybe_conf) = fish arg_ws1 "--config="
        ; if (length arg_ws2 /= 1 || isNothing maybe_tool 
                                  || isNothing maybe_conf) 
           then do officialMsg usage
                   exitWith (ExitFailure 1)
           else 

     do { let tool = unJust maybe_tool
              conf = unJust maybe_conf
              (confdir, conffile) = splitPathname conf
              root_dir = head arg_ws2
              base_genv = [("tool", tool), 
                           ("confdir", confdir), 
                           ("conffilename", conffile)]

        ; conf_ok <- doesFileExist conf
        ; if    not conf_ok
           then do officialMsg ("Config file `" ++ conf ++ "' doesn't exist.")
                   exitWith (ExitFailure 1)
           else 

     do { -- Find all the .T files
        ; all_tfiles <- findTFiles root_dir
        ; putStr "\n"
        ; officialMsg ("Found " ++ show (length all_tfiles) 
                       ++ " test description files:")
        ; putStr "\n"
        ; putStrLn (unlines (map ("   "++) all_tfiles))
        -- Parse them all
        ; all_parsed 
             <- mapM (\tfpath -> parseOneTFile
                                    (addTVars base_genv tfpath) tfpath)
                     all_tfiles
        ; let parse_fails = filter isLeft all_parsed
        ; when (not (null parse_fails)) (
             do officialMsg ("Parse errors for the following .T files:\n")
                putStr (unlines (map (("   "++).unLeft) parse_fails))
          )
        ; let parsed_ok = map unRight (filter isRight all_parsed)
        -- Run all the tests in each successfully-parsed .T file.
        ; resultss 
             <- mapM ( \ (path,topdefs) -> processParsedTFile 
                                              path
                                              (addTVars base_genv path)
                                              topdefs) 
                     parsed_ok
        ; let results = concat resultss
        ; putStr "\n"
        ; officialMsg ("=== All done. ===")
        -- ; putStr ("\n" ++ ((unlines . map show) results))
        ; putStr ("\n" ++ executive_summary results)
        ; putStr "\n"
        -- ; exitWith ExitSuccess
     }}}

addTVars some_genv tfpath
   = case splitPathname tfpath of
         (tfdir, tfname) -> ("testdir", tfdir) 
                            : ("testfilename", tfname) 
                            : some_genv

-- Summarise overall outcome
executive_summary :: [(TestID, Maybe (Result, Result))] 
                  -> String
executive_summary outcomes
   = let n_cands     = length outcomes
         meta_fails  = filter is_meta_fail outcomes
         outcomes_ok = filter (not.is_meta_fail) outcomes
         skipped     = filter is_skip       outcomes_ok

         p_p = filter (got ((== Pass), (== Pass))) outcomes_ok
         p_f = filter (got ((== Pass), (== Fail))) outcomes_ok
         f_p = filter (got ((== Fail), (== Pass))) outcomes_ok
         f_f = filter (got ((== Fail), (== Fail))) outcomes_ok

         exp_u = filter (got ((== Unknown), const True)) outcomes_ok
         act_u = filter (got (const True, (== Unknown))) outcomes_ok
         u_u   = filter (got ((== Unknown), (== Unknown))) outcomes_ok

         unexpected_u
            = filter (`notElem` u_u) (exp_u ++ act_u)

         unexpected = nub (p_f ++ f_p ++ unexpected_u)

         summary
            = unlines [ "OVERALL SUMMARY:"
                      , ""
                      , "   " ++ show n_cands 
                              ++ " total test candidates, of which:"
                      , "   " ++ show (length meta_fails) 
                              ++ " framework failures,"
                      , "   " ++ show (length skipped) ++ " were skipped,"
                      , ""
                      , "   " ++ show (length p_p) ++ " expected passes,"
                      , "   " ++ show (length f_f) ++ " expected failures,"
                      , "   " ++ show (length f_p) ++ " unexpected passes,"
                      , "   " ++ show (length p_f) ++ " unexpected failures,"
                      , ""
                      , "   " ++ show (length exp_u) ++ " specified as unknown,"
                      , "   " ++ show (length act_u) ++ " actual unknowns,"
                      , "   " ++ show (length u_u) ++ " expected unknowns."
                      ]

         unexpected_summary
            | null unexpected 
            = ""
            | otherwise
            = "\nThe following tests had unexpected outcomes:\n"
              ++ unlines (map ppTest unexpected)

         metafail_summary
            | null meta_fails
            = ""
            | otherwise
            = "\nThe following tests had framework failures:\n"
              ++ unlines (map (("   "++).show.fst) meta_fails)

         ppTest (test, Just (exp,act))
             = "   exp:" ++ show exp ++ ", act:" ++ show act 
               ++ "    " ++ show test

         is_meta_fail (_, Nothing) = True
         is_meta_fail other       = False

         got (f1,f2) (_, Just (r1,r2)) = f1 r1 && f2 r2
         got (f1,f2) other             = False

         is_skip (_, Just (Skipped, Skipped)) = True
         is_skip (_, Just (r1, r2))
            | r1 == Skipped || r2 == Skipped 
            = panic "is_skip"
         is_skip other = False

         is_exp_unk (_, Just (Unknown, Unknown)) = True
         is_exp_unk other                         = False
     in
         summary ++ unexpected_summary ++ metafail_summary

-- Helper for cmd line args
fish :: [String] -> String -> ([String], Maybe String)
fish strs prefix
   = let n_prefix  = length prefix
         pfx_eq    = (== prefix).(take n_prefix)
         matched   = filter pfx_eq strs
         unmatched = filter (not.pfx_eq) strs
     in  case matched of
            [m] -> (unmatched, Just (drop n_prefix m))
            _   -> (strs,      Nothing)


-- (eg) "foo/bar/xyzzy.ext" --> ("foo/bar", "xyzzy.ext")
splitPathname :: String -> (String, String)
splitPathname full
   | '/' `notElem` full = (".", full)
   | otherwise
   = let full_r = reverse full
         f_r    = takeWhile (/= '/') full_r
         p_r    = drop (1 + length f_r) full_r
     in  if null p_r then (".", reverse f_r)
                     else (reverse p_r, reverse f_r)


-- (eg) "foo/bar/xyzzy.ext" --> ("foo/bar", "xyzzy", "ext")
--splitPathname3 full
--   = let (dir, base_and_ext) = splitPathname full
--     in  if '.' `elem` base_and_ext
--         then let r_bande = reverse base_and_ext
--                  r_ext = takeWhile (/= '.') r_bande
--                  r_root = drop (1 + length r_ext) r_bande
--              in  (dir, reverse r_root, reverse r_ext)
--         else (dir, base_and_ext, "")


module Main where

import RunOneTest 	( run_one_test )
import CmdSyntax	( Var, Result(..), Expr(..), officialMsg, panic )
import Directory
import System
import List

findTests :: FilePath        -- name of root dir of tests
          -> IO [FilePath]   -- name of all dirs containing "testconfig.T"

findTests root_in
   = snarf ((reverse . dropWhile (== '/') . reverse) root_in)
           "."
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
                let include_this_dir
                       = (not.null) [ () | (False, f) <- tagged_contents, 
                                                         f == "testconfig.T"]
                let subdir_names
                       = [ f | (True, f) <- tagged_contents ]
                subdir_testss
                   <- mapM (\d -> snarf root (dir++"/"++d)) subdir_names
                let subdir_tests 
                       = (if include_this_dir then [scrub (root++"/"++dir)] 
                                              else [])
                         ++ concat subdir_testss
                return subdir_tests

        -- (eg)   "tests/./test1/run.stderr"  -->  "tests/test1/run.stderr"
        scrub :: String -> String
        scrub []               = []
        scrub ('/':'.':'/':cs) = '/':scrub cs
        scrub (c:cs)           = c : scrub cs


run_multiple_tests :: [(Var,String)] 		-- default var binds
                   -> [FilePath] 		-- paths to test dirs
                   -> IO [(FilePath, 
                           Maybe (Result, Result))]
run_multiple_tests base_varenv dirs_containing_tests
   = mapM f dirs_containing_tests
     where f a_dir = do res <- run_one_test a_dir base_varenv
                        return (a_dir, res)


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
   = imain "--tool=ghc --config=../config/cam-02-unx ../tests/"

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
              base_varenv = [("tool", tool), ("confdir", confdir), 
                             ("conffile", conffile)]

        ; conf_ok <- doesFileExist conf
        ; if    not conf_ok
           then do officialMsg ("Config file `" ++ conf ++ "' doesn't exist.")
                   exitWith (ExitFailure 1)
           else 

     do { all_tests <- findTests root_dir
        ; putStr "\n"
        ; officialMsg ("Found " ++ show (length all_tests) ++ " tests:")
        ; putStrLn (unlines all_tests)
        ; all_results <- run_multiple_tests base_varenv all_tests
        ; putStr "\n"
        ; officialMsg ("All done.")
        -- ; putStr ("\n" ++ ((unlines . map show) all_results))
        ; putStr ("\n" ++ executive_summary all_results)
        ; putStr "\n"
        -- ; exitWith ExitSuccess
     }}}
     where
        splitPathname full
           | '/' `notElem` full = (".", full)
           | otherwise
           = let full_r = reverse full
                 f_r    = takeWhile (/= '/') full_r
                 p_r    = drop (1 + length f_r) full_r
             in  if null p_r then (".", reverse f_r)
                             else (reverse p_r, reverse f_r)

-- Summarise overall outcome
executive_summary :: [(FilePath, Maybe (Result, Result))] -> String
executive_summary outcomes
   = let n_cands    = length outcomes
         meta_fails = filter is_meta_fail outcomes
         outcomes_ok = filter (not.is_meta_fail) outcomes
         skipped    = filter is_skip       outcomes_ok

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
              ++ unlines (map (("   "++).fst) meta_fails)

         ppTest (dir, Just (exp,act))
             = "   exp:" ++ show exp ++ ", act:" ++ show act ++ "    " ++ dir

         is_meta_fail (_, Nothing) = True
         is_meta_fail other        = False

         got (f1,f2) (_, Just (r1,r2)) = f1 r1 && f2 r2
         got (f1,f2) other             = False

         is_skip (_, Just (Skipped, Skipped)) = True
         is_skip (_, Just (r1, r2))
            | r1 == Skipped || r2 == Skipped 
            = panic "is_skip"
         is_skip other = False

         is_exp_unk (_, Just (Unknown, Unknown)) = True
         is_exp_unk other                        = False
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

isNothing Nothing  = True
isNothing (Just _) = False

unJust (Just x) = x

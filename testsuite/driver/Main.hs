
module Main where

import CmdSyntax
import CmdSemantics	( parseOneTFile, processParsedTFile )
import Directory
import System
import List
import Monad		( when )
import Time		( ClockTime, getClockTime )
import IO		( catch )


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
                let dir_contents = filter ((/= ".").(take 1))
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
   = "\nusage:\n" ++
     "runtests --tool=<compiler-to-test>\n" ++   		   -- "
     "         --config=<path_to_config_file>\n" ++ 		   -- "
     "         --rootdir=<path_to_root_of_tests_directory>\n" ++   -- "
     "         --save-summary=<path_of_summary_file_to_create>\n" ++   -- "
     "         --compare-summary=<path_of_summary_file_to_compare_against>\n" ++   -- "
     "         [--$var=literal-value]\n" ++			   -- "
     "         [names of tests to run, if you don't want all]\n" ++
     "\n" ++
     "         You may not set any of the following vars from the\n" ++
     "         command line:\n" ++
     concatMap (\w -> "            $" ++ w ++ "\n") special_var_names


main
   = getArgs >>= main_really
imain arg_str
   = main_really (words arg_str)
test
   = imain ("--tool=ghc --config=../config/msrc/cam-02-unx.T " 
            ++ "--rootdir=../tests/codeGen")

main_really arg_ws0
   = do { start_time <- getClockTime
        ; let (arg_ws1, maybe_tool)    = fish arg_ws0 "--tool="
        ; let (arg_ws2, maybe_conf)    = fish arg_ws1 "--config="
        ; let (arg_ws3, maybe_root)    = fish arg_ws2 "--rootdir="
        ; let (arg_ws4, maybe_save_su) = fish arg_ws3 "--save-summary="
        ; let (arg_ws5, maybe_cmp_su)  = fish arg_ws4 "--compare-summary="
        ; let (arg_ws6, cmd_binds)  = getBinds arg_ws5
        ; let invalid_binds
                 = filter (`elem` special_var_names) 
                          (map fst cmd_binds)
        ; if (isNothing maybe_tool 
              || isNothing maybe_conf
              || isNothing maybe_root
              || not (null invalid_binds)) 
           then do officialMsg usage
                   exitWith (ExitFailure 1)
           else 

     do { let tool = unJust maybe_tool
              conf = unJust maybe_conf
              root = unJust maybe_root
              (confdir, conffile) = splitPathname conf
              base_genv = [("tool", tool), 
                           ("confdir", confdir), 
                           ("conffilename", conffile)]

              tests_to_run
                 = if null arg_ws6 then Nothing{-all of them-}
                                   else Just arg_ws4{-just these-}

        ; conf_ok <- doesFileExist conf
        ; if    not conf_ok
           then do officialMsg ("Config file `" ++ conf ++ "' doesn't exist.")
                   exitWith (ExitFailure 1)
           else 

     do { -- Find all the .T files
        ; all_tfiles <- findTFiles root
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
                                              tests_to_run
                                              path
                                              (addTVars base_genv path)
                                              topdefs) 
                     parsed_ok
        ; let results = concat resultss
        ; putStr "\n"
        ; officialMsg ("=== All done. ===")
        -- ; putStr ("\n" ++ ((unlines . map show) results))
        ; let summary = makeSummary start_time results
        ; putStr ("\n" ++ snd (ppSummary summary))
        ; putStr "\n"
        ; case maybe_cmp_su of
             Nothing -> return ()
             Just fn 
                -> do { officialMsg ("=== Comparing against: " ++ fn ++ " ===")
                      ; maybe_txt <- maybe_readFile fn
                      ; case maybe_txt of {
                           Nothing -> do 
                              officialMsg ("=== Can't read abovementioned file ===")
                              return ()
                         ; Just txt -> 
                   do { case ((reads txt) :: [(Summary, String)]) of
                           ((old_summ, ""):_)
                              -> do putStrLn ""
                                    putStrLn (ppSummaryDiffs old_summ summary)
                           other
                              -> do officialMsg 
                                       ("=== Parse error in: " ++ fn ++ " ===")
                                    return ()
                      }}}
        ; case maybe_save_su of
             Nothing -> return ()
             Just fn -> do officialMsg ("=== Saving summary in: " ++ fn ++ " ===")
                           wr_ok <- maybe_writeFile fn (show summary)
                           when (not wr_ok)
                              (officialMsg ("=== Can't write abovementioned file ==="))
        ; putStrLn ""
        -- ; exitWith ExitSuccess
     }}}
     where
        addTVars some_genv tfpath
           = case splitPathname tfpath of
                (tfdir, tfname) -> ("testdir", tfdir) 
                                   : ("testfilename", tfname) 
                                   : some_genv


data Summary 
   = Summary {
        start_time :: String,
        n_cands :: Int,
        frame_fails,	-- Tests which got a framework failure
        skipped,	-- Tests which asked to be skipped
        p_p,		-- Tests which: expect Pass, actual Pass 
        p_f,		-- Tests which: expect Pass, actual Fail 
        f_p, 		-- Tests which: expect Fail, actual Pass 
        f_f, 		-- Tests which: expect Fail, actual Fail
        exp_u,		-- Tests which: expect Unknown
        act_u,		-- Tests which: actual Unknown
        u_u,		-- Tests which: expect Unknown, Actual Unknown
        u_notu,		-- Tests which: expect Unknown, Actual /= Unknown
        unexp		-- All tests for which Expected /= Actual
           :: [TestResult]
     }
        deriving (Show, Read)


-- Cook up a summary from raw test results
makeSummary :: ClockTime	-- when the test run started
            -> [TestResult]
            -> Summary

makeSummary start_time outcomes
   = let n_cands     = length outcomes
         frame_fails = filter is_frame_fail outcomes
         outcomes_ok = filter (not.is_frame_fail) outcomes
         skipped     = filter is_skip       outcomes_ok

         p_p    = filter (got (== Pass) (== Pass)) outcomes_ok
         p_f    = filter (got (== Pass) (== Fail)) outcomes_ok
         f_p    = filter (got (== Fail) (== Pass)) outcomes_ok
         f_f    = filter (got (== Fail) (== Fail)) outcomes_ok

         exp_u  = filter (got (== Unknown) (const True)) outcomes_ok
         act_u  = filter (got (const True) (== Unknown)) outcomes_ok
         u_u    = filter (got (== Unknown) (== Unknown)) outcomes_ok
         u_notu = filter (got (== Unknown) (/= Unknown)) outcomes_ok

         unexp  = nubBy (\a b -> testid_of_TestResult a == testid_of_TestResult b)
                        (p_f ++ f_p ++ u_notu)
     in 
         Summary { start_time=show start_time,
                   n_cands=n_cands, 
                   frame_fails=frame_fails, skipped=skipped,
                   p_p=p_p, p_f=p_f, f_p=f_p, f_f=f_f,
                   exp_u=exp_u, act_u=act_u, u_u=u_u, u_notu=u_notu,
                   unexp=unexp
                 }
     where
        is_frame_fail (TestFFail _)   = True
        is_frame_fail (TestRanOK _ _ _) = False

        got f1 f2 (TestRanOK nm r1 r2) = f1 r1 && f2 r2
        got f1 f2 (TestFFail nm)     = False

        is_skip (TestRanOK nm Skipped Skipped) = True
        is_skip (TestRanOK nm r1 r2)
           | r1 == Skipped || r2 == Skipped 
           = panic "is_skip"
        is_skip other = False


-- Produce both verbose and ultra-terse versions of a Summary
ppSummary :: Summary -> (String, String)
ppSummary s
   = let summary_big
            = unlines [ "OVERALL SUMMARY for run started at " 
                           ++ start_time s
                      , ""
                      , "   " ++ show (n_cands s)
                              ++ " total test candidates, of which:"
                      , "   " ++ show (length (frame_fails s)) 
                              ++ " framework failures,"
                      , "   " ++ show (length (skipped s)) ++ " were skipped,"
                      , ""
                      , "   " ++ show (length (p_p s)) ++ " expected passes,"
                      , "   " ++ show (length (f_f s)) ++ " expected failures,"
                      , "   " ++ show (length (f_p s)) ++ " unexpected passes,"
                      , "   " ++ show (length (p_f s)) ++ " unexpected failures,"
                      , ""
                      , "   " ++ show (length (exp_u s)) ++ " specified as unknown,"
                      , "   " ++ show (length (act_u s)) ++ " actual unknowns,"
                      , "   " ++ show (length (u_u s)) ++ " expected unknowns."
                      ]

         summary_short
            = show (n_cands s) ++ " cands, " 
              ++ show (length (frame_fails s)) ++ " framework-failed, " 
              ++ show (length (skipped s)) ++ " skipped, "
              ++ show (length (unexp s)) ++ " unexpected outcomes."

         unexpected_summary
            | null (unexp s)
            = ""
            | otherwise
            = "\nThe following tests had unexpected outcomes:\n"
              ++ unlines (map (("   "++). ppTestID . testid_of_TestResult) 
                              (unexp s))

         framefail_summary
            | null (frame_fails s)
            = ""
            | otherwise
            = "\nThe following tests had framework failures:\n"
              ++ unlines (map (("   "++).show) (frame_fails s))
     in
         (summary_short,
          summary_big ++ unexpected_summary ++ framefail_summary)


-- Print differences between two summaries
ppSummaryDiffs old new
   = "DIFFERENCES from run of " ++ start_time old ++ "\n"
     ++ "\n"
     ++ "Prev: "
     ++ fst (ppSummary old) 
     ++ "\n Now: "
     ++ fst (ppSummary new)
     ++ if null diff_unexp then []
        else "\nNew unexpected outcomes:\n" ++ unlines (map ppTestResult diff_unexp)

     where
        old_unexp = unexp old
        now_unexp = unexp new
        old_unexp_ids = map testid_of_TestResult old_unexp
        diff_unexp = filter (\new_u -> testid_of_TestResult new_u 
                                          `notElem` old_unexp_ids)
                            now_unexp


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

-- Extract all the   --$var=value  binds from a bunch of cmd line opts.
getBinds :: [String] -> ([String], [(Var,String)])
getBinds cmd_line_opts
   = f [] [] cmd_line_opts 
     where
        f acc_n acc_y []
           = (reverse acc_n, reverse acc_y)
        f acc_n acc_y (str:strs)
           | take 3 str /= "--$" || '=' `notElem` drop 3 str
           = f (str:acc_n) acc_y strs
           | otherwise
           = let varname = takeWhile (/= '=') (drop 3 str)
                 value   = drop (4 + length varname) str
             in 
                 f acc_n ((varname,value):acc_y) strs

-- These vars have special meanings and may not be set from the
-- command line.
special_var_names
   = ["testfilename", "testdir", "conffilename", "confdir", 
      "tool", "testname"]


-- File reader/writer with exception catching.
maybe_readFile :: FilePath -> IO (Maybe String)
maybe_readFile fname
   = catch (readFile fname >>= (return . Just))
           (const (return Nothing))

maybe_writeFile :: FilePath -> String -> IO Bool
maybe_writeFile fname cts
   = catch (writeFile fname cts >> (return True))
           (const (return False))


-- (eg) "foo/bar/xyzzy.ext" --> ("foo/bar", "xyzzy", "ext")
--splitPathname3 full
--   = let (dir, base_and_ext) = splitPathname full
--     in  if '.' `elem` base_and_ext
--         then let r_bande = reverse base_and_ext
--                  r_ext = takeWhile (/= '.') r_bande
--                  r_root = drop (1 + length r_ext) r_bande
--              in  (dir, reverse r_root, reverse r_ext)
--         else (dir, base_and_ext, "")


module Main where

import CmdSyntax
import CmdSemantics	( parseOneTFile, processParsedTFile )

import GetOpt
import Directory
import System
import List
import Maybe		( isJust, fromJust )
import Monad		( when )
import Time		( ClockTime, getClockTime )
import IO		( try )


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


data Opt
 = OptConfig         String
 | OptRootDir        String
 | OptOutSummary     String
 | OptSaveSummary    String
 | OptCompareSummary String

option_descrs = [
  Option "" ["config"]      (ReqArg OptConfig "FILE")
	"config file",
  Option "" ["rootdir"]     (ReqArg OptRootDir "DIR")
	"root of tree containing tests (default: .)",
  Option "" ["output-summary"] (ReqArg OptOutSummary "PATH")
	"file in which to save the (human-readable) summary",
  Option "" ["save-summary"] (ReqArg OptSaveSummary "PATH")
	"file in which to save the summary",
  Option "" ["compare-summary"] (ReqArg OptCompareSummary "PATH")
	"old summary to compare against"
  ]

usage      = GetOpt.usageInfo header option_descrs ++ '\n':extra_info
header     = "runtests [OPTION | VAR=VALUE | TEST]..."

extra_info
   = "         You may not set any of the following vars from the\n" ++
     "         command line:\n" ++
     concatMap (\w -> "            $" ++ w ++ "\n") special_var_names

getConfigOpt    os = exactlyOne "-config"       [ t | OptConfig  t <- os ]
getRootOpt      os = upToOne    "-rootdir"      [ t | OptRootDir t <- os ]
getOutSuOpt     os = upToOne "-output-summary"  [ t | OptOutSummary t <- os ]
getSaveSuOpt    os = upToOne "-save-summary"    [ t | OptSaveSummary t <- os ]
getCompareSuOpt os = upToOne "-compare-summary" [ t | OptCompareSummary t <- os]

exactlyOne s []  = die ("missing " ++ '-':s ++ " option")
exactlyOne _ [a] = return a
exactlyOne s as  = die ("multiple " ++ '-':s ++ " options")

upToOne s []  = return Nothing 
upToOne s [a] = return (Just a)
upToOne s as  = die ("multiple " ++ '-':s ++ " options")

main
   = getArgs >>= main_really
imain arg_str
   = main_really (words arg_str)
test
   = imain ("tool=ghc --config=../config/msrc/cam-02-unx.T " 
            ++ "--rootdir=../tests/codeGen")

main_really arg_ws0
   = do start_time <- getClockTime
    	case getOpt Permute option_descrs arg_ws0 of 
	    (opts, args, []) -> got_args opts args start_time
	    (_, _, errs)     -> die (concat errs ++ usage)

got_args opts args start_time
  = do   conf <- getConfigOpt opts
 	 maybe_root <- getRootOpt opts
 	 maybe_save_su <- getSaveSuOpt opts
 	 maybe_cmp_su <- getCompareSuOpt opts
	 maybe_out_su <- getOutSuOpt opts

         let (not_binds, cmd_binds)  = getBinds args
         let invalid_binds
                 = filter (`elem` special_var_names) 
                          (map fst cmd_binds)

	 when (not (null invalid_binds)) $ do
	   die ("cannot set special variable $" ++ head invalid_binds ++
		" on the command line")

         let -- default root is '.'
	      root | isJust maybe_root = fromJust maybe_root
		   | otherwise         = "." 

              (confdir, conffile) = splitPathname conf

              base_genv = [("confdir", confdir), 
                           ("conffilename", conffile)] ++ cmd_binds

              tests_to_run
                 = if null not_binds then Nothing{-all of them-}
                                     else Just not_binds{-just these-}

         conf_ok <- doesFileExist conf
         when (not conf_ok) (die ("config file `" ++ conf ++ "' doesn't exist"))

        -- Find all the .T files
         all_tfiles <- findTFiles root
         officialMsg ("Found " ++ show (length all_tfiles) 
                       ++ " test description files:")
         putStrLn (unlines (map ("   "++) all_tfiles))
        -- Parse them all
         all_parsed 
             <- mapM (\tfpath -> parseOneTFile
                                    (addTVars base_genv tfpath) tfpath)
                     all_tfiles
         let parse_fails = filter isLeft all_parsed
         when (not (null parse_fails)) (
             do officialMsg ("Parse errors for the following .T files:\n")
                putStr (unlines (map (("   "++).unLeft) parse_fails))
          )
         let parsed_ok = map unRight (filter isRight all_parsed)
        -- Run all the tests in each successfully-parsed .T file.
         resultss 
             <- mapM ( \ (path,topdefs) -> processParsedTFile 
                                              tests_to_run
                                              path
                                              (addTVars base_genv path)
                                              topdefs) 
                     parsed_ok
         let results = concat resultss
         putStr "\n"
         officialMsg ("=== All done. ===")
        -- ; putStr ("\n" ++ ((unlines . map show) results))
         let summary = makeSummary start_time results

	 let pp_summary = snd (ppSummary summary)
	 if isJust maybe_out_su
	    then writeFile (fromJust maybe_out_su) pp_summary
	    else putStrLn ("\n" ++ pp_summary)

         case maybe_cmp_su of
             Nothing -> return ()
             Just fn 
                -> do { officialMsg ("=== Comparing against: " ++ fn ++ " ===")
                      ; maybe_txt <- try (readFile fn)
                      ; case maybe_txt of {
                           Left err -> do 
                              officialMsg ("=== Can't read abovementioned file ===")
			      print err
                         ; Right txt -> 
                   do { case ((reads txt) :: [(Summary, String)]) of
                           ((old_summ, ""):_)
                              -> do putStrLn ""
                                    putStrLn (ppSummaryDiffs old_summ summary)
                           other
                              -> do officialMsg 
                                       ("=== Parse error in: " ++ fn ++ " ===")
                                    return ()
                      }}}
         case maybe_save_su of
             Nothing -> return ()
             Just fn -> do officialMsg ("=== Saving summary in: " ++ fn ++ " ===")
                           wr_ok <- try (writeFile fn (show summary))
			   case wr_ok of
			     Right _  -> return ()
			     Left err -> do officialMsg ("=== Can't write abovementioned file ===")
					    print err
         putStrLn ""
        -- ; exitWith ExitSuccess
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
              ++ unlines (map (("   "++). ppTestID . testid_of_TestResult) 
                              (frame_fails s))
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

-- Extract all the var=value binds from a bunch of cmd line opts.
getBinds :: [String] -> ([String], [(Var,String)])
getBinds args = f args [] []
  where
   f [] binds rest = (reverse rest, reverse binds)
   f (arg:args) binds rest
	| isJust maybe_bind  = f args (fromJust maybe_bind:binds) rest
	| otherwise          = f args binds (arg:rest)
      where
	maybe_bind = is_bind (break (== '=') arg)
   	is_bind (var, '=':value) = Just (var,value)
   	is_bind str = Nothing

-- These vars have special meanings and may not be set from the
-- command line.
special_var_names
   = ["testfilename", "testdir", "conffilename", "confdir", "testname"]

-- (eg) "foo/bar/xyzzy.ext" --> ("foo/bar", "xyzzy", "ext")
--splitPathname3 full
--   = let (dir, base_and_ext) = splitPathname full
--     in  if '.' `elem` base_and_ext
--         then let r_bande = reverse base_and_ext
--                  r_ext = takeWhile (/= '.') r_bande
--                  r_root = drop (1 + length r_ext) r_bande
--              in  (dir, reverse r_root, reverse r_ext)
--         else (dir, base_and_ext, "")


module RunOneTest ( run_one_test )
where

import CmdSyntax
import CmdSemantics
import Directory
import Monad	( when )
import System	( ExitCode(..) )

-- This function should always return, no matter how disastrously
-- things go.  If things go badly wrong, ie the test dir does not
-- exist, or the config files have syntax errs, or whatever, it should
-- print suitable error messages and return MetaFail.  Then its caller
-- (which is responsible for running multiple tests) can decide whether
-- to abort the entire run, or keep going, as it pleases.

-- returns: 
---   Nothing if there is a meta-failure -- for whatever
--       reason, the test could not be conducted.  This indicates
--       a failure of the test framework; this should never happen.
--    Just (expected_result, actual_result) -- fst is the result
--       which the testconfig.T file says should happen, the second
--       is what actually happened.

run_one_test :: FilePath	-- test dir
             -> [(Var, String)]	-- default var bindings
		-- containing at least $tool, $confdir, $conffile
             -> IO (Maybe (Result, Result))
run_one_test test_dir p_default
   = do { putStr "\n"
        ; officialMsg ("====== " ++ test_dir ++ " ======")
        ; dir_exists <- doesDirectoryExist test_dir
        ; if    not dir_exists 
           then do officialMsg ("test directory `" ++ test_dir ++ 
                                "' doesn't exist.")
                   return Nothing
           else 

     do { let p_init = p_default ++ [("testdir", test_dir)]
        ; let tds = [mkInclude (EVar "confdir") (EVar "conffile"),
                     mkInclude (EVar "testdir") (EString "testconfig.T")]
        -- ; print (show tds, show p_init)
        ; doEval test_dir p_init tds
     }}
     where
        mkInclude dir file 
           = TInclude (EOp OpAppend dir (EOp OpAppend (EString "/") file))
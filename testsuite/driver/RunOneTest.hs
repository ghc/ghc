
module RunOneTest ( OneTest(..), t_root_of, run_one_test, splitPathname )
where

import CmdSyntax
import CmdSemantics
import Directory
import Monad	( when )
import System	( ExitCode(..) )

data OneTest
   -- Exactly using the specified .T file
   = Precisely FilePath
   -- Using the specified .T file, but the supplied testroot
   | Defaulted FilePath String
     deriving Eq

instance Show OneTest where
   show ot@(Precisely t_file) = t_file
   show ot@(Defaulted t_file basename)
      = t_file ++ "(" ++ t_root_of ot ++ ")"

-- Get the name of the .T file to use
t_file_of (Precisely t_file) = t_file
t_file_of (Defaulted t_file basename) = t_file

-- Get the name of the test root
t_root_of (Precisely t_file) 
   = case splitPathname3 t_file of (dir, root, ext) -> root
t_root_of (Defaulted t_file basefile) 
   = case splitPathname3 basefile of (dir, root, ext) -> root



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

run_one_test :: OneTest
             -> [(Var, String)]	-- default var bindings
		-- containing at least $tool, $confdir, $conffile
             -> IO (Maybe (Result, Result))
run_one_test test_descr p_default
   = do { putStr "\n"
        ; officialMsg ("====== " ++ show test_descr ++ " ======")
        ; let t_file_path = t_file_of test_descr
        ; let t_root      = t_root_of test_descr
        ; t_exists <- doesFileExist t_file_path
        ; if    not t_exists 
           then do officialMsg ("test file `" ++ t_file_path ++ 
                                "' doesn't exist.")
                   return Nothing
           else 

     do { let (t_file_dir, t_file_name) = splitPathname t_file_path
        ; let p_init = p_default 
                       ++ [("testdir",  t_file_dir),
                           ("testroot", t_root),
                           ("testfile", t_file_name)]
        ; let tds = [mkInclude (EVar "confdir") (EVar "conffile"),
                     mkInclude (EVar "testdir") (EVar "testfile")]
        -- ; print (show tds, show p_init)
        ; doEval t_file_dir p_init tds
     }}
     where
        mkInclude dir file 
           = TInclude (EOp OpAppend dir (EOp OpAppend (EString "/") file))


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
splitPathname3 full
   = let (dir, base_and_ext) = splitPathname full
     in  if '.' `elem` base_and_ext
         then let r_bande = reverse base_and_ext
                  r_ext = takeWhile (/= '.') r_bande
                  r_root = drop (1 + length r_ext) r_bande
              in  (dir, reverse r_root, reverse r_ext)
         else (dir, base_and_ext, "")


-----------------------------------------------------------------------
--- Stuff to do with multiple-source-file tests.  We assume         ---
--- that the name of the test is to be used as the basename         ---
--- for everything.                                                 ---
-----------------------------------------------------------------------

-- global variables:
$stdin = ""
$expect = "pass"
$normalise_errmsg = False
$normalise_output = False

---------------------------------------------------------------
--- UTILITY FNs                                             ---
---------------------------------------------------------------

include ($confdir ++ "/" ++ $conffilename)
include ($confdir ++ "/../std-macros.T")

-- (eg) "fooble"  --> "testdir/fooble"
def testdirify ( $basename )
{
   return $testdir ++ "/" ++ $basename
}

---------------------------------------------------------------
--- COMPILATION                                             ---
---------------------------------------------------------------

-- Clean up prior to the test, so that we can't spuriously conclude
-- that it passed on the basis of old run outputs.
def pretest_cleanup()
{
   rm_nofail(qualify("comp.stderr"))
   rm_nofail(qualify("run.stderr"))
   rm_nofail(qualify("run.stdout"))
   -- simple_build_Main zaps the following:
   --    objects
   --    executable
   -- not interested in the return code
}

-- Guess flags suitable for the compiler.
def guess_compiler_flags()
{
   if   $tool contains "ghc"
   then 
        return "-no-recomp --make -dcore-lint" ++
               " -i" ++ $testdir
   else 

-- Problem here is that nhc and hbc don't understand --make,
--    and we rely on it.
-- if   $tool contains "nhc"
-- then 
--      return "-an-nhc-specific-flag"
-- else
-- if   $tool contains "hbc"
-- then
--      return ""
-- else

        framefail ("Can't guess what kind of Haskell compiler " ++ 
                   "you're testing: $tool = " ++ $tool)

-- fi
-- fi
   fi
}

-- Build Main, and return the compiler result code.  Compilation
-- output goes into testname.comp.stderr.  Source is assumed to
-- be in Main.hs or Main.lhs, and modules reachable from it.

def simple_build_prog_WRK ( $_main, $_extra_args ) 
{
   $flags = guess_compiler_flags()
   $errname = qualify("comp.stderr")
   $exename = qualify("")  -- ie, the exe name == the test name

   rm_or_fail($errname)
   rm_or_fail($exename)
   rm_nofail(testdirify("*.o"))
   $cmd = "\"" ++ $tool ++ "\" " ++ $flags ++ " " ++ $_extra_args ++ " "
	  ++ (if defined $extra_hc_flags
		  then $extra_hc_flags 
		  else "")
          ++ " -o " ++ $exename ++ " " 
          ++ $_main ++ " >" ++ $errname ++ " 2>&1"
   $res = run $cmd
   return $res
}


---------------------------------------------------------------
--- CONDUCTING A COMPLETE TEST                              ---
---------------------------------------------------------------

-- Compile and run (should_run) style test

def multimod-compile( $mod, $extra_compile_args )
{
   pretest_cleanup()

   $main = if $mod == "" then "Main" else $mod
   $res = simple_build_prog_WRK( $main, $extra_compile_args )

   if   $res /= "0" then
	say_fail_because_compiler_barfd ( $res )
        return False 
   else
	return True
   fi
}

def multimod-run-test ( $extra_run_args, 
                        $allowable_nonzero_exit_code )
{
   $exit_code = 
      if $allowable_nonzero_exit_code /= "" then 
	  $allowable_nonzero_exit_code 
      else "0"

   return simple_run_pgm( $extra_run_args, $exit_code )
}


---------------------------------------------------------------
--- TOP-LEVEL FNS                                           ---
---------------------------------------------------------------

--------------------------------------------------------------
-- top-level
-- Compile and run (should_run) style test

def mtc ( $mod, $extra_compile_args )
{
   $test_passed = multimod-compile( $mod, $extra_compile_args )

   if ($expect == "pass") then
      expect pass
   else
      expect fail
   fi
   pass when $test_passed
   fail when otherwise
}

def mtr ( $extra_compile_args, 
          $extra_run_args, 
          $allowable_nonzero_exit_code )
{
   $test_passed 
      = multimod-compile( "Main", $extra_compile_args )
	&& multimod-run-test( $extra_run_args, 
                              $allowable_nonzero_exit_code )

   if ($expect == "pass") then
      expect pass
   else
      expect fail
   fi
   pass when $test_passed
   fail when otherwise
}


-----------------------------------------------------------------------
--- end                                             multimod-test.T ---
-----------------------------------------------------------------------

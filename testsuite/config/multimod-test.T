
-----------------------------------------------------------------------
--- Stuff to do with multiple-source-file tests.  We assume         ---
--- that the name of the test is to be used as the basename         ---
--- for everything.                                                 ---
-----------------------------------------------------------------------

-- global variables:
$stdin = ""
$expect = "pass"

---------------------------------------------------------------
--- UTILITY FNs                                             ---
---------------------------------------------------------------

include ($confdir ++ "/" ++ $conffilename)
include ($confdir ++ "/../std-macros.T")

-- (eg) "run.stdout"  -->  "testdir/testname.run.stdout"
def qualify ( $_filename_frag )
{
   if   $_filename_frag == ""
   then
      return $testdir ++ "/" ++ $testname
   else
      return $testdir ++ "/" ++ $testname ++ "." ++ $_filename_frag
   fi
}

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

def simple_build_Main_WRK ( $_extra_args ) 
{
   $flags = guess_compiler_flags()
   $errname = qualify("comp.stderr")
   $exename = qualify("")  -- ie, the exe name == the test name
   $srcname = testdirify("Main.hs")
   if   not(exists($srcname))
   then $srcname = testdirify("Main.lhs")
   fi
   rm_or_fail($errname)
   rm_or_fail($exename)
   rm_nofail(testdirify("*.o"))
   $cmd = $tool ++ " " ++ $flags ++ " " ++ $_extra_args ++ " "
	  ++ (if defined $extra_hc_flags
		  then $extra_hc_flags 
		  else "")
          ++ " -o " ++ $exename ++ " " 
          ++ $srcname ++ " &> " ++ $errname
   $res = run $cmd
   return $res
}


---------------------------------------------------------------
--- RUNNING, AND ASSESSING RUN RESULTS                      ---
---------------------------------------------------------------

-- Run testname.  If testname.stdin exists, route input from that, else
-- from /dev/null.  Route output to testname.run.stdout and 
-- testname.run.stderr.  Returns the exit code of the run.

def simple_run_main( $_extra_args )
{
   -- figure out what to use for stdin
   $devnull = "/dev/null"
   if $stdin == "" then
	$stdin = qualify("stdin")
   fi
   $use_stdin = if exists($stdin) then $stdin else $devnull

   $run_stdout = qualify("run.stdout")
   $run_stderr = qualify("run.stderr")

   rm_or_fail($run_stdout)
   rm_or_fail($run_stderr)
   $cmd = qualify("") 
          ++ " " ++ $_extra_args
          ++ " < " ++ $use_stdin
          ++ " > " ++ $run_stdout
          ++ " 2> " ++ $run_stderr
   $res = runCmd($cmd)
   return $res
}


-- returns True if both files exist and are identical.
def same ( $_file1, $_file2 )
{
   if defined $verbose 
	then print "vanilla-test: comparing " ++ $_file1
		 ++ " and " ++ $_file2
   fi
   $cts1 = contents($_file1)
   $cts2 = contents($_file2)
   $same = $cts1 == $cts2
   if not($same) then 
	say_fail_because_noteq($_file1, $_file2) 
   fi
   return $same
}


-- Check that the run.stdout file matches at least one spec'd stdout.
def check_stdout_ok()
{
   $r_stdout  = qualify("run.stdout")
   $s_stdout  = qualify("stdout")

   if   not ( exists($s_stdout) )
   then if ((contents $r_stdout) == "")
	   then return True
	   else say_fail_because_nonempty($r_stdout)
		return False
	fi
   fi

   return same($s_stdout, $r_stdout)
}

-- If there's any spec'd stderr files, check that the run.stderr matches it.
-- Check that the run.stdout file matches at least one spec'd stdout.
def check_stderr_ok()
{
   $r_stderr  = qualify("run.stderr")
   $s_stderr  = qualify("stderr")

   if   not ( exists($s_stderr) )
   then if ((contents $r_stderr) == "")
	   then return True
	   else say_fail_because_output_produced($r_stdout)
		return False
	fi
   fi

   return same($s_stderr, $r_stderr)
}

---------------------------------------------------------------
--- CONDUCTING A COMPLETE TEST                              ---
---------------------------------------------------------------

-- Compile and run (should_run) style test

def multimod-run-test-actions ( $extra_compile_args, 
                                $extra_run_args, 
                                $allowable_nonzero_exit_code )
{
   pretest_cleanup()

   $res = simple_build_Main_WRK( $extra_compile_args )
   -- If the compiler barf'd, fail.
   if   $res /= "0" 
   then say_fail_because_compiler_barfd ( $res )
        return False 
   fi

   $res = simple_run_main( $extra_run_args )
   if   $res /= "0" && $res /= $allowable_nonzero_exit_code
   then print "multimod-test: unexpected exit code (" ++ $res 
              ++ ") from testee"
        return False
   fi
   $test_passed = check_stdout_ok() && check_stderr_ok()
   return $test_passed
}


---------------------------------------------------------------
--- TOP-LEVEL FNS                                           ---
---------------------------------------------------------------

--------------------------------------------------------------
-- top-level
-- Compile and run (should_run) style test

def mtr ( $extra_compile_args, 
          $extra_run_args, 
          $allowable_nonzero_exit_code )
{
   $test_passed 
      = multimod-run-test-actions ( $extra_compile_args, 
                                    $extra_run_args, 
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

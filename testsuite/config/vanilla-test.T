-----------------------------------------------------------------------
--- Stuff to do with simple single-source-file tests.  We assume    ---
--- that the name of the test is to be used as the basename         ---
--- for everything.                                                 ---
-----------------------------------------------------------------------

-- global variables:
$stdin = ""
$expect = "pass"

---------------------------------------------------------------
-- Define the following things on the command line:
--
-- $verbose	print command lines
-- $accept	accept any changed output

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

def testnameWith ( $_filename_frag )
{
   if   $_filename_frag == ""
   then
      return $testname
   else
      return $testname ++ "." ++ $_filename_frag
   fi
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
   -- rm_nofail(qualify("o"))
   -- rm_nofail(qualify(""))
   -- not interested in the return code
}

-- Guess flags suitable for the compiler.
def guess_compiler_flags()
{
   if   $tool contains "ghc"
   then 
        return "-no-recomp -dcore-lint" 
   else 
   if   $tool contains "nhc"
   then 
        return "-an-nhc-specific-flag"
   else
   if   $tool contains "hbc"
   then
        return ""
   else
        framefail ("Can't guess what kind of Haskell compiler " ++ 
                   "you're testing: $tool = " ++ $tool)
   fi
   fi
   fi
}

-- Build Main, and return the compiler result code.  Compilation
-- output goes into testname.comp.stderr.

def simple_build_Main_WRK ( $_extra_args, $compile_only ) 
{
   $flags = guess_compiler_flags()
   $errname = qualify("comp.stderr")
   $exename = qualify("")  -- ie, the exe name == the test name
   $srcname = qualify("hs")
   rm_or_fail($errname)
   rm_or_fail($exename)
   $cmd = $tool ++ " " ++ $flags ++ " " ++ $_extra_args ++ " "
	  ++ (if defined $extra_hc_flags
		  then $extra_hc_flags 
		  else "")
          ++ (if defined $compile_to_hc && $compile_to_hc
		  then " -C "
	     	  else if $compile_only 
                  	then " -c " 
                  	else " -o " ++ $exename ++ " ")
          ++ $srcname ++ " &> " ++ $errname
   $res = runCmd($cmd)
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
   if $stdin /= "" then
	$use_stdin = $stdin
   else
	$stdin = testnameWith("stdin")
	$stdin_path = $testdir ++ "/" ++ $stdin
	$use_stdin = if exists($stdin_path) then $stdin else $devnull
   fi

   $run_stdout = testnameWith("run.stdout")
   $run_stderr = testnameWith("run.stderr")

   rm_or_fail($run_stdout)
   rm_or_fail($run_stderr)
   $cmd = "cd " ++ $testdir ++ " && "
	  ++ "./" ++ $testname ++ " " ++ $_extra_args
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

def vanilla-run-test-actions ( $extra_compile_args, 
                               $extra_run_args, 
                               $allowable_nonzero_exit_code )
{
   pretest_cleanup()

   $res = simple_build_Main_WRK( $extra_compile_args, False )
   -- If the compiler barf'd, fail.
   if   $res /= "0" 
   then say_fail_because_compiler_barfd ( $res )
        return False 
   fi

   $res = simple_run_main( $extra_run_args )
   if   $res /= "0" && $res /= $allowable_nonzero_exit_code
   then print "vanilla-test: unexpected exit code (" ++ $res 
              ++ ") from testee"
        return False
   fi
   $test_passed = check_stdout_ok() && check_stderr_ok()
   return $test_passed
}


-- Compile only (should_compile) style test.  Deemed to have
-- succeeded if the compiler returned zero AND (testname.comp.stderr
-- matches testname.stderr, if it exists, or is empty).

def vanilla-compok-test-actions ( $extra_compile_args )
{
   pretest_cleanup()
   $res = simple_build_Main_WRK ( $extra_compile_args, True )

   -- If the compiler barf'd, fail.
   if   $res /= "0" 
   then say_fail_because_compiler_barfd ( $res )
        return False 
   fi

   -- If there's an expected .stderr, presumably containing
   -- warnings, ensure the compiler produced the same.
   $actual_stderr   = qualify("comp.stderr")
   $expected_stderr = qualify("stderr")
   if   exists($expected_stderr)
   then $stderr_a = normalise_errmsg(contents($actual_stderr))
        $stderr_e = normalise_errmsg(contents($expected_stderr))
        if   $stderr_e /= $stderr_a
        then say_fail_because_noteq($expected_stderr, $actual_stderr)
             return False
        else return True
        fi
   fi

   -- There's no expected stderr, so just insist that the compiler
   -- produced nothing on stderr.
   if   (contents $actual_stderr) /= "" 
   then say_fail_because_nonempty($actual_stderr)
        return False
   fi

   -- Must have succeeded.
   return True
}


-- Compile with expected fail (should_fail) style test.  Deemed to have
-- succeeded if the compiler returned nonzero AND testname.comp.stderr
-- equals testname.stderr.

def vanilla-compfail-test-actions ( $extra_compile_args )
{
   pretest_cleanup()
   $expected_stderr = qualify("stderr")

   -- Sanity check
   if   not(exists($expected_stderr)) 
   then framefail "should_fail: expected .stderr is missing" 
   fi

   $res = simple_build_Main_WRK ( $extra_compile_args, True )

   $actual_stderr = qualify("comp.stderr")
   $stderr_a = normalise_errmsg(contents($actual_stderr))
   $stderr_e = normalise_errmsg(contents($expected_stderr))

   if   $stderr_e /= $stderr_a
   then say_fail_because_noteq($expected_stderr, $actual_stderr)
        return False
   fi

   if $res /= "0" && $stderr_e == $stderr_a
   then
     return True
   else
     return False
   fi
}


---------------------------------------------------------------
--- TOP-LEVEL FNS                                           ---
---------------------------------------------------------------

--------------------------------------------------------------
-- top-level
-- Compile and run (should_run) style test

def vtr ( $extra_compile_args, 
          $extra_run_args, 
          $allowable_nonzero_exit_code )
{
   $test_passed 
      = vanilla-run-test-actions ( $extra_compile_args, 
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


-- Compile only (should_compile) style test

def vtc ( $extra_compile_args )
{
   $test_passed = vanilla-compok-test-actions ( $extra_compile_args )

   if ($expect == "pass") then
      expect pass
   else
      expect fail
   fi
   pass when $test_passed
   fail when otherwise
}


-- Compile only, and expect failure (should_fail) style test

def vtcf ( $extra_compile_args )
{
   $test_passed = vanilla-compfail-test-actions ( $extra_compile_args )

   if ($expect == "pass") then
      expect pass
   else
      expect fail
   fi
   pass when $test_passed
   fail when otherwise
}



-----------------------------------------------------------------------
--- end                                              vanilla-test.T ---
-----------------------------------------------------------------------

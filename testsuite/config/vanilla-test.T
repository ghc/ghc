-----------------------------------------------------------------------
--- Stuff to do with simple single-source-file tests.  We assume    ---
--- that the name of the test is to be used as the basename         ---
--- for everything.                                                 ---
-----------------------------------------------------------------------

-- global variables:
$stdin = ""
$expect = "pass"
$normalise_errmsg = False
$normalise_output = False

---------------------------------------------------------------
-- Define the following things on the command line:
--
-- $platform    TARGETPLATFORM from config.mk
-- $verbose	print command lines
-- $accept	accept any changed output

---------------------------------------------------------------
--- UTILITY FNs                                             ---
---------------------------------------------------------------

include ($confdir ++ "/" ++ $conffilename)
include ($confdir ++ "/../std-macros.T")

---------------------------------------------------------------
--- COMPILATION                                             ---
---------------------------------------------------------------

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
   $errname = testnameWith("comp.stderr")
   $srcname = testnameWith("hs")
   rm_or_fail($errname)
   rm_or_fail($testname)

   $cmd = "cd " ++ $testdir ++ " && " ++ 
	  $tool ++ " " ++ $flags ++ " " ++ $_extra_args ++ " "
	  ++ (if defined $extra_hc_flags
		  then $extra_hc_flags 
		  else "")
          ++ (if defined $compile_to_hc && $compile_to_hc
		  then " -C "
	     	  else if $compile_only 
                  	then " -c " 
                  	else " -o " ++ $testname ++ " ")
          ++ $srcname ++ " >" ++ $errname ++ " 2>&1"
   print $cmd
   $res = runCmd($cmd)
   return $res
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

   $exit_code = 
      if $allowable_nonzero_exit_code /= "" then 
	  $allowable_nonzero_exit_code 
      else "0"

   return simple_run_pgm( $extra_run_args, $exit_code )
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
   $expected_stderr = platform_qualify("stderr")
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
   $expected_stderr = platform_qualify("stderr")

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

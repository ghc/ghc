
-----------------------------------------------------------------------
--- Stuff to do with multiple-source-file tests.  We assume         ---
--- that the name of the test is to be used as the basename         ---
--- for everything.                                                 ---
-----------------------------------------------------------------------

---------------------------------------------------------------
--- UTILITY FNs                                             ---
---------------------------------------------------------------

include ($confdir ++ "/" ++ $conffilename)

-- Gotta do some pretty basic stuff :)
def not ( $_bool )
{
        if $_bool == "True" then return False
   else if $_bool == "False" then return True
   else framefail ("not(): invalid input: " ++ $_bool )
   fi fi
}

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

-- Delete a file and abort if that doesn't work.
def rm_or_fail ( $_files )
{
   $cmd = "rm -f " ++ $_files
   $res = run $cmd
   if $res /= "0" then framefail ("rm_or_fail: can't rm: " ++ $_files) fi
}

-- Delete a file but keep going antidisirregardless of the outcome.
def rm_nofail ( $_files )
{
   $cmd = "rm -f " ++ $_files
   $res = run $cmd
}

-- Pipe an error message through normalise_errmsg.
def normalise_errmsg ( $errmsg )
{
   $unpathify = $confdir ++ "/../../utils/normalise_errmsg/normalise_errmsg"
   $normd = $errmsg | $unpathify
   return $normd
}

-- Give hints as to why a test is failing.
def say_fail_because_noteq ( $filename1, $filename2 )
{
   print "--- FAIL because the following files differ:"
   print "---      " ++ $filename1
   print "---      " ++ $filename2
}

def say_fail_because_nonempty ( $filename1 )
{
   print "--- FAIL because the following file is non-empty:"
   print "---      " ++ $filename1
}

def say_fail_because_compiler_barfd ( $res )
{
   print "--- FAIL because the compiler returned non-zero exit code = " ++ $res
   $comp_stderr = qualify("comp.stderr")
   if   exists($comp_stderr)
   then print "--- Error messages:"
        $barfage = contents(qualify("comp.stderr"))
        print $barfage
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
   $cmd = $tool ++ " " ++ $flags ++ " " ++ $_extra_args
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
   $devnull = "/dev/null"
   $spec_stdin = qualify("stdin")
   $run_stdout = qualify("run.stdout")
   $run_stderr = qualify("run.stderr")
   $use_stdin
      = if exists($spec_stdin) then $spec_stdin else $devnull fi

   rm_or_fail($run_stdout)
   rm_or_fail($run_stderr)
   $cmd = qualify("") 
          ++ " " ++ $_extra_args
          ++ " < " ++ $use_stdin
          ++ " > " ++ $run_stdout
          ++ " 2> " ++ $run_stderr
   $res = run $cmd
   return $res
}


-- returns True if both files exist and are identical.
def exist_and_same ( $_file1, $_file2 )
{
   if   not(exists($_file1) && exists($_file2))
   then return False
   else print "multimod-test: comparing " ++ $_file1 ++ " and " ++ $_file2
        $cts1 = contents($_file1)
        $cts2 = contents($_file2)
        $same = $cts1 == $cts2
        if not($same) then say_fail_because_noteq($_file1, $_file2) fi
        return $same
   fi
}


-- Check that the run.stdout file matches at least one spec'd stdout.
def check_stdout_ok()
{
   $r_stdout  = qualify("run.stdout")
   $s_stdout  = qualify("stdout")
   $s1_stdout = qualify("stdout1")
   $s2_stdout = qualify("stdout2")
   $s3_stdout = qualify("stdout3")
   $s4_stdout = qualify("stdout4")
   $sm_stdout = qualify("stdout-mingw")

   -- check for minimal level of sanity
   if   not ( exists($s_stdout) && exists($r_stdout) )
   then framefail "multimod-test: .stdout and/or .run.stdout are missing"
   fi

   return
      exist_and_same($s_stdout, $r_stdout)
      || exist_and_same($s1_stdout, $r_stdout)
      || exist_and_same($s2_stdout, $r_stdout)
      || exist_and_same($s3_stdout, $r_stdout)
      || exist_and_same($s4_stdout, $r_stdout)
      || exist_and_same($sm_stdout, $r_stdout)
}


-- If there's any spec'd stderr files, check that the run.stderr matches it.
-- Check that the run.stdout file matches at least one spec'd stdout.
def check_stderr_ok()
{
   $r_stderr  = qualify("run.stderr")
   $s_stderr  = qualify("stderr")
   $s1_stderr = qualify("stderr1")
   $s2_stderr = qualify("stderr2")
   $s3_stderr = qualify("stderr3")
   $s4_stderr = qualify("stderr4")
   $sm_stderr = qualify("stderr-mingw")

   -- if it doesn't look like there's any stderr to be seen, it's OK.
   if not ( exists($s_stderr) ) then return True fi

   return
      exist_and_same($s_stderr, $r_stderr)
      || exist_and_same($s1_stderr, $r_stderr)
      || exist_and_same($s2_stderr, $r_stderr)
      || exist_and_same($s3_stderr, $r_stderr)
      || exist_and_same($s4_stderr, $r_stderr)
      || exist_and_same($sm_stderr, $r_stderr)
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


-- Compile only (should_compile) style test.  Deemed to have
-- succeeded if the compiler returned zero AND (testname.comp.stderr
-- matches testname.stderr, if it exists, or is empty).
--
--def vanilla-compok-test-actions ( $extra_compile_args )
--{
--   pretest_cleanup()
--   $res = simple_build_Main_WRK ( $extra_compile_args, True )
--
-- -- If the compiler barf'd, fail.
-- if   $res /= "0" 
-- then say_fail_because_compiler_barfd ( $res )
--      return False 
-- fi
--
-- -- If there's an expected .stderr, presumably containing
-- -- warnings, ensure the compiler produced the same.
-- $actual_stderr   = qualify("comp.stderr")
-- $expected_stderr = qualify("stderr")
-- if   exists($expected_stderr)
-- then $stderr_a = normalise_errmsg(contents($actual_stderr))
--      $stderr_e = normalise_errmsg(contents($expected_stderr))
--      if   $stderr_e /= $stderr_a
--      then print ( "-- UNMATCHED WARNING MSGS: EXPECTED\n" ++ $stderr_e ++
--                   "-- ACTUAL\n" ++ $stderr_a ++ "-- END\n" )
--           return False
--      else return True
--      fi
-- fi
--
-- -- There's no expected stderr, so just insist that the compiler
-- -- produced nothing on stderr.
-- if   contents($actual_stderr) /= "" 
-- then say_fail_because_nonempty($actual_stderr)
--      return False
-- fi
--
-- -- Must have succeeded.
-- return True
-- }


-- Compile with expected fail (should_fail) style test.  Deemed to have
-- succeeded if the compiler returned nonzero AND testname.comp.stderr
-- equals testname.stderr.
--
--def vanilla-compfail-test-actions ( $extra_compile_args )
--{
--   pretest_cleanup()
--   $expected_stderr = qualify("stderr")
--
--   -- Sanity check
--   if   not(exists($expected_stderr)) 
--   then framefail "should_fail: expected .stderr is missing" 
--   fi
--
--   $res = simple_build_Main_WRK ( $extra_compile_args, True )
--
--   $stderr_a = normalise_errmsg(contents(qualify("comp.stderr")))
--   $stderr_e = normalise_errmsg(contents($expected_stderr))
--
--   if $stderr_e /= $stderr_a
--   then print ( "-- UNMATCHED ERROR MSGS: EXPECTED\n" ++ $stderr_e ++
--                "-- ACTUAL\n" ++ $stderr_a ++ "-- END\n" )
--   fi
--
--   if $res /= "0" && $stderr_e == $stderr_a
--   then
--     return True
--   else
--     return False
--   fi
--}


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
   expect pass
   pass when $test_passed
   fail when otherwise
}


---- Compile only (should_compile) style test
--
--def vtc ( $extra_compile_args )
--{
--   $test_passed 
--      = vanilla-compok-test-actions ( $extra_compile_args )
--   expect pass
--   pass when $test_passed
--   fail when otherwise
--}
--
--
---- Compile only, and expect failure (should_fail) style test
--
--def vtcf ( $extra_compile_args )
--{
--   $test_passed 
--      = vanilla-compfail-test-actions ( $extra_compile_args )
--   expect pass
--   pass when $test_passed
--   fail when otherwise
--}



-----------------------------------------------------------------------
--- end                                             multimod-test.T ---
-----------------------------------------------------------------------


include ($confdir ++ "/" ++ $conffile)

-----------------------------------------------------------------------
--- Stuff to do with simple single-source-file tests.  We assume    ---
--- that the name of the .T file is to be used as the basename      ---
--- for everything.                                                 ---
-----------------------------------------------------------------------

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
      return $testdir ++ "/" ++ $testroot
   else
      return $testdir ++ "/" ++ $testroot ++ "." ++ $_filename_frag
   fi
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

-- Clean up prior to the test, so that we can't spuriously conclude
-- that it passed on the basis of old run outputs.
def pretest_cleanup()
{
   rm_nofail(qualify("comp.stdout"))
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
        return "-no-recomp"
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


-- Compile testname.hs into testname; comp errors -> testname.comp.stderr.
-- Used for run tests, so framefail if compilation fails.

def simple_build_Main ( $_extra_args ) 
{
   $flags = guess_compiler_flags()
   $errname = qualify("comp.stderr")
   $exename = qualify("")  -- ie, the exe name == the test name
   $srcname = qualify("hs")
   rm_or_fail($errname)
   rm_or_fail($exename)
   $cmd = $tool ++ " " ++ $flags ++ " " ++ $_extra_args
          ++ " -o " ++ $exename ++ " " 
          ++ $srcname ++ " &> " ++ $errname
   $res = run $cmd
   if $res /= "0" then framefail "simple_build_Main: failed" fi
}


-- Run testname.  If testname.stdin exists, route input from that, else
-- from /dev/null.  Route output to testname.run.stdout and 
-- testname.run.stderr.  Returns the exit code of the run.

def simple_run_main()
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
          ++ " < " ++ $use_stdin
          ++ " > " ++ $run_stdout
          ++ " 2> " ++ $run_stderr
   $res = run $cmd
   return $res
}


-- returns True if both files exist and are identical.
def exist_and_same ( $_file1, $_file2 )
{
   if   not( exists($_file1) && exists($_file2))
   then return False
   else print "vanilla-test: comparing " ++ $_file1 ++ " and " ++ $_file2
        return (contents($_file1) == contents($_file2))
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
   then framefail "vanilla-test: can't see a spec'd or run stdout missing"
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


-- top-level actions
def vanilla-test ()
{
   pretest_cleanup()
   simple_build_Main( $extra_args )
   $res = simple_run_main()
   if $res /= "0" then framefail "vanilla-test: non-zero exit code from testee" fi
   $test_passed = check_stdout_ok() && check_stderr_ok()
}


-- finally, define pass/fail clauses
vanilla-test()
expect pass
pass when $test_passed
fail when otherwise



include ($confdir ++ "/" ++ $conffilename)

-----------------------------------------------------------------------
--- Stuff to do with simple single-source-file tests.  We assume    ---
--- that the name of the .T file is to be used as the basename      ---
--- for everything.                                                 ---
-----------------------------------------------------------------------

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
   rm_nofail(qualify("comp.stderr"))
   rm_nofail(qualify("comp.stdout"))
   rm_nofail(qualify("run.stderr"))
   rm_nofail(qualify("run.stdout"))
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

-- Compile testname.hs into testname; comp errors -> testname.comp.stdout.
-- Used for run tests, so framefail if compilation fails.
def simple_build_Main() 
{
   $flags = guess_compiler_flags()
   rm_or_fail(qualify("comp.stdout"))
   rm_or_fail(qualify(""))  -- ie, the exe name == the test name
   $cmd = $tool ++ " " ++ $flags 
          ++ " -o " ++ qualify("") ++ " " 
          ++ qualify("hs") 
          ++ " &> " ++ qualify("comp.stdout")
   $res = run $cmd
   if $res /= "0" then framefail "simple_build_Main: failed" fi
}


-- Just try to compile testname.hs to testname.o; no attempt at linking.
-- Is used for compile-only tests, so don't framefail if compilation
-- fails.
def simple_compile_Main() 
{
   $flags = guess_compiler_flags()
   rm_or_fail(qualify("comp.stdout"))
   rm_or_fail(qualify("o"))
   $cmd = $tool ++ " " ++ $flags 
          ++ " -c " ++ qualify("hs") 
          ++ " -o " ++ qualify("o") 
          ++ " &> " ++ qualify("comp.stdout")
   $res = run $cmd
   return $res
}


-- Run testname > testname.stdout 2> testname.stderr 
-- Returns the exit code of the run.
def simple_run_main_no_stdin ( )
{
   rm_or_fail(qualify("run.stdout"))
   rm_or_fail(qualify("run.stderr"))
   $cmd = qualify("") 
          ++ " > " ++ qualify("run.stdout")
          ++ " 2> " ++ qualify("run.stderr")
   $res = run $cmd
   return $res
}


-- Run testname < testname.stdin > testname.stdout 2> testname.stderr 
-- Returns the exit code of the run.
def simple_run_main ( )
{
   rm_or_fail(qualify("run.stdout"))
   rm_or_fail(qualify("run.stderr"))
   $cmd = qualify("") 
          ++ " < " ++ qualify("run.stdin")
          ++ " > " ++ qualify("run.stdout")
          ++ " 2> " ++ qualify("run.stderr")
   $res = run $cmd
   return $res
}

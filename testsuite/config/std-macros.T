
include ($confdir ++ "/" ++ $conffile)

def qualify ( $_filename )
{
   return $testdir ++ "/" ++ $_filename
}

def rm_or_fail ( $_files )
{
   $cmd = "rm -f " ++ $_files
   $res = run $cmd
   if $res /= "0" then framefail ("rm_or_fail: can't rm: " ++ $_files) fi
}

def rm_nofail ( $_files )
{
   $cmd = "rm -f " ++ $_files
   $res = run $cmd
}

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

-- Compile Main.hs into main; comp errors -> comp.stdout.
-- Used for run tests, so framefail if compilation fails.
def simple_build_Main() 
{
   $flags = guess_compiler_flags()
   rm_or_fail(qualify("comp.stdout"))
   rm_or_fail(qualify("Main.hi"))
   rm_or_fail(qualify("Main.o"))
   rm_or_fail(qualify("main"))
   $cmd = $tool ++ " " ++ $flags 
          ++ " -o " ++ qualify("main") ++ " " 
          ++ qualify("Main.hs") 
          ++ " &> " ++ qualify("comp.stdout")
   $res = run $cmd
   if $res /= "0" then framefail "simple_build_Main: failed" fi
}


-- Just try to compile Main.hs to Main.o; no attempt at linking.
-- Is used for compile-only tests, so don't framefail if compilation
-- fails.
def simple_compile_Main() 
{
   $flags = guess_compiler_flags()
   rm_or_fail(qualify("comp.stdout"))
   rm_or_fail(qualify("Main.hi"))
   rm_or_fail(qualify("Main.o"))
   $cmd = $tool ++ " " ++ $flags 
          ++ " -c " ++ qualify("Main.hs") 
          ++ " &> " ++ qualify("comp.stdout")
   $res = run $cmd
   return $res
}

-- Run main > run.stdout 2> run.stderr 
-- Returns the exit code of the run.
def simple_run_main_no_stdin ( )
{
   rm_or_fail(qualify("run.stdout"))
   rm_or_fail(qualify("run.stderr"))
   $cmd = qualify("main") 
          ++ " > " ++ qualify("run.stdout")
          ++ " 2> " ++ qualify("run.stderr")
   $res = run $cmd
   return $res
}


-- Run main < run.stdin > run.stdout 2> run.stderr 
-- Returns the exit code of the run.
def simple_run_main ( )
{
   rm_or_fail(qualify("run.stdout"))
   rm_or_fail(qualify("run.stderr"))
   $cmd = qualify("main") 
          ++ " < " ++ qualify("run.stdin")
          ++ " > " ++ qualify("run.stdout")
          ++ " 2> " ++ qualify("run.stderr")
   $res = run $cmd
   return $res
}



$diff = "diff -C 2"
$rm   = "rm -f"
$cp   = "cp"

-- -----------------------------------------------------------------------------
-- generic useful stuff

-- Gotta do some pretty basic stuff :)
def not ( $_bool )
{
        if $_bool == "True" then return False
   else if $_bool == "False" then return True
   else framefail ("not(): invalid input: " ++ $_bool )
   fi fi
}

def runCmd( $cmd )
{
   if defined $verbose then
	print $cmd
   fi
   $res = run $cmd
   return $res
}

def runCmdDontFail( $cmd )
{
   $res = runCmd($cmd)
   if $res /= "0"
	then framefail ("unexpected cmd failure: " ++ $cmd)
   fi
}

-- Delete a file and abort if that doesn't work.
def rm_or_fail ( $_files )
{
   $cmd = $rm ++ " " ++ $_files
   $res = runCmd($cmd)
   if $res /= "0" then framefail ("rm_or_fail: can't rm: " ++ $_files) fi
}

-- Delete a file but keep going antidisirregardless of the outcome.
def rm_nofail ( $_files )
{
   $cmd = $rm ++ " " ++ $_files
   $res = runCmd($cmd)
}

-----------------------------------------------------------------------------

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

-- "foo" -> qualify("foo-platform") if it exists, or qualify("foo") otherwise
def platform_qualify ( $_filename_frag )
{
   $name = qualify($_filename_frag)
   if   exists($name ++ "-" ++ $platform)
   then return $name ++ "-" ++ $platform
   else return $name
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

-- Pipe an error message through normalise_errmsg.
def normalise_errmsg ( $errmsg )
{
   $unpathify = $confdir ++ "/../../utils/normalise_errmsg/normalise_errmsg"
   $normd = $errmsg | $unpathify
   return $normd
}

-- returns True if both files are identical.
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

-- returns True if both files are identical when normalised (unpathified)
def same_normalised ( $_file1, $_file2 )
{
   if defined $verbose 
	then print "vanilla-test: comparing " ++ $_file1
		 ++ " and " ++ $_file2
   fi
   $cts1 = normalise_errmsg(contents($_file1))
   $cts2 = normalise_errmsg(contents($_file2))
   $same = $cts1 == $cts2
   if not($same) then 
	say_fail_because_noteq($_file1, $_file2) 
   fi
   return $same
}

-- Give hints as to why a test is failing.
def say_fail_because_noteq ( $filename1, $filename2 )
{
   print "--- FAIL because the following files differ:"
   print "---      " ++ $filename1
   print "---      " ++ $filename2
   if defined $accept then
	print "--- (accepting new output)"
	runCmdDontFail($cp ++ " " ++ $filename2 ++ " " ++ $filename1)
   fi
   if defined $verbose then
	$ignore = runCmd($diff ++ " " ++ $filename1 ++ " " ++ $filename2)
   fi
}

def say_fail_because_nonempty ( $filename1 )
{
   print "--- FAIL because the following file is non-empty:"
   print "---      " ++ $filename1
   print "--- contents:"
   print (contents $filename1)
   print ("--- end of " ++ $filename1)
}

def say_fail_because_exit_code_wrong ( $prg, $exit_code, $should_be )
{
   print "--- FAIL because $prog had the wrong exit code (" ++ 
	$exit_code ++ ", should be " ++ $should_be ++ ")"
}

def say_fail_because_compiler_barfd ( $res )
{
   print "--- FAIL because the compiler returned non-zero exit code = " ++ $res
   $comp_stderr = qualify("comp.stderr")
   if   exists($comp_stderr)
   then print "--- Error messages:"
        print contents(qualify("comp.stderr"))
   fi
}

---------------------------------------------------------------
--- RUNNING, AND ASSESSING RUN RESULTS                      ---
---------------------------------------------------------------

-- Run testname.  If testname.stdin exists, route input from that, else
-- from /dev/null.  Route output to testname.run.stdout and 
-- testname.run.stderr.  Returns the exit code of the run.

def simple_run_pgm( $extra_args, $exit_code )
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
	  ++ "./" ++ $testname ++ " " ++ $extra_args
          ++ " < " ++ $use_stdin
          ++ " > " ++ $run_stdout
          ++ " 2> " ++ $run_stderr

   -- run the command
   $res = runCmd($cmd)

   -- check the exit code
   if   $res /= $exit_code then
	say_fail_because_exit_code_wrong($testname, $res, $exit_code)
	return False
   fi

   -- check the stdout and stderr outputs
   $test_passed = check_stdout_ok() && check_stderr_ok()

   return $test_passed
}

-- Check that the run.stdout file matches either the .stdout-TARGETPLATFORM
-- (if it exists) or the .stdout otherwise.
def check_stdout_ok()
{
   $r_stdout  = qualify("run.stdout")
   $s_stdout  = platform_qualify("stdout")

   if   not ( exists($s_stdout) )
   then if ((contents $r_stdout) == "")
     then return True
     else say_fail_because_nonempty($r_stdout)
	  return False
     fi
   fi

   if   $normalise_output
   then return same_normalised($s_stdout, $r_stdout)
   else return same($s_stdout, $r_stdout)
   fi
}

-- Check that the run.stderr matches either the .stderr-TARGETPLATFORM
-- (if it exists) or the .stderr otherwise. Normalise the stderr if
-- $normalise_errmsg is set
def check_stderr_ok()
{
   $r_stderr  = qualify("run.stderr")
   $s_stderr  = platform_qualify("stderr")

   if   not ( exists($s_stderr) )
   then if ((contents $r_stderr) == "")
	   then return True
	   else say_fail_because_nonempty($r_stderr)
		return False
	fi
   fi

--   if   $normalise_errmsg
--   then
	return same_normalised($s_stderr, $r_stderr)
--   else return same($s_stderr, $r_stderr)
--   fi
}

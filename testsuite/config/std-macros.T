
$diff = "diff -c2"
$rm   = "rm -f"
$cp   = "cp"

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

def say_fail_because_compiler_barfd ( $res )
{
   print "--- FAIL because the compiler returned non-zero exit code = " ++ $res
   $comp_stderr = qualify("comp.stderr")
   if   exists($comp_stderr)
   then print "--- Error messages:"
        print contents(qualify("comp.stderr"))
   fi
}

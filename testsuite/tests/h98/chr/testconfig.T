
include ($confdir ++ "/../std-macros.T")
expect pass

pretest_cleanup()

simple_build_Main()
$res = simple_run_main_no_stdin()

pass when 
   contents("run.stdout") contains "True"

fail when otherwise


include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()

simple_build_Main()
$res = simple_run_main_no_stdin()

-- sigh
$stuff = contents("getDCs.run.stdout")
pass when 
   $stuff contains "bin" &&
   $stuff contains "lib" &&
   $stuff contains "include"

fail when otherwise

include ($confdir ++ "/../std-macros.T")
expect pass

pretest_cleanup()

simple_build_Main()
$res = simple_run_main_no_stdin()

pass when 
   $tool contains "ghc"
   && contents("run.stderr") contains "Prelude.chr: bad argument"

fail when otherwise
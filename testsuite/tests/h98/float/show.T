
include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()

simple_build_Main()
$res = simple_run_main_no_stdin()

pass when contents("show.run.stderr") contains "Prelude.read: no parse"

fail when otherwise
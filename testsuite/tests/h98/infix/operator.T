
include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()

simple_build_Main()
$res = simple_run_main_no_stdin()

pass when contents("operator.run.stdout") == contents("operator.expect.stdout")

fail when otherwise
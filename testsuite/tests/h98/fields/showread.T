
include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()

simple_build_Main()
$res = simple_run_main_no_stdin()

pass when contents("showread.run.stdout") == contents("showread.expect.stdout")

fail when otherwise
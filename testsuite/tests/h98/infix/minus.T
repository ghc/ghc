
include ($confdir ++ "/../singlefile-macros.T")
expect unknown	-- should this compile or not?

pretest_cleanup()

simple_build_Main()
$res = simple_run_main_no_stdin()

pass when contents("minus.run.stdout") == contents("minus.expect.stdout")

fail when otherwise
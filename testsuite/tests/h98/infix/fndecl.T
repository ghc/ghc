
include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()
simple_build_Main()
$res = simple_run_Main()

pass when contents("fndecl.run.stdout") == contents("fndecl.expected.stdout")

fail when otherwise

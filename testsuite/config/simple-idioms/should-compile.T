
include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()
$res = simple_compile_Main()
pass when contents("comp.stdout") == ""

fail when otherwise

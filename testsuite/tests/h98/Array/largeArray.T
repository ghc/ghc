
include ($confdir ++ "/../singlefile-macros.T")

test "largeArray" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout"))
             == contents(qualify("expect.stdout"))
   fail when otherwise
}

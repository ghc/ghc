
include ($confdir ++ "/../singlefile-macros.T")

test "edecls" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout")) 
             == contents(qualify("expect.stdout"))
   fail when otherwise
}
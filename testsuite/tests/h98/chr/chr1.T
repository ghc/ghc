
include ($confdir ++ "/../singlefile-macros.T")

test "chr1" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout")) 
             == contents(qualify("expected.stdout"))
   fail when otherwise
}


include ($confdir ++ "/../singlefile-macros.T")

test "show" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stderr")) 
             contains "Prelude.read: no parse"
   fail when otherwise
}

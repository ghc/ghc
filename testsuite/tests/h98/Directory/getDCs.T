
include ($confdir ++ "/../singlefile-macros.T")

test "getDCs" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   -- sigh
   $stuff = contents(qualify("run.stdout"))
   pass when 
      $stuff contains "bin" &&
      $stuff contains "lib" &&
      $stuff contains "include"
   fail when otherwise
}

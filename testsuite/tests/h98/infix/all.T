
include ($confdir ++ "/../singlefile-macros.T")

test "fndecl" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main()
   pass when contents(qualify("run.stdout"))
             == contents(qualify("expected.stdout"))
   fail when otherwise
}

test "infix" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout")) 
        == contents(qualify("expect.stdout"))
   fail when otherwise
}

test "local" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout"))
         == contents(qualify("expect.stdout"))
   fail when otherwise
}

test "minus" {
   expect unknown	-- should this compile or not?
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout")) 
             == contents(qualify("expect.stdout"))
   fail when otherwise
}

test "operator" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout")) 
        == contents(qualify("expect.stdout"))
   fail when otherwise
}

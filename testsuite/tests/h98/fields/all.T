
include ($confdir ++ "/../singlefile-macros.T")

test "empty" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout")) == "True\n"
   fail when otherwise
}

test "operator" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout"))
             == contents(qualify("expected.stdout"))
   fail when otherwise
}

test "puns" {
   expect pass
   pretest_cleanup()
   $res = simple_compile_Main()
   pass when 
      $tool contains "ghc"
      && contents(qualify("comp.stdout")) contains "parse error on input"
   pass when 
      $tool contains "nhc"
      && contents(qualify("comp.stdout")) contains "Named field puns"
   fail when otherwise
}

test "showread" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout")) 
             == contents(qualify("expect.stdout"))
   fail when otherwise
}

test "type" {
   expect pass
   pretest_cleanup()
   simple_build_Main()
   $res = simple_run_main_no_stdin()
   pass when contents(qualify("run.stdout")) 
             == contents(qualify("expect.stdout"))
   fail when otherwise
}

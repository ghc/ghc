
include ($confdir ++ "/../singlefile-macros.T")

test "qual" {
   expect pass
   pretest_cleanup()
   $res = simple_compile_Main()
   pass when contents(qualify("comp.stdout")) == ""
   fail when otherwise
}

test "twiddle" {
   expect pass
   pretest_cleanup()
   $res = simple_compile_Main()
   pass when contents(qualify("comp.stdout")) == ""
   fail when otherwise
}
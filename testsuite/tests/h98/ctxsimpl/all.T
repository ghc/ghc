
include ($confdir ++ "/../singlefile-macros.T")

test "ok1" {
   expect pass
   pretest_cleanup()
   $res = simple_compile_Main()
   pass when contents(qualify("comp.stdout")) == ""
   fail when otherwise
}


test "bad1" {
   expect pass
   pretest_cleanup()
   $res = simple_compile_Main()
   pass when 
      $tool contains "ghc"
      && contents(qualify("comp.stdout")) contains "Could not deduce"
   pass when 
      $tool contains "nhc"
      && contents(qualify("comp.stdout")) contains "Couldn't simplify the context"
   fail when otherwise
}

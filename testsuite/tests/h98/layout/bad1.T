
include ($confdir ++ "/../singlefile-macros.T")

test "bad1" {
   expect pass
   pretest_cleanup()
   $res = simple_compile_Main()
   pass when 
      $tool contains "ghc"
      && contents(qualify("comp.stdout")) contains "parse error"
      && contents(qualify("comp.stdout")) contains "possibly incorrect indentation"
   pass when 
      $tool contains "nhc"
      && contents(qualify("comp.stdout")) contains "Found {-end-of-definition-} but"
   fail when otherwise
}
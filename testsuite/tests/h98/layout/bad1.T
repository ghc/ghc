
include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()
$res = simple_compile_Main()

pass when 
   $tool contains "ghc"
   && contents("bad1.comp.stdout") contains "parse error"
   && contents("bad1.comp.stdout") contains "possibly incorrect indentation"

pass when 
   $tool contains "nhc"
   && contents("bad1.comp.stdout") contains "Found {-end-of-definition-} but"

fail when otherwise


include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()
$res = simple_compile_Main()

pass when 
   $tool contains "ghc"
   && contents("bad1.comp.stdout") contains "Could not deduce"

pass when 
   $tool contains "nhc"
   && contents("bad1.comp.stdout") contains "Couldn't simplify the context"

fail when otherwise

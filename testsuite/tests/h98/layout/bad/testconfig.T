
include ($confdir ++ "/../std-macros.T")
expect pass

pretest_cleanup()
$res = simple_compile_Main()

pass when 
   $tool contains "ghc"
   && contents("comp.stdout") contains "Could not deduce"

pass when 
   $tool contains "nhc"
   && contents("comp.stdout") contains "Found {-end-of-definition-} but"

fail when otherwise

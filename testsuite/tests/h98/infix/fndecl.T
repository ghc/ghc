
include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()
$res = simple_compile_Main()

pass when 
   $tool contains "ghc"
   && contents("fndecl.comp.stdout") contains "Ambiguous type variable"

-- put a pass clause here for NHC

fail when otherwise

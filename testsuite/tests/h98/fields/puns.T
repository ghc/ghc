
include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()
$res = simple_compile_Main()

pass when 
   $tool contains "ghc"
   && contents("puns.comp.stdout") contains "parse error on input"

-- put a pass clause here for NHC

fail when otherwise


include ($confdir ++ "/../singlefile-macros.T")
expect pass

pretest_cleanup()
$res = simple_compile_Main()

pass when 
   $tool contains "ghc"
   && contents("bad1.comp.stdout") contains "parse error"
   && contents("bad1.comp.stdout") contains "possibly incorrect indentation"

-- put a pass clause here for NHC

fail when otherwise

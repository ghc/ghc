
include ($confdir ++ "/../std-macros.T")
expect pass

pretest_cleanup()
$res = simple_compile_Main()

pass when 
   $tool contains "ghc"
   && contents("comp.stdout") contains "parse error on input"

pass when 
   $tool contains "nhc"
   && contents("comp.stdout") contains "Named field puns"

fail when otherwise

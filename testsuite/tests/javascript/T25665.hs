-- #25665: link a package whose archive contains a wasm object (built from a C
-- source) followed by archive padding. In the Darwin format (the default of
-- llvm-ar on Darwin hosts), llvm-ar adds '\n' bytes after each member and
-- includes them in the member size. The Makefile adds this padding by hand,
-- thus the test does not depend on llvm-ar.
--
-- See Note [Wasm objects in Darwin archives] in GHC.StgToJS.Object
import T25665Lib

main :: IO ()
main = print =<< t25665 41

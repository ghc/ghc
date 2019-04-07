import Data.Map

-- If someone improves the specializer so that
-- GHC no longer misses the specialization below,
-- then this test will fail, as it expects a warning
-- to be issued.
-- Another reason this could fail is due to spelling:
-- the test checks for the "specialisation" spelling,
-- but due to changes in how the warnings are listed in DynFalgs.hs
-- the compiler may spit out the "specialization" spelling.
main :: IO ()
main = do
  let m = [] :: [Map String Bool]
  mapM_ print m

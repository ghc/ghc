
import Test.QuickCheck.Batch

prop_silly :: [()] -> Bool
prop_silly xs = head xs == head xs 

do_test = runTests "test" defOpt [ run prop_silly ]

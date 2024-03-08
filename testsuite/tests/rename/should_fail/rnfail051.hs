-- This test used to check that the renamer properly rejected view patterns in
-- expressions contexts (#2033).
--
-- Due to GHC Proposal #281, (e1 -> e2) is now assumed to stand for a function
-- type, not a view pattern, and the user has to enable RequiredTypeArguments to
-- use this syntax.
--
-- As a consequence, the error message emitted by this test case now talks about
-- RequiredTypeArguments rather than ViewPatterns.

module RnFail051 where

main :: IO ()
main = wrapper (_ -> putStrLn "_")

wrapper :: (String -> IO ()) -> IO ()
wrapper f = f ""

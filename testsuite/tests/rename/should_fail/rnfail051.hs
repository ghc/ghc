-- trac #2033: This used to fail when the renamer didn't check for a view
-- /pattern/ being used in an /expression/ context

module RnFail051 where

main :: IO ()
main = wrapper (_ -> putStrLn "_")

wrapper :: (String -> IO ()) -> IO ()
wrapper f = f ""


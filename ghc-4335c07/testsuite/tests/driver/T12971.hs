-- Test that setting the TMP environment variable to a path with non-ASCII
-- characters works.
main :: IO ()
main = putStrLn "hello world"

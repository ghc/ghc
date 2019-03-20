import System.Environment

-- Make sure that getExecutablePath works on Windows. See #16466.
main :: IO ()
main = () <$ getExecutablePath


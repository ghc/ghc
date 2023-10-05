import Data.Typeable

data Ω = Ω

main :: IO ()
main = print $ typeOf Ω

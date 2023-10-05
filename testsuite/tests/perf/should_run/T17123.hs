import qualified Data.Text as T
import Data.Foldable

xs :: [T.Text]
xs = [ T.pack $ show n | n <- [0..10000] ]

main :: IO ()
main = do
    fold xs `seq` return ()

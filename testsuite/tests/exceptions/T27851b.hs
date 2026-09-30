import Control.Exception

newtype Wrapped = Wrapped String deriving Show
instance Exception Wrapped

main :: IO ()
main = print (mapException (\(ErrorCall s) -> Wrapped s) (error "boom" :: Int))

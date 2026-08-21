import Control.Exception

main = mask $ \restore -> restore (pure ())


import Control.Applicative
main = show <$> pure 1 >>= print

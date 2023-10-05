import Control.Applicative
main = (pure 1 >>= print) <**> pure show

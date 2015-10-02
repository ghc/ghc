import Control.Exception
import Control.DeepSeq
main = evaluate (('a' : undefined) `deepseq` return () :: IO ())

module T18227A (kilter) where
import Data.ByteString.Internal

kilter :: ByteString -> IO ByteString
kilter ps@(PS x _ _) = createAndTrim 1 $ const $ pure 1


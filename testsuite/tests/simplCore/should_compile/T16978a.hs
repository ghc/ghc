-- | Caused Core Lint failure due to floating of unlifted join point to the
-- top-level.
module Bug where

import Control.Monad.Trans.State.Strict (State, modify')
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as B

data Value = String !Text | Null

type Render a = State B.Builder a

tellBuilder :: B.Builder -> Render ()
tellBuilder b' = modify' f where
    f b = b <> b'

renderNode :: Value -> Render ()
renderNode v =
  renderValue v >>= outputRaw

outputRaw :: Text -> Render ()
outputRaw = tellBuilder . B.fromText
{-# INLINE outputRaw #-}

renderValue :: Value -> Render Text
renderValue v = case v of
    String str -> return str
    _ -> let x = x in x
{-# INLINE renderValue #-}

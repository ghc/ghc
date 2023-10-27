{-# LANGUAGE MagicHash #-}

import GHC.Exts
import Debug.Trace
import GHC.IO
import GHC.ST

data StrictPair a b = !a :*: !b

strictFun :: Int -> Int
{-# OPAQUE strictFun #-}
strictFun x = x*x*x

opaqueId :: a -> a
{-# OPAQUE opaqueId #-}
{-# RULES
  "opaqueId/noinline" opaqueId = noinline
#-}
-- work around noinline's special desugaring
opaqueId v = v

evaluateST :: a -> ST s a
-- hide the fact that we are actually in IO because !11515
-- causes seq# to look like it can throw precise exceptions
evaluateST x = ST (\s -> seq# x s)

fun :: Int -> Int -> ST s Int
{-# OPAQUE fun #-}
fun = lazy $ \ !x y -> do
  -- This should evaluate x before y.
  _ <- evaluateST $ opaqueId (x :*: x)
  _ <- evaluateST y
  evaluateST $! strictFun x

main :: IO ()
main = () <$ stToIO (fun (trace "x eval'd" 12) (trace "y eval'd" 13))

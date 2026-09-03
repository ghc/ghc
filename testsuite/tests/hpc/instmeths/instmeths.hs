import Data.List
import Data.Maybe
import Trace.Hpc.Mix
import Trace.Hpc.Reflect
import Trace.Hpc.Tix
import Trace.Hpc.Util

class Foo a where
  defMeth, defMeth', reqMeth :: a -> String
  defMeth = const "class default"
  defMeth' = const "class default"

newtype T a = T a deriving (Show, Eq, Functor)
instance Foo (T a) where
  reqMeth = const "Foo (T a) instance"

newtype T' a = T' a

instance Foo (T' a) where
  reqMeth = const "Foo (T' a) instance"
  defMeth = const "Foo (T' a) instance"

-- no method use could "cover" this instance, so we (for now) omit the top-level box for it
class Marker a
instance Marker (T Bool)

interesting :: (Int, MixEntry) -> Maybe (Int, HpcPos, String)
interesting (n, (pos, TopLevelBox [name])) | isInteresting name = Just (n, pos, name)
  where isInteresting = (||) <$> isInst <*> isMeth
        isInst = ("$f" `isPrefixOf`)
        isMeth = (||) <$> (`elem` ["(==)", "(/=)", "show", "showList", "showsPrec", "fmap", "(<$)"])
                      <*> ("Meth" `isSubsequenceOf`)
interesting _ = Nothing

-- candidate for HPC.Utils, maybe?
sourceAt :: String -> HpcPos -> String
sourceAt src pos
  | l1 == l2 = take (c2 - c1 + 1) . drop (c1 - 1) $ ls !! (l1 - 1)
  | otherwise = intercalate "\n" $ first : (middle ++ [last])
  where
    (l1, c1, l2, c2) = fromHpcPos pos
    ls = lines src
    first = drop (c1 - 1) $ ls !! (l1 - 1)
    middle = take (l2 - l1 - 1) $ drop l1 ls
    last = take c2 $ ls !! (l2 - 1)

main :: IO ()
main = do
  print (T 23 == (succ <$> T 22)) -- tick Eq and Functor by using any of their methods
  print (reqMeth (T' 1)) -- tick `Foo (T' a)` and that instance's `reqMeth`

  Mix source _ _ _ mixEntries <- readMix [".hpc"] (Left "Main")
  src <- readFileUtf8 source
  let boxes = mapMaybe interesting $ zip [0 ..] mixEntries
  Tix [TixModule "Main" _ _ counts] <- examineTix
  mapM_ print [ (counts !! n, path, sourceAt src pos)
              | (n, pos, path) <- sortOn (\(_, _, s) -> s) boxes ]

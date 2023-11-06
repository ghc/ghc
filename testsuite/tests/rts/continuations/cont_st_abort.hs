
import ContST
import Data.STRef
import Control.Monad.ST

type HasAbort s a = PromptTag s (Maybe a)

abort :: HasAbort s a -> ST s a
abort tag = control0 tag (\ _c -> pure Nothing)

withAbort :: forall s a . (HasAbort s a -> ST s a) -> ST s (Maybe a)
withAbort k = do
    tag <- newPromptTag :: ST s (PromptTag s (Maybe a))
    prompt tag $ do
        a <- k tag
        pure (Just a)


eg1 :: forall s. ST s String -> ST s () -> ST s (Maybe Bool)
eg1 m n = withAbort $ \tag ->
  let go :: ST s Bool
      go = do t <- m
              case t of
                  "abort" -> abort tag
                  "true"  -> pure True
                  _       -> go <* n
  in go

runEg1 :: String -> (Maybe Bool, Int)
runEg1 input = runST $ do
    ref <- newSTRef input
    cnt <- newSTRef (0 :: Int)

    let m = do
            t <- readSTRef ref
            case t of
                []   -> return []
                _:t' -> do
                    writeSTRef ref t'
                    return t'

    let n = modifySTRef' cnt (1 +)

    res <- eg1 m n
    val <- readSTRef cnt
    return (res, val)

main :: IO ()
main = do
    print $ runEg1 "...abort"
    print $ runEg1 "...true"

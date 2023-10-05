import ContIO

main :: IO ()
main = do
  tag1 <- newPromptTag
  tag2 <- newPromptTag
  prompt tag1 $
    control0 tag2 $ \k -> -- should error: no such prompt on the stack!
      k (pure ())

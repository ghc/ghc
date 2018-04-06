{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

import T13688Quasi

pattern A = [aQuoter|hello world|]

pattern B <- [aQuoter|hello world|]
  where B = [aQuoter|hello world|]

main :: IO ()
main = do
    print A
    case "hello world" of
      A -> putStrLn "good"
      _ -> putStrLn "bad"

    print B
    case "hello world" of
      B -> putStrLn "good"
      _ -> putStrLn "bad"

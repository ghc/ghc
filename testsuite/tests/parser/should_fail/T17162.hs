-- {-# LANGUAGE NoBangPatterns #-}

module T17162 where

charIsRepresentable :: TextEncoding -> Char -> IO Bool
charIsRepresentable !enc c =
  withCString enc [c]
              (\cstr -> do str <- peekCString enc cstr
                           case str of
                             [ch] | ch == c -> pure True
                             _ -> pure False)
    `catch`
       \(_ :: IOException) -> pure False

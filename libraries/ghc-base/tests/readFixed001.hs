
import Data.Fixed

main :: IO ()
main = do f "  (( (  12.3456  ) )  )  "
          f "  (( (  12.3     ) )  )  "
          f "  (( (  12.      ) )  )  "
          f "  (( (  12       ) )  )  "
          f "  (( - (  12.3456  ) )  )  "
          f "  (( (  -12.3456  ) )  )  "

f :: String -> IO ()
f str = print (reads str :: [(Centi, String)])

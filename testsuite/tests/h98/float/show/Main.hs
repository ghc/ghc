import Numeric

main = do
  s <- return ""
  putStrLn (showFloat ((read s)::Float) "")

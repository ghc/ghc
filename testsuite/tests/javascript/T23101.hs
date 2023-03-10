
foreign import javascript "(($1) => { return $1; })"
  bool_id :: Bool -> Bool

foreign import javascript "(($1) => { return !$1; })"
  bool_not :: Bool -> Bool

foreign import javascript "(($1) => { console.log($1); })"
  bool_log :: Bool -> IO ()

main :: IO ()
main = do
  bool_log True
  bool_log False
  bool_log (bool_id True)
  bool_log (bool_id False)
  bool_log (bool_not True)
  bool_log (bool_not False)
  print (bool_id True)
  print (bool_id False)
  print (bool_not True)
  print (bool_not False)

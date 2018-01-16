-- !!! tests for large character values in literals
import Data.Char
main = do
  print (ord '\xffff')
  print (ord '\o7777')
  print (ord '\65535')
  print (map ord "\xffff\o7777\65535")

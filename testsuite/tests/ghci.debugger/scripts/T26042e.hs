module T7 where

-- f acc [] = acc
-- f acc (x:xs) =
--   let !y = x+1
--    in f (y:acc) xs

a = do
  let !y = b
  let !x = ['a', 'b', 'c']
  let !z = y x
  let !t = y ['b']
  (z, t)

b = length

main = do
  let !(x, y) = a
  print '1'
  print '2'
  print '3'
  print x
  print y


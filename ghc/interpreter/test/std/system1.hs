--!!! Testing System
module T where

import System(getArgs,getProgName,getEnv,system)

-- like print but no annoying "\n"
pr :: Show a => a -> IO ()
pr = putStr . show

test1 = system "exit 0" >>= pr
test2 = system "exit 1" >>= pr
test3 = system "exit 2" >>= pr

test4 = getArgs        >>= pr
test5 = getProgName    >>= pr

-- We want to test getEnv - but there's too much variety in possible 
-- environments so we pick an env var that doesn't vary too much
-- and list every variation we've ever come across.
test6 = do
  shell <- getEnv "SHELL"
  let sh = last $ chop '/' shell
  if (sh `elem` shells) 
    then
      putStr "getEnv \"SHELL\" returns known shell"
    else
      putStr "getEnv \"SHELL\" returns unknown shell"
  return ()
 where
  shells = ["sh" 
           ,"csh"
           ,"tcsh"
           ,"bash"
	   ,"zsh"
           ]

chop :: Eq a => a -> [a] -> [[a]]
chop seq [] = []
chop sep xs = ys : case zs of 
                   []    -> []
                   _:zs' -> chop sep zs'
 where
  (ys,zs) = break (sep ==) xs

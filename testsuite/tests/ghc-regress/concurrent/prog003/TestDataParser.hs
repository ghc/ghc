
module TestDataParser (
   parse_testdata, -- Read a => FilePath -> IO (TestData a)
   write_testdata  -- Show a => TestData -> IO ()
) where

import TestData

-- Parser for parsing TestData. The following is a sample of a test data: 
--
--
-- Name: sample
-- Threads: 4
-- Modes: ["CAS","IO","BACK"]
-- Repeat: 6
-- Initial-List: 
-- [4,5,6,7,8,4,5,754,345,23432,6547,4]
-- Tasks: 
-- [Find 3,Delete 4,Insert 34,Find 45]
-- [Delete 34,Insert 43,Delete 3,Delete 45,Find 87]
-- [Insert 3 , Find 6]
-- [Find 3,Find 67,  Insert 3]
--
--
-- Some assumptions:
--   - Number of tasks must correspond to number of threads.
--   - Fields must come in the exact sequence specified above.

-- Auxiliary Functions

partitionAt :: Eq a => (a -> Bool) -> [a] -> [[a]]
partitionAt f as = 
  filter (/=[]) (partitionAt' f as)
  where
    partitionAt' _ [] = []
    partitionAt' f as = let (v,rest) = span f as
                        in v:(partitionAt' f (drop 1 rest))
              
is_delimit :: Char -> Bool
is_delimit ' '  = True
is_delimit '\n' = True
is_delimit _    = False
              
not_delimit :: Char -> Bool
not_delimit x = not (is_delimit x)

-- Parsing Functions

parse_token :: String -> (String,String)
parse_token str = let str' = dropWhile is_delimit str 
                  in span not_delimit str'

parse_list :: String -> ([String],String)
parse_list str = 
   let str' = dropWhile is_delimit str
   in parse_list (tail str')
   where
     parse_list str = let comma x = x == ','
                          close x = x == ']'
                          commaOrClose x = or [comma x,close x]
                          (a,rest)   = span (\x -> not $ commaOrClose x) str
                      in case head rest of
                           ',' -> let (as,rest') = parse_list (tail rest)
                                  in (a:as,rest')
                           ']' -> ([a],tail rest)                         

parse_list_many :: Int -> String -> ([[String]],String)
parse_list_many 0 str = ([],str)
parse_list_many n str = let (a,rest)   = parse_list str
                            (as,rest') = parse_list_many (n-1) rest
                        in (a:as,rest')
                    
drop_token :: String -> String
drop_token str = let (_,rest) = parse_token str
                 in rest              
      
-- Main function for parseing testcases from file              
parse_testdata :: Read a => FilePath -> IO (TestData a)
parse_testdata fname = do
  { input <- readFile fname
  ; let (name,input1) = parse_token (drop_token input)
        (t,input2)    = parse_token (drop_token input1)
        (list,input3) = parse_list (drop_token input2)
        (ops,_)       = parse_list_many (read t) (drop_token input3)
  ; return $ TestData { t_name    = name
                      , t_threads = read t
                      , t_init_list = map read list
                      , t_tasks   = map readops ops } }
  where
    readops (s:ss) = let (oper,rest) = parse_token s
                         (arg,_)     = parse_token rest
                         x  = read arg
                         op = case oper of
                               "Find"   -> Find x
                               "Delete" -> Delete x
                               "Insert" -> Insert x
                               _        -> error ("parse_testdata: " ++ oper ++ "\n" ++ rest ++ "\n" ++ arg)
                     in op:(readops ss) 
    readops [] = []
    
-- Main function for writing test data to file
write_testdata :: Show a => TestData a -> IO ()
write_testdata tc = writeFile (t_name tc) (show tc) 

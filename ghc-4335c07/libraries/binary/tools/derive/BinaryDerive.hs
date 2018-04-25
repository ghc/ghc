{-# LANGUAGE ScopedTypeVariables #-}

module BinaryDerive where

import Data.Generics
import Data.List

deriveM ::  (Typeable a, Data a) => a -> IO ()
deriveM (a :: a) = mapM_ putStrLn . lines $ derive (undefined :: a)

derive :: (Typeable a, Data a) => a -> String
derive x = 
    "instance " ++ context ++ "Binary " ++ inst ++ " where\n" ++
    concat putDefs ++ getDefs
    where
    context
        | nTypeChildren > 0 =
            wrap (join ", " (map ("Binary "++) typeLetters)) ++ " => "
        | otherwise = ""
    inst = wrap $ tyConName typeName ++ concatMap (" "++) typeLetters
    wrap x = if nTypeChildren > 0 then "("++x++")" else x 
    join sep lst = concat $ intersperse sep lst
    nTypeChildren = length typeChildren
    typeLetters = take nTypeChildren manyLetters
    manyLetters = map (:[]) ['a'..'z']
    (typeName,typeChildren) = splitTyConApp (typeOf x)
    constrs :: [(Int, (String, Int))]
    constrs = zip [0..] $ map gen $ dataTypeConstrs (dataTypeOf x)
    gen con = ( showConstr con
              , length $ gmapQ undefined $ fromConstr con `asTypeOf` x
              )
    putDefs = map ((++"\n") . putDef) constrs
    putDef (n, (name, ps)) =
        let wrap = if ps /= 0 then ("("++) . (++")") else id
            pattern = name ++ concatMap (' ':) (take ps manyLetters)
        in
        "  put " ++ wrap pattern ++" = "
        ++ concat [ "putWord8 " ++ show n | length constrs  > 1 ]
        ++ concat [ " >> "                | length constrs  > 1 && ps  > 0 ]
        ++ concat [ "return ()"           | length constrs == 1 && ps == 0 ]
        ++ join " >> " (map ("put "++) (take ps manyLetters))
    getDefs =
       (if length constrs > 1
            then "  get = do\n    tag_ <- getWord8\n    case tag_ of\n"
            else "  get =")
        ++ concatMap ((++"\n")) (map getDef constrs) ++
       (if length constrs > 1
	    then "      _ -> fail \"no decoding\""
	    else ""
       )
    getDef (n, (name, ps)) =
        let wrap = if ps /= 0 then ("("++) . (++")") else id
        in
        concat [ "      " ++ show n ++ " ->" | length constrs > 1 ]
        ++ concatMap (\x -> " get >>= \\"++x++" ->") (take ps manyLetters)
        ++ " return "
        ++ wrap (name ++ concatMap (" "++) (take ps manyLetters))

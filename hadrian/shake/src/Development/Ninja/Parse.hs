{-# LANGUAGE RecordWildCards, TupleSections #-}

module Development.Ninja.Parse(parse) where

import qualified Data.ByteString.Char8 as BS
import Development.Ninja.Env
import Development.Ninja.Type
import Development.Ninja.Lexer
import Control.Monad
import General.Extra


parse :: FilePath -> Env Str Str -> IO Ninja
parse file env = parseFile file env newNinja


parseFile :: FilePath -> Env Str Str -> Ninja -> IO Ninja
parseFile file env ninja = do
    lexes <- lexerFile $ if file == "-" then Nothing else Just file
    foldM (applyStmt env) ninja{sources=file:sources ninja} $ withBinds lexes

withBinds :: [Lexeme] -> [(Lexeme, [(Str,Expr)])]
withBinds [] = []
withBinds (x:xs) = (x,a) : withBinds b
    where
        (a,b) = f xs
        f (LexBind a b : rest) = let (as,bs) = f rest in ((a,b):as, bs)
        f xs = ([], xs)


applyStmt :: Env Str Str -> Ninja -> (Lexeme, [(Str,Expr)]) -> IO Ninja
applyStmt env ninja@Ninja{..} (key, binds) = case key of
    LexBuild outputs rule deps -> do
        outputs <- mapM (askExpr env) outputs
        deps <- mapM (askExpr env) deps
        binds <- mapM (\(a,b) -> (a,) <$> askExpr env b) binds
        let (normal,implicit,orderOnly) = splitDeps deps
        let build = Build rule env normal implicit orderOnly binds
        pure $
            if rule == BS.pack "phony" then ninja{phonys = [(x, normal ++ implicit ++ orderOnly) | x <- outputs] ++ phonys}
            else if length outputs == 1 then ninja{singles = (headErr outputs, build) : singles}
            else ninja{multiples = (outputs, build) : multiples}
    LexRule name ->
        pure ninja{rules = (name, Rule binds) : rules}
    LexDefault xs -> do
        xs <- mapM (askExpr env) xs
        pure ninja{defaults = xs ++ defaults}
    LexPool name -> do
        depth <- getDepth env binds
        pure ninja{pools = (name, depth) : pools}
    LexInclude expr -> do
        file <- askExpr env expr
        parseFile (BS.unpack file) env ninja
    LexSubninja expr -> do
        file <- askExpr env expr
        e <- scopeEnv env
        parseFile (BS.unpack file) e ninja
    LexDefine a b -> do
        addBind env a b
        pure ninja
    LexBind a _ ->
        error $ "Ninja parsing, unexpected binding defining " ++ BS.unpack a


splitDeps :: [Str] -> ([Str], [Str], [Str])
splitDeps (x:xs) | x == BS.pack "|" = ([],a++b,c)
                 | x == BS.pack "||" = ([],b,a++c)
                 | otherwise = (x:a,b,c)
    where (a,b,c) = splitDeps xs
splitDeps [] = ([], [], [])


getDepth :: Env Str Str -> [(Str, Expr)] -> IO Int
getDepth env xs = case lookup (BS.pack "depth") xs of
    Nothing -> pure 1
    Just x -> do
        x <- askExpr env x
        case BS.readInt x of
            Just (i, n) | BS.null n -> pure i
            _ -> error $ "Ninja parsing, could not parse depth field in pool, got: " ++ BS.unpack x

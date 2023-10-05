{-# LANGUAGE DataKinds, TemplateHaskell #-}

applicate :: Bool -> [Stmt] -> ExpQ
applicate rawPatterns stmt = do
    return $ foldl (\g e -> VarE '(<**>) `AppE` e `AppE` g)
                    (VarE 'pure `AppE` f')
                    es

tuple :: Int -> ExpQ
tuple n = do
    ns <- replicateM n (newName "x")
    lamE [foldr (\x y -> conP '(:) [varP x,y]) wildP ns] (tupE $ map varE ns)

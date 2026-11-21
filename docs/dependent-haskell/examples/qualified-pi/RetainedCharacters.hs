{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module RetainedCharacters
    ( RememberedChar, remember, rememberedChar
    , RememberedList, rememberList, rememberedList
    ) where

-- | Store a character and index its type by the same argument.
data RememberedChar (c :: Char) where
    RememberedChar :: Char -> RememberedChar c

-- | Keep the ordinary argument and retain its character index.
{-# NOINLINE remember #-}
remember :: foreach (c :: Char) -> RememberedChar c
remember c = RememberedChar c

-- | Read the stored character.
rememberedChar :: RememberedChar c -> Char
rememberedChar (RememberedChar c) = c

-- | Index a stored list by its complete argument.
data RememberedList a (xs :: [a]) where
    RememberedList :: [a] -> RememberedList a xs

-- | Keep the list and its index without inspecting the elements.
{-# NOINLINE rememberList #-}
rememberList :: forall a. foreach (xs :: [a]) -> RememberedList a xs
rememberList xs = RememberedList xs

-- | Read the stored list.
rememberedList :: RememberedList a xs -> [a]
rememberedList (RememberedList xs) = xs

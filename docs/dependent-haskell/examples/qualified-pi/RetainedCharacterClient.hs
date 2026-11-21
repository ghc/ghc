{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module RetainedCharacterClient (observeCharacters) where

import RetainedCharacters

-- | Read a literal, a list of literals, and the head of a list with a lazy field.
observeCharacters :: Char -> (Int, [Int], [Int])
observeCharacters later =
    ( fromEnum (rememberedChar (remember 'λ' :: RememberedChar 'λ'))
    , map fromEnum (rememberedList (rememberList ['λ', '🙂']))
    , map fromEnum (take 1 (rememberedList (rememberList ['λ', later])))
    )

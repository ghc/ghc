{-# LANGUAGE FlexibleInstances, NoExperimentalDependentTypes #-}

-- | Preview a stream through its exported interface.
module StreamPreview (Preview(preview)) where

import ErasedStream (Stream, toList)

-- | Read a short prefix for display.
class Preview a where
    preview :: a -> String

instance Preview Char where
    preview x = [x]

instance Preview (Stream Char) where
    preview xs = take 2 (toList xs)

instance Preview (Stream Bool) where
    preview xs = show (take 1 (toList xs))

-- | An input can be an error or a stream of characters.
instance Preview err => Preview (Either err (Stream Char)) where
    preview (Left err) = preview err
    preview (Right xs) = preview xs

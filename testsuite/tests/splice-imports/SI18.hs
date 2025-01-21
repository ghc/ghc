{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitStagePersistence #-}
module SI18 where

-- Variable quotes for a locally bound identifier fails
boo a = 'a

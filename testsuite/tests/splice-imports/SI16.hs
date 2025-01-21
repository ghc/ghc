{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitStagePersistence #-}
module SI16 where

x = ()

-- Testing variable quotes for top-level local definitions, disallowed with
-- NoImplicitStagePersistnece

foo = 'x



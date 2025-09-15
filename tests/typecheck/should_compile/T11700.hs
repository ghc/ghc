{-# LANGUAGE GADTs, TypeFamilies #-} -- Remove this line and the code compiles.

module T11700 where

data Muse
data Message
data Folder

class PersistEntity record

data Entity record where
  Entity :: PersistEntity record => record -> Entity record

fn1 :: (Entity Muse, Entity Message) -> Message
fn1 cluster = let (Entity foo, Entity msg) = cluster
              in msg
-- fn1 (Entity foo, Entity msg) = msg


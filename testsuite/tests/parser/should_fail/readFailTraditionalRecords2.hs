
{-# LANGUAGE NoTraditionalRecordSyntax #-}

module ReadFailTraditionalRecords2 where

f (Foo { i = j }) = j


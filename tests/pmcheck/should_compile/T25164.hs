{-# LANGUAGE TemplateHaskell #-}

module T25164 where

import T25164_aux ( genDoBlock )

$( genDoBlock )

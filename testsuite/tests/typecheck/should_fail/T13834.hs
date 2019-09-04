{-# LANGUAGE TypeApplications #-}

module Bug where

foo = notInScope @Bool True


{-# LANGUAGE TemplateHaskell #-}

module T8987 where
import Language.Haskell.TH

$(reportWarning ['1', undefined] >> return [])
module T27182 ( oneM ) where

data Parser = BindP Int ( Char -> Bool )

oneM :: Int -> ( Char -> Bool ) -> Parser
oneM p = BindP p

module Hello (hello) where

import Test

hello :: String
hello = "Imported from dependency 'test':" <> show test

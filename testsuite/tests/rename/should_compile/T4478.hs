
-- We don't want to warn about duplicate exports for things exported
-- by both "module" exports
module T4478 (module Prelude, module Data.List) where

import Prelude
import Data.List

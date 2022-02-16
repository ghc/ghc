module GHC.Driver.Config.StgToCmm
where

data PrimitiveImplementation
    = LlvmPrimitives
    | NcgPrimitives
    | GenericPrimitives

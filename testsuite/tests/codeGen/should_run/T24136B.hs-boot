module T24136B where

-- Declared abstractly: an hs-boot interface cannot carry the LambdaFormInfo of
-- a value binding, so any module importing mkT through this boot interface sees
-- it as an unknown closure and references it with tag 0.  The defining module
-- must therefore keep a static indirection for mkT (rather than aliasing the
-- symbol straight to the constructor), so that such an untagged enter lands on
-- stg_IND_STATIC and is handed back the properly tagged indirectee.
data T

mkT :: T

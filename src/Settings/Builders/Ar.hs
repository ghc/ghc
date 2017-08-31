module Settings.Builders.Ar (arBuilderArgs) where

import Settings.Builders.Common

arBuilderArgs :: Args
arBuilderArgs = builder Ar ? mconcat [ arg "q"
                                     , arg =<< getOutput
                                     , getInputs ]

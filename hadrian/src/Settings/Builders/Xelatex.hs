module Settings.Builders.Xelatex (xelatexBuilderArgs) where

import Settings.Builders.Common

xelatexBuilderArgs :: Args
xelatexBuilderArgs = builder Xelatex ? mconcat [ arg "-halt-on-error"
                                               , arg =<< getInput ]

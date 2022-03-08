module Settings.Builders.Xelatex (xelatexBuilderArgs) where

import Settings.Builders.Common

xelatexBuilderArgs :: Args
xelatexBuilderArgs = mconcat
    [ builder Xelatex ? mconcat [ arg "-halt-on-error"
                                , arg =<< getInput ]

      -- this fixes #20913 although is a bit of a hack given that this is
      -- specific to the user-guide.
    , builder Makeindex ? mconcat [ arg "-s", arg "python.ist" ]
    ]

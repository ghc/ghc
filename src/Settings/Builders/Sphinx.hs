module Settings.Builders.Sphinx (sphinxBuilderArgs) where

import Settings.Builders.Common

sphinxBuilderArgs :: Args
sphinxBuilderArgs = do
    outPath <- getOutput
    mconcat [ builder (Sphinx Html) ? mconcat
                [ arg "-b", arg "html"
                , arg "-d", arg $ outPath -/- ".doctrees-html"
                , arg =<< getInput
                , arg outPath ]
            , builder (Sphinx Latex) ? mconcat
                [ arg "-b", arg "latex"
                , arg "-d", arg $ outPath -/- ".doctrees-latex"
                , arg =<< getInput
                , arg outPath ]
            , builder (Sphinx Man) ? mconcat
                [ arg "-b", arg "latex"
                , arg "-d", arg $ outPath -/- ".doctrees-man"
                , arg =<< getInput
                , arg outPath ] ]

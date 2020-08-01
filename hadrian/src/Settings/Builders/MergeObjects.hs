module Settings.Builders.MergeObjects (mergeObjectsBuilderArgs) where

import Settings.Builders.Common

mergeObjectsBuilderArgs :: Args
mergeObjectsBuilderArgs = builder MergeObjects ? mconcat
    [ getStagedSettingList ConfMergeObjectsArgs
    , arg "-o", arg =<< getOutput
    , getInputs ]

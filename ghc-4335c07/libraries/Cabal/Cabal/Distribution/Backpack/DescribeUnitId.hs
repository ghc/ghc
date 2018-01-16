{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Distribution.Backpack.DescribeUnitId where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.PackageId
import Distribution.Types.ComponentName
import Distribution.Compat.Stack
import Distribution.Verbosity
import Distribution.ModuleName
import Distribution.Text
import Distribution.Simple.Utils

import Text.PrettyPrint

-- Unit identifiers have a well defined, machine-readable format,
-- but this format isn't very user-friendly for users.  This
-- module defines some functions for solving common rendering
-- problems one has for displaying these.
--
-- There are three basic problems we tackle:
--
--  - Users don't want to see pkg-0.5-inplace-libname,
--    they want to see "library 'libname' from 'pkg-0.5'"
--
--  - Users don't want to see the raw component identifier, which
--    usually contains a wordy hash that doesn't matter.
--
--  - Users don't want to see a hash of the instantiation: they
--    want to see the actual instantiation, and they want it in
--    interpretable form.
--

-- | Print a Setup message stating (1) what operation we are doing,
-- for (2) which component (with enough details to uniquely identify
-- the build in question.)
--
setupMessage' :: Text a => Verbosity
             -> String            -- ^ Operation being done (capitalized), on:
             -> PackageIdentifier -- ^ Package
             -> ComponentName     -- ^ Component name
             -> Maybe [(ModuleName, a)] -- ^ Instantiation, if available.
                                        -- Polymorphic to take
                                        -- 'OpenModule' or 'Module'
             -> IO ()
setupMessage' verbosity msg pkgid cname mb_insts = withFrozenCallStack $ do
    noticeDoc verbosity $
      case mb_insts of
        Just insts | not (null insts) ->
          hang (msg_doc <+> text "instantiated with") 2
               (vcat [ disp k <+> text "=" <+> disp v
                     | (k,v) <- insts ]) $$
          for_doc
        _ ->
          msg_doc <+> for_doc

  where
    msg_doc = text msg <+> text (showComponentName cname)
    for_doc = text "for" <+> disp pkgid <<>> text ".."

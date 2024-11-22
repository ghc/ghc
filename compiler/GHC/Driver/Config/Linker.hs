module GHC.Driver.Config.Linker
  ( initFrameworkOpts
  , initLinkerConfig
  )
where

import GHC.Prelude
import GHC.Platform
import GHC.Linker.Config

import GHC.Driver.DynFlags
import GHC.Driver.Session

import Data.List (isPrefixOf)

initFrameworkOpts :: DynFlags -> FrameworkOpts
initFrameworkOpts dflags = FrameworkOpts
  { foFrameworkPaths    = frameworkPaths    dflags
  , foCmdlineFrameworks = cmdlineFrameworks dflags
  }

-- | Initialize linker configuration from DynFlags
initLinkerConfig :: DynFlags -> LinkerConfig
initLinkerConfig dflags =
  let
    -- see Note [Solaris linker]
    ld_filter = case platformOS (targetPlatform dflags) of
                  OSSolaris2 -> sunos_ld_filter
                  _          -> id
    sunos_ld_filter :: [String] -> [String]
    sunos_ld_filter x = if (undefined_found x && ld_warning_found x)
                          then (ld_prefix x) ++ (ld_postfix x)
                          else x
    breakStartsWith x y = break (isPrefixOf x) y
    ld_prefix = fst . breakStartsWith "Undefined"
    undefined_found = not . null . snd . breakStartsWith "Undefined"
    ld_warn_break = breakStartsWith "ld: warning: symbol referencing errors"
    ld_postfix = tail . snd . ld_warn_break
    ld_warning_found = not . null . snd . ld_warn_break

    -- program and arguments
    --
    -- `-optl` args come at the end, so that later `-l` options
    -- given there manually can fill in symbols needed by
    -- Haskell libraries coming in via `args`.
    (p,pre_args) = pgm_l dflags
    post_args    = map Option (getOpts dflags opt_l)

  in LinkerConfig
    { linkerProgram     = p
    , linkerOptionsPre  = pre_args
    , linkerOptionsPost = post_args
    , linkerTempDir     = tmpDir dflags
    , linkerFilter      = ld_filter
    }

{- Note [Solaris linker]
   ~~~~~~~~~~~~~~~~~~~~~
  SunOS/Solaris ld emits harmless warning messages about unresolved
  symbols in case of compiling into shared library when we do not
  link against all the required libs. That is the case of GHC which
  does not link against RTS library explicitly in order to be able to
  choose the library later based on binary application linking
  parameters. The warnings look like:

Undefined                       first referenced
  symbol                             in file
stg_ap_n_fast                       ./T2386_Lib.o
stg_upd_frame_info                  ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziLib_litE_closure ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziLib_appE_closure ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziLib_conE_closure ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziSyntax_mkNameGzud_closure ./T2386_Lib.o
newCAF                              ./T2386_Lib.o
stg_bh_upd_frame_info               ./T2386_Lib.o
stg_ap_ppp_fast                     ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziLib_stringL_closure ./T2386_Lib.o
stg_ap_p_fast                       ./T2386_Lib.o
stg_ap_pp_fast                      ./T2386_Lib.o
ld: warning: symbol referencing errors

  this is actually coming from T2386 testcase. The emitting of those
  warnings is also a reason why so many TH testcases fail on Solaris.

  Following filter code is SunOS/Solaris linker specific and should
  filter out only linker warnings. Please note that the logic is a
  little bit more complex due to the simple reason that we need to preserve
  any other linker emitted messages. If there are any. Simply speaking
  if we see "Undefined" and later "ld: warning:..." then we omit all
  text between (including) the marks. Otherwise we copy the whole output.
-}


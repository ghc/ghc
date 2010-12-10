# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# dummy-ghc

# This is a tiny program to fool Cabal's configure that we have a
# stage1 GHC, which lets us configure all the packages before we've
# build stage1.

utils/dummy-ghc_dist-dummy-ghc_MODULES = dummy-ghc
utils/dummy-ghc_dist-dummy-ghc_PROG    = dummy-ghc$(exeext)

# depend on project.mk, so we pick up the new version number if it changes.
utils/dummy-ghc/dist-dummy-ghc/build/dummy-ghc.hs : utils/dummy-ghc/ghc.mk $(MKDIRHIER) mk/project.mk compiler/main/DynFlags.hs
	"$(MKDIRHIER)" $(dir $@)
	"$(RM)" $(RM_OPTS) $@
	echo 'import System.Environment'                                  >> $@
	echo 'import System.Cmd'                                          >> $@
	echo 'import System.Exit'                                         >> $@
	echo 'main :: IO ()'                                              >> $@
	echo 'main = do args <- getArgs'                                  >> $@
	echo '          case args of'                                     >> $@
	echo '              ["--numeric-version"] ->'                     >> $@
	echo '                  putStrLn "$(ProjectVersion)"'             >> $@
	echo '              ["--supported-languages"] ->'                 >> $@
	echo '                  mapM_ putStrLn extensions'                >> $@
	echo '              _ ->'                                         >> $@
	echo '                  do e <- rawSystem "$(GHC_STAGE0)" args'   >> $@
	echo '                     exitWith e'                            >> $@
# This unpleasant sed script grabs the lines between the
# 	xFlags ::
# line and the
#   ]
# line of compiler/main/DynFlags.hs, and if they look like
#   ( "PostfixOperators", ...
# then it translates them into
#   ["PostfixOperators", "NoPostfixOperators"] ++
# Tabs are a pain to handle portably with sed, so rather than worrying
# about them we just use tr to remove them all before we start.
	echo 'extensions :: [String]'                                     >> $@
	echo 'extensions ='                                               >> $@
	'$(TR)' -d '\t' < compiler/main/DynFlags.hs | '$(SED)' '/^xFlags/,/]/s/^ *( *"\([^"]*\)"[^"]*/  ["\1", "No\1"] ++/p;d' >> $@
	echo '  []'                                                       >> $@

# We don't build dummy-ghc with Cabal, so we need to pass -package
# flags manually
utils/dummy-ghc_dist-dummy-ghc_HC_OPTS = -package process
$(eval $(call build-prog,utils/dummy-ghc,dist-dummy-ghc,0))


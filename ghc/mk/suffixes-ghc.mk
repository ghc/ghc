# -----------------------------------------------------------------------------
# suffxies-ghc.mk

# suffix rules needed for compiling bits of ghc.

# -----------------------------------------------------------------------------

ifdef UnlitSuffixRules

define UnlitSuffixCmds
	$(RM) $@
	$(GHC_UNLIT) $<  $@ || ( $(RM) $@ && exit 1 )
	@chmod 444 $@
endef

.lprl.prl:
	$(UnlitSuffixCmds)

.lh.h:
	$(UnlitSuffixCmds)

.lc.c:
	$(UnlitSuffixCmds)

.lhc.hc:
	$(UnlitSuffixCmds)

endif

# -----------------------------------------------------------------------------

ifdef UgenSuffixRules

%.h %.c %.U.hs : %.ugn
	@$(RM) $@ $*.hs $*.U.hs $*.h
	$(UGEN) $< || $(RM) $*.h $@ $*.hs
	@$(MV) -f $*.hs $*.U.hs
	@chmod 444 $*.h $@ $*.U.hs

endif

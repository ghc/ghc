#################################################################################
#
#	 $Id: suffix.mk,v 1.2 1997/09/03 23:39:45 sof Exp $
#
#		GHC-specific suffix rules
#
#################################################################################

#-----------------------------------------------------------------------------
# Ugen suffix rules. 
#
# Hack, the implicit rule assumes the ugen files
# resides in a directory parser/
#

parser/%.h parser/%.c parser/U_%.hs : parser/%.ugn
	@$(RM) $@ parser/$*.hs parser/U_$*.hs parser/$*.h
	$(UGEN) $< || $(RM) parser/$*.h parser/U_$*.hs
	@$(MV) -f parser/$*.hs parser/U_$*.hs
	@chmod 444 parser/$*.h parser/U_$*.hs


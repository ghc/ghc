#################################################################################
#
#	 $Id: suffix.mk,v 1.1 1997/03/14 08:00:37 simonpj Exp $
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
	$(UGEN) $< || $(RM) parser/$*.h parser/$*.hs
	@$(MV) -f parser/$*.hs parser/U_$*.hs
	@chmod 444 parser/$*.h parser/U_$*.hs


#################################################################################
#
#	 $Id: suffix.mk,v 1.3 1997/09/04 19:51:32 sof Exp $
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
	@$(RM) $@ parser/$*.c parser/$*.hs parser/U_$*.hs parser/$*.h
	$(UGEN) $< || $(RM) parser/$*.h parser/$*.c parser/$*.hs
	@$(MV) -f parser/$*.hs parser/U_$*.hs
	@chmod 444 parser/$*.h parser/$*.c parser/U_$*.hs


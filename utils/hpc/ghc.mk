utils/hpc_dist_MODULES = Main HpcCombine HpcDraft HpcFlags HpcLexer HpcMap \
			 HpcMarkup HpcOverlay HpcParser HpcReport HpcSet \
			 HpcShowTix HpcUtils
utils/hpc_dist_HC_OPTS = -cpp -package hpc
utils/hpc_dist_INSTALL = YES
utils/hpc_dist_PROG    = hpc$(exeext)
$(eval $(call build-prog,utils/hpc,dist,1))

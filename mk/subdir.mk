#-----------------------------------------------------------------------------
# $Id: subdir.mk,v 1.2 1996/11/21 16:50:40 simonm Exp $

# Rules for passing on make commands to sub-directories

# Useful Variables to set

# 	SUBDIRS = subdirectories to recurse into

#	NoAllTargetForSubdirs			]
#	NoDocsTargetForSubdirs			]
#	NoInstallTargetForSubdirs		]
#	NoInstallDocsTargetForSubdirs		] omit specified rules
#	NoDependTargetForSubdirs		]
#	NoCleanTargetForSubdirs			]
#	NoVeryCleanTargetForSubdirs		]

#	ProjectMakeVarsForAllInSubdirs		make variables to pass to
#						Makefiles for the all target
#						in the current project
#	SetupMakeVarsForAllInSubdirs		same, but for the current
#						setup

#	ProjectMakeVarsForRunTestsInSubdirs	same, for project runtests
#	SetupMakeVarsForRunTestsInSubdirs	same, for setup

#-----------------------------------------------------------------------------
# The rules...

ifndef NoAllTargetForSubdirs
all::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) all; \
	done
endif

ifndef NoDocsTargetForSubdirs
docs::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) docs; \
	done
else
docs::
endif

ifndef NoRunTestsTargetForSubdirs
runtests::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) runtests; \
	done
else
runtests::
endif

ifndef NoInstallTargetForSubdirs
install::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) prefix='$(prefix)' install; \
	done
else
install::
endif

ifndef NoInstallDocsTargetForSubdirs
install_docs::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) prefix='$(prefix)' install_docs; \
	done
else
install_docs::
endif

ifndef NoDependTargetForSubdirs
depend::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) depend; \
	done
else
depend::
endif

ifndef NoTagTargetForSubdirs
tags::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) tags; \
	done
endif

ifndef NoCleanTargetForSubdirs
clean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) clean; \
	done
endif

ifndef NoVeryCleanTargetForSubdirs
veryclean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) veryclean; \
	done
endif

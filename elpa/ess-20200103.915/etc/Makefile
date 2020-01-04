### Makefile - for scripts and icons (./etc) of ESS distribution.
###

## Before making changes here, please take a look at Makeconf
include ../Makeconf

#ETCFILES = $(wildcard BACKBUG[S5].BAT backbug[s5] *.S sas-keys.*)
#ETCFILES = ESSR.R ess-developer.R SVN-REVISION *.S sas-keys.* ess-sas-sh-command
# ETCFILES_1 = *.S sas-keys.* ess-sas-sh-command *.jl
ETCFILES_1 = ess-sas-sh-command *.jl
isRELEASE=$(shell test -f .IS.RELEASE && echo 'yes')
ifeq ($(isRELEASE),yes)
  ETCFILES = .IS.RELEASE git-ref $(ETCFILES_1)
else
  ETCFILES =                     $(ETCFILES_1)
endif

#ICONS = $(wildcard icons/*.xpm)
ICONS = icons/*.xpm

ESSR_UTIL_FILES = ESSR/LOADREMOTE
ESSR_CODE_FILES = ESSR/R/*.R ESSR/R/.*.R

all:

show-etc:
	@echo $(ETCFILES)
	ls -l $(ETCFILES)

install :
	$(INSTALLDIR) $(ETCDIR)/icons
	$(INSTALLDIR) $(ETCDIR)/ESSR/R
	$(INSTALL) $(ETCFILES) $(ETCDIR)
	$(INSTALL) $(ICONS) $(ETCDIR)/icons
	$(INSTALL) $(ESSR_UTIL_FILES) $(ETCDIR)/ESSR
	$(INSTALL) $(ESSR_CODE_FILES) $(ETCDIR)/ESSR/R
	chmod +x $(ETCDIR)/ess-sas-sh-command

uninstall :
	-cd $(ETCDIR) && $(UNINSTALL) $(ETCFILES)
	-cd $(ETCDIR) && $(UNINSTALL) $(ICONS)
	-cd $(ETCDIR) && $(UNINSTALL) $(ESSR_UTIL_FILES)
	-cd $(ETCDIR) && $(UNINSTALL) $(ESSR_CODE_FILES)



## 'clean'     shall remove *exactly* those things that are *not* in version control
clean distclean:
	rm -rf SVN-REVISION
## 'distclean' removes also things in VC (svn, when they are remade by "make"):
# distclean: clean
#	rm -rf ESSR_*.tar.gz

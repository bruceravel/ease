
### Set the following variables appropriate to your installation #######
## how is emacs invoked?
EMACS = emacs
## how is /usr/bin/perl invoked?
PERL  = /usr/bin/perl
## what is the installation directory?
INSTALL = /home/bruce/source/ease
### End of user defined variables ######################################


## uncomment the next line to compile EASE support for xanes and correct
# XANES  = --eval "(setq ease-xanes-p t)"

VNUM  = 0.6.5
FLAGS = -batch -q -no-site-file -l ./ease-compile.el

.SUFFIXES:
.SUFFIXES:	.elc .el
.el.elc:
	$(EMACS) $(FLAGS) -f batch-byte-compile $*.el

.PHONY:	default lisp docs progs clean install
default:
	rm -rf *.elc
	$(MAKE) lisp
	$(MAKE) docs
	$(MAKE) progs
	@echo ' '
	@echo '*******************************************************'
	@echo ' Now do "make install" to install EASE.'
	@echo ' The installation step is not necessary if this is the'
	@echo ' install directory.'
	@echo ''
	@echo ' Use the f2e script to convert your .fuse and .emacs'
	@echo ' files from FUSE to EASE.'
	@echo '*******************************************************'
	@echo ' '

lisp:
	$(EMACS) $(FLAGS) $(XANES) -f ease-compile-files

docs:
	$(MAKE) -C docs/         info
	$(MAKE) -C tutorial/doc/ info


ALL_SCRIPTS = scripts/intrp scripts/kw scripts/eshift scripts/gnufix \
scripts/rhofix scripts/minmax scripts/mr scripts/lsdf

progs:
	$(PERL) scripts/fixin $(ALL_SCRIPTS)
	rm -f scripts/*.bak

clean:
	rm -f *.elc Makefile .config.ease scripts/*.bak *~
	mv -f input.el.in input.el
	$(MAKE) -C docs/ clean
	$(MAKE) -C tutorial/doc/ clean


install:
	install -d $(INSTALL)
	install -m 644 *.el  $(INSTALL)
	sleep 1
	install -m 644 *.elc $(INSTALL)
	install -m 644 Makefile $(INSTALL)
	install -d $(INSTALL)/docs
	install -d $(INSTALL)/emulation
	install -d $(INSTALL)/fortran
	install -d $(INSTALL)/scripts
	install docs/*.info         $(INSTALL)/docs
	install tutorial/doc/*.info $(INSTALL)/docs
	install emulation/*         $(INSTALL)/emulation
	install fortran/*           $(INSTALL)/fortran
	install scripts/*           $(INSTALL)/scripts
	chmod +x $(INSTALL)/scripts/*

#	cp -f docs/*.info         $(INSTALL)/docs
#	cp -f tutorial/doc/*.info $(INSTALL)/docs
#	cp -f emulation/*         $(INSTALL)/emulation
#	cp -f fortran/*           $(INSTALL)/fortran
#	cp -f scripts/*           $(INSTALL)/scripts




.PHONY:	info all dvi ps txt html clean

info:
	$(MAKE) ease.info
	$(MAKE) ease-dir.info

all:
	$(MAKE) info dvi ps txt html

dvi:
	$(MAKE) ease.dvi
	$(MAKE) quickref.dvi
	$(MAKE) gpelcard.dvi

ps:
	$(MAKE) ease.ps
	$(MAKE) quickref.ps
	$(MAKE) gpelcard.ps

txt:
	$(MAKE) ease.txt

html:
	$(MAKE) ease_toc.html

clean:
	rm -f *.html *.info *.log *.prm\
ease.aux ease.ky ease.txt ease.cp \
ease.log ease.vr ease.cps ease.pg \
ease.dvi ease.ps ease.fn \
ease.toc ease.info ease.tp \
quickref.aux quickref.dvi quickref.log quickref.ps \
gpelcard.aux gpelcard.dvi gpelcard.log gpelcard.ps


ease-dir.info:	ease-dir.texi
	@if (makeinfo --version)  < /dev/null > /dev/null 2>&1; \
	then makeinfo ease-dir.texi; \
	else echo; \
	echo " * * makeinfo not found -- cannot make tutorial in info form"; \
        echo; fi


## convert ease.texi to various formats

ease.info:	ease.texi
	@if (makeinfo --version)  < /dev/null > /dev/null 2>&1; \
	then makeinfo ease.texi; \
	else echo; \
	echo " * * makeinfo not found -- cannot make tutorial in info form"; \
        echo; fi

ease.dvi:	ease.texi
	@if (texi2dvi --version)  < /dev/null > /dev/null 2>&1; \
	then texi2dvi ease.texi; \
	else echo; \
	echo " * * texi2dvi not found -- cannot make tutorial in dvi form"; \
        echo; fi

ease.ps:	ease.dvi
	@if (dvips --version)  < /dev/null > /dev/null 2>&1; \
	then dvips ease.dvi; \
	else echo; \
	echo " * * dvips not found -- cannot make tutorial in postscript form"; \
        echo; fi

ease.txt:	ease.texi
	@(./texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then \
	  ./texi2html -monolithic ease.texi; \
	  (lynx      --version) < /dev/null > /dev/null 2>&1; \
	  if [ "$$?" == "255" ] ; \
	  then \
	    lynx -dump ease.html > ease.txt; \
	  else echo;\
	    echo " * * lynx not found -- cannot make tutorial in ASCII form"; \
	    echo;\
	  fi;\
	else echo; \
	  echo " * * texi2html not found -- cannot make tutorial in ASCII form"; \
	  echo;\
        fi;\
	rm ease.html; \

ease_toc.html:	ease.texi
	@(texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then texi2html -split_chapter ease.texi; \
	else echo; \
	echo " * * texi2html not found -- cannot make tutorial in html form"; \
        echo; fi




quickref.dvi:	quickref.tex
	latex quickref.tex

quickref.ps:	quickref.dvi
	dvips quickref.dvi



gpelcard.dvi:	gpelcard.tex
	latex gpelcard.tex

gpelcard.ps:	gpelcard.dvi
	dvips gpelcard.dvi



######################################################################

## convert atoms.texi to various formats

atoms.info:	atoms.texi
	@if (makeinfo --version)  < /dev/null > /dev/null 2>&1; \
	then makeinfo atoms.texi; \
	else echo; \
	echo " * * makeinfo not found -- cannot make tutorial in info form"; \
        echo; fi

atoms.dvi:	atoms.texi
	@if (texi2dvi --version)  < /dev/null > /dev/null 2>&1; \
	then texi2dvi atoms.texi; \
	else echo; \
	echo " * * texi2dvi not found -- cannot make tutorial in dvi form"; \
        echo; fi

atoms.ps:	atoms.dvi
	@if (dvips --version)  < /dev/null > /dev/null 2>&1; \
	then dvips atoms.dvi; \
	else echo; \
	echo " * * dvips not found -- cannot make tutorial in postscript form"; \
        echo; fi

atoms.txt:	atoms.texi
	@(./texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then \
	  ./texi2html -monolithic atoms.texi; \
	  (lynx      --version) < /dev/null > /dev/null 2>&1; \
	  if [ "$$?" == "255" ] ; \
	  then \
	    lynx -dump atoms.html > atoms.txt; \
	  else echo;\
	    echo " * * lynx not found -- cannot make tutorial in ASCII form"; \
	    echo;\
	  fi;\
	else echo; \
	  echo " * * texi2html not found -- cannot make tutorial in ASCII form"; \
	  echo;\
        fi;\
	rm atoms.html; \

atoms_toc.html:	atoms.texi
	@(texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then texi2html -split_chapter atoms.texi; \
	else echo; \
	echo " * * texi2html not found -- cannot make tutorial in html form"; \
        echo; fi

## convert feff7.texi to various formats

feff7.info:	feff7.texi
	@if (makeinfo --version)  < /dev/null > /dev/null 2>&1; \
	then makeinfo feff7.texi; \
	else echo; \
	echo " * * makeinfo not found -- cannot make tutorial in info form"; \
        echo; fi

feff7.dvi:	feff7.texi
	@if (texi2dvi --version)  < /dev/null > /dev/null 2>&1; \
	then texi2dvi feff7.texi; \
	else echo; \
	echo " * * texi2dvi not found -- cannot make tutorial in dvi form"; \
        echo; fi

feff7.ps:	feff7.dvi
	@if (dvips --version)  < /dev/null > /dev/null 2>&1; \
	then dvips feff7.dvi; \
	else echo; \
	echo " * * dvips not found -- cannot make tutorial in postscript form"; \
        echo; fi

feff7.txt:	feff7.texi
	@(./texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then \
	  ./texi2html -monolithic feff7.texi; \
	  (lynx      --version) < /dev/null > /dev/null 2>&1; \
	  if [ "$$?" == "255" ] ; \
	  then \
	    lynx -dump feff7.html > feff7.txt; \
	  else echo;\
	    echo " * * lynx not found -- cannot make tutorial in ASCII form"; \
	    echo;\
	  fi;\
	else echo; \
	  echo " * * texi2html not found -- cannot make tutorial in ASCII form"; \
	  echo;\
        fi;\
	rm feff7.html; \

feff7_toc.html:	feff7.texi
	@(texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then texi2html -split_chapter feff7.texi; \
	else echo; \
	echo " * * texi2html not found -- cannot make tutorial in html form"; \
        echo; fi

## convert phit.texi to various formats

phit.info:	phit.texi
	@if (makeinfo --version)  < /dev/null > /dev/null 2>&1; \
	then makeinfo phit.texi; \
	else echo; \
	echo " * * makeinfo not found -- cannot make tutorial in info form"; \
        echo; fi

phit.dvi:	phit.texi
	@if (texi2dvi --version)  < /dev/null > /dev/null 2>&1; \
	then texi2dvi phit.texi; \
	else echo; \
	echo " * * texi2dvi not found -- cannot make tutorial in dvi form"; \
        echo; fi

phit.ps:	phit.dvi
	@if (dvips --version)  < /dev/null > /dev/null 2>&1; \
	then dvips phit.dvi; \
	else echo; \
	echo " * * dvips not found -- cannot make tutorial in postscript form"; \
        echo; fi

phit.txt:	phit.texi
	@(./texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then \
	  ./texi2html -monolithic phit.texi; \
	  (lynx      --version) < /dev/null > /dev/null 2>&1; \
	  if [ "$$?" == "255" ] ; \
	  then \
	    lynx -dump phit.html > phit.txt; \
	  else echo;\
	    echo " * * lynx not found -- cannot make tutorial in ASCII form"; \
	    echo;\
	  fi;\
	else echo; \
	  echo " * * texi2html not found -- cannot make tutorial in ASCII form"; \
	  echo;\
        fi;\
	rm phit.html; \

phit_toc.html:	phit.texi
	@(texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then texi2html -split_chapter phit.texi; \
	else echo; \
	echo " * * texi2html not found -- cannot make tutorial in html form"; \
        echo; fi

## convert normal.texi to various formats

normal.info:	normal.texi
	@if (makeinfo --version)  < /dev/null > /dev/null 2>&1; \
	then makeinfo normal.texi; \
	else echo; \
	echo " * * makeinfo not found -- cannot make tutorial in info form"; \
        echo; fi

normal.dvi:	normal.texi
	@if (texi2dvi --version)  < /dev/null > /dev/null 2>&1; \
	then texi2dvi normal.texi; \
	else echo; \
	echo " * * texi2dvi not found -- cannot make tutorial in dvi form"; \
        echo; fi

normal.ps:	normal.dvi
	@if (dvips --version)  < /dev/null > /dev/null 2>&1; \
	then dvips normal.dvi; \
	else echo; \
	echo " * * dvips not found -- cannot make tutorial in postscript form"; \
        echo; fi

normal.txt:	normal.texi
	@(./texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then \
	  ./texi2html -monolithic normal.texi; \
	  (lynx      --version) < /dev/null > /dev/null 2>&1; \
	  if [ "$$?" == "255" ] ; \
	  then \
	    lynx -dump normal.html > normal.txt; \
	  else echo;\
	    echo " * * lynx not found -- cannot make tutorial in ASCII form"; \
	    echo;\
	  fi;\
	else echo; \
	  echo " * * texi2html not found -- cannot make tutorial in ASCII form"; \
	  echo;\
        fi;\
	rm normal.html; \

normal_toc.html:	normal.texi
	@(texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then texi2html -split_chapter normal.texi; \
	else echo; \
	echo " * * texi2html not found -- cannot make tutorial in html form"; \
        echo; fi

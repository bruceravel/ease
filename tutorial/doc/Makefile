

.PHONY:	info all dvi ps txt html clean

info:
	$(MAKE) ease-tutorial.info

all:
	$(MAKE) info dvi ps txt html

dvi:
	$(MAKE) ease-tutorial.dvi

ps:
	$(MAKE) ease-tutorial.ps

txt:
	$(MAKE) ease-tutorial.txt

html:
	$(MAKE) ease-tutorial_toc.html

clean:
	rm -f *.html *.info *.log *.prm\
ease-tutorial.aux ease-tutorial.ky ease-tutorial.txt ease-tutorial.cp \
ease-tutorial.log ease-tutorial.vr ease-tutorial.cps ease-tutorial.pg \
ease-tutorial.dvi ease-tutorial.ps ease-tutorial.fn \
ease-tutorial.toc ease-tutorial.info ease-tutorial.tp


ease-tutorial.info:	ease-tutorial.texi
	@if (makeinfo --version)  < /dev/null > /dev/null 2>&1; \
	then makeinfo ease-tutorial.texi; \
	else echo; \
	echo " * * makeinfo not found -- cannot make tutorial in info form"; \
        echo; fi

ease-tutorial.dvi:	ease-tutorial.texi
	@if (texi2dvi --version)  < /dev/null > /dev/null 2>&1; \
	then texi2dvi ease-tutorial.texi; \
	else echo; \
	echo " * * texi2dvi not found -- cannot make tutorial in dvi form"; \
        echo; fi

ease-tutorial.ps:	ease-tutorial.dvi
	@if (dvips --version)  < /dev/null > /dev/null 2>&1; \
	then dvips ease-tutorial.dvi; \
	else echo; \
	echo " * * dvips not found -- cannot make tutorial in postscript form"; \
        echo; fi

ease-tutorial.txt:	ease-tutorial.texi
	@(./texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then \
	  ./texi2html -monolithic ease-tutorial.texi; \
	  (lynx      --version) < /dev/null > /dev/null 2>&1; \
	  if [ "$$?" == "255" ] ; \
	  then \
	    lynx -dump ease-tutorial.html > ease-tutorial.txt; \
	  else echo;\
	    echo " * * lynx not found -- cannot make tutorial in ASCII form"; \
	    echo;\
	  fi;\
	else echo; \
	  echo " * * texi2html not found -- cannot make tutorial in ASCII form"; \
	  echo;\
        fi;\
	rm ease-tutorial.html; \

ease-tutorial_toc.html:	ease-tutorial.texi
	@(texi2html --version)  < /dev/null > /dev/null 2>&1; \
	if [ "$$?" == "255" ] ; \
	then texi2html -split_chapter ease-tutorial.texi; \
	else echo; \
	echo " * * texi2html not found -- cannot make tutorial in html form"; \
        echo; fi

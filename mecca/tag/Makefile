# -*- Makefile -*-
# ----------------------------------------------
export
SHELL    = sh

INSTALLDIR = ../../../../../bin/.

# ----------------------------------------------
### name of the executable that will be produced
PROG1       = imtag.exe

# --------------------------------------------------------------------

all: $(PROG1)

$(PROG1): imtag.pas imcom.inc imdot.inc
	@if which fpc ; then \
	   fpc -l -viwnh -B imtag -o$(PROG1) ;\
	   mv -f $(PROG1) $(INSTALLDIR)/. ;\
	else \
	  echo '##########################################################';\
	  echo '### Warning: fpc (free pascal compiler) not available. ###';\
	  echo '##########################################################';\
	fi

clean:
	rm -f *~
	rm -f *.o

distclean: clean
	rm -f $(INSTALLDIR)/$(PROG1)

install: all

# ----------------------------------------------

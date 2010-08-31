# -*- Makefile -*-
# ----------------------------------------------
export
SHELL    = sh

INSTALLDIR = ../../../../../bin/.

# ----------------------------------------------
### name of the executable that will be produced
PROG1       = imdouble.exe
PROG2       = embudget.exe

# --------------------------------------------------------------------

all: $(PROG1) $(PROG2)

$(PROG1): imdouble.pas imcom.inc
	@if which fpc ; then \
	   fpc -l -viwnh -B imdouble -o$(PROG1) ;\
	   mv -f $(PROG1) $(INSTALLDIR)/. ;\
	else \
	  echo '##########################################################';\
	  echo '### Warning: fpc (free pascal compiler) not available. ###';\
	  echo '##########################################################';\
	fi

$(PROG2): embudget.pas imcom.inc
	@if which fpc ; then \
	   fpc -l -viwnh -B embudget -o$(PROG2) ;\
	   mv -f $(PROG2) $(INSTALLDIR)/. ;\
	else \
	  echo '##########################################################';\
	  echo '### Warning: fpc (free pascal compiler) not available. ###';\
	  echo '##########################################################';\
	fi

clean:
	rm -f *~
	rm -f *.o

distclean: clean
	rm -f $(INSTALLDIR)/$(PROG1) $(INSTALLDIR)/$(PROG2)

install: all

# ----------------------------------------------

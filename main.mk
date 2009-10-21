### name of the executable that will be produced
PROG = $(INSTALLDIR)/caaba.exe

# complete list of all f90 source files
SRCS0 = $(wildcard *.f90)
SRCS  = $(filter-out F%.f90, $(SRCS0))

# the object files are the same as the source files but with suffix ".o"
OBJS := $(SRCS:.f90=.o)

MAKEFILE_INC = depend.mk

# If you don't have the perl script sfmakedepend, get it from:
# http://www.arsc.edu/~kate/Perl
F_makedepend = ./sfmakedepend --file=$(MAKEFILE_INC)

all: $(PROG)

# the dependencies depend on the link
# the executable depends on depend and also on all objects
# the executable is created by linking all objects
$(PROG): depend $(OBJS)
	$(F90) $(F90R8) $(F90FLAGS) $(OBJS) $(NETCDF_LIB) -o $@

# update file dependencies
depend $(MAKEFILE_INC): $(SRCS)
	$(F_makedepend) $(SRCS)

.PHONY: zip
zip:
	./zipcaaba.tcsh zip

.PHONY: zipall
zipall:
	./zipcaaba.tcsh zipall

# forcheck
.PHONY: check
check:
	-forchk -rigor -cond -f95 -obs -ff -decl -ext -intr -spec -ancmpl -anprg -anref -shcom -shinc -shmod -shprg -shref -shsrc -shsub -inf -plen 25 -pwid 132 *.f90 /soft/ECHAM5/lib/netcdf90.flb >& forcheck.log
	@echo "(forcheck found: 8=errors, 4=warnings, 2=infos)"

# put the same find commands here as in cmg command:
.PHONY: TAGS
TAGS:
	@F90FILES=`find . -name "*.f90" -type f` ;\
	 INCFILES=`find . -name "*.inc" -type f` ;\
	 NMLFILES=`find . -name "*.nml" -type f` ;\
	 TEXFILES=`find . -name "*.tex" -type f` ;\
	 KPPFILES1=`find . -name "*.eqn" -type f` ;\
	 KPPFILES2=`find . -name "*.spc" -type f` ;\
	 KPPFILES3=`find . -name "*.kpp" -type f` ;\
	 XSCRIPTS=`find . -name "x*" -type f -perm -100` ;\
	 etags -l fortran $$F90FILES $$INCFILES $$NMLFILES \
           -lnone $$TEXFILES $$KPPFILES1 $$KPPFILES2 $$KPPFILES3 $$XSCRIPTS

# check files
list:
	@echo "SRCS           = $(SRCS)"
	@echo "OUTPUT         = $(OUTPUT)"
	@echo "SYSTEM         = $(SYSTEM)"
	@echo "HOST           = $(HOST)"
	@echo "COMPILER       = $(COMPILER)"
	@echo "BITS           = $(BITS)"
	@echo "F90R8          = $(F90R8)"
	@echo "F90FLAGS       = $(F90FLAGS)"
	@echo "NETCDF_INCLUDE = $(NETCDF_INCLUDE)"
	@echo "NETCDF_LIB     = $(NETCDF_LIB)"

# this command is executed by "gmake clean"
clean:
	rm -f depend.mk.old *.o *.mod *.log *~

distclean: clean
	rm -f $(PROG)
	rm -f depend.mk* 
	rm -f *.nc
	rm -f *.dat
	rm -f mecca/latex/*.aux
	rm -f mecca/latex/*.blg
	rm -f mecca/latex/*.dvi
	rm -f mecca/latex/*.log
	rm -f *.exe

# all object files *.o depend on their source files *.f90
# the object files are created with the "-c" compiler option
# do not delete $(ADDEFS), since it is set by Makefile.m
%.o: %.f90
	$(F90) $(DEFOPT)$(OUTPUT) $(ADDEFS) $(F90R8) $(F90FLAGS) $(NETCDF_INCLUDE) -c $<

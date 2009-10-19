##############################################################################

### select either ASCII or NETCDF output:
#OUTPUT     = ASCII
OUTPUT     = NETCDF
##############################################################################

### directory for executable:
INSTALLDIR = .

##############################################################################

### Try to find the compiler for current machine automatically based on
### operating system:

SYSTEM := $(shell uname)
HOST := $(shell hostname)

DEFOPT = -D

### 32 or 64-bit architecture?
ifeq "$(findstring 64,$(MACHTYPE))" "64"
  BITS = 64
else
  BITS = 32
endif

ifeq ($(SYSTEM),Linux)
  ### Choose compiler:
  #COMPILER = lahey
  #COMPILER = intel
  COMPILER = g95
    G95VERSION = 0.91
    #G95VERSION = 0.9
  #COMPILER = gfortran
  #COMPILER = pgf90
endif

ifeq ($(SYSTEM),OSF1)
  COMPILER = f90_alpha
endif

ifeq ($(SYSTEM),AIX)
  COMPILER = xlf95
  DEFOPT = -Wf,-D
endif

### If this automatic mechanim doesn't work for you, define the compiler here:
#COMPILER = 

### If this doesn't work for you either, enter the correct values for
### F90, F90FLAGS, etc. in the block near the end of this file

##############################################################################

ifeq ($(COMPILER),lahey)
  ### Lahey compiler:
  F90        = lf95

  ### Choose compiler options:
  ### For lf95-6.2a, --staticlink must not be used!
  #F90FLAGS  = -Cpp --pca --ap
  F90FLAGS   = -Cpp --chk a,e,s,u --pca --ap -O0 -g --trap
  ### --staticlink is necessary for lf95-6.2d because then the
  ### undefined reference to `__divdi3' will be found in
  ### /usr/local/lf9562/lib/libfst.a
  #F90FLAGS   = -Cpp --staticlink --pca --ap
  #F90FLAGS  = -Cpp --staticlink --chk a,e,s,u --pca --ap -O0 -g --trap

  ifeq ($(BITS),64)
    NETCDF_INCLUDE  = -I/soft/netcdf_64/v3.6.2_lf_64/include
    NETCDF_LIB      = -L/soft/netcdf_64/v3.6.2_lf_64/lib -lnetcdf
  else
    NETCDF_INCLUDE  = -I/soft/netcdf/v3.6.2_lf/include
    NETCDF_LIB      = -L/soft/netcdf/v3.6.2_lf/lib -lnetcdf
  endif
endif

# ----------------------------------------------------------------------------

ifeq ($(COMPILER),g95)
  ### www.g95.org:
  #F90  = g95
  ### F90 is defined below, depending on 32/64 bit and version 0.9/0.91

  ### Choose compiler options:
  ### -Wall           = Enable most warning messages
  ### -cpp            = run the C preprocessor
  ### -O<n>           = optimization level <n>=0...3, zero = no optimization
  ### -O              = same as -O1
  ### -g              = debug option
  ### -fbounds-check  = check array and substring bounds at runtime
  ### -fimplicit-none = IMPLICIT NONE, unless overridden by IMPLICIT statements
  ### -ftrace=full    = show line number when arithmetic exceptions occur
  #F90FLAGS  = -cpp
  #F90FLAGS = -cpp -O0 -g -fbounds-check -fimplicit-none -Wall -ftrace=full
  F90FLAGS = -cpp -O0 -g -fbounds-check -fimplicit-none -ftrace=full

  ifeq ($(BITS),64)
    ifeq ($(G95VERSION),0.91)
      F90  = /soft/g95_64/v0.91/bin/g95
      NETCDF_INCLUDE = -I/soft/netcdf_64/v3.6.2_g95v091_64/include
      NETCDF_LIB     = -L/soft/netcdf_64/v3.6.2_g95v091_64/lib -lnetcdf
    else
      F90  = /soft/g95_64/v0.9/bin/g95
      NETCDF_INCLUDE = -I/soft/netcdf_64/v3.6.2_g_64/include
      NETCDF_LIB     = -L/soft/netcdf_64/v3.6.2_g_64/lib -lnetcdf
    endif
  else
    ifeq ($(G95VERSION),0.91)
      F90  = /soft/g95/v0.91/bin/g95
      NETCDF_INCLUDE = -I/soft/netcdf/v3.6.2_g95v091/include
      NETCDF_LIB     = -L/soft/netcdf/v3.6.2_g95v091/lib -lnetcdf
    else
      F90  = /soft/g95/v0.9/bin/g95
      NETCDF_INCLUDE = -I/soft/netcdf/v3.6.0b6_g/include
      NETCDF_LIB     = -L/soft/netcdf/v3.6.0b6_g/lib -lnetcdf
    endif
  endif
endif

# ----------------------------------------------------------------------------

ifeq ($(COMPILER),f90_alpha)
  ### UNIX alpha:
  F90        	  = f90
  F90FLAGS   	  = -O -cpp
  NETCDF_INCLUDE  = -I/soft/netcdf/v3.6.0b6/include
  NETCDF_LIB      = -L/soft/netcdf/v3.6.0b6/lib -lnetcdf
endif

# ----------------------------------------------------------------------------

ifeq ($(COMPILER),intel)
  ### intel compiler (8.0.039):
  F90        	  = ifort
  F90FLAGS   	  = -cpp -O0
  NETCDF_INCLUDE  = -I/soft/netcdf/v3.5.1b11_i/include
  NETCDF_LIB      = -L/soft/netcdf/v3.5.1b11_i/lib -lnetcdf
endif

# ----------------------------------------------------------------------------

### UNDER CONSTRUCTION, DOESN'T WORK YET!!!
### the linker doesn't like "-x f95-cpp-input" in $(F90FLAGS) -> gfortran bug?
ifeq ($(COMPILER),gfortran)
  ### gnu compiler: http://gcc.gnu.org/fortran/index.html
  F90      = gfortran
  F90FLAGS = -O0 -x f95-cpp-input
  ifeq ($(BITS),64)
    NETCDF_INCLUDE = -I/soft/netcdf_64/v3.6.2_gf_64/include
    NETCDF_LIB     = -L/soft/netcdf_64/v3.6.2_gf_64/lib -lnetcdf
  else
    NETCDF_INCLUDE = -I/soft/netcdf/v3.6.2_gf/include
    NETCDF_LIB     = -L/soft/netcdf/v3.6.2_gf/lib -lnetcdf
  endif
endif

# ----------------------------------------------------------------------------

ifeq ($(COMPILER),pgf90)
  ### portland group f90 (needs version 5.2-4 or newer!)
  ### (input provided by Tom Sobieraj and Jacek Kaminski)
  F90            = pgf90
  F90FLAGS       = -Mpreprocess -O
  ### next line suggested by Mike Long (email 2008-10-14):
  #F90FLAGS      = -Mpreprocess -O3 -Munroll -Mnoframe -Mlre
  NETCDF_INCLUDE = -I/usr/local/include
  NETCDF_LIB     = -L/usr/local/lib -lnetcdf
endif

# ----------------------------------------------------------------------------

ifeq ($(COMPILER),xlf95)
  ### AIX:
  F90       = xlf95_r
  F90FLAGS  = -q64 -qsuppress=1518-061:1518-128 -qsuppress=1500-036 -O3 -qstrict -qMAXMEM=-1 -qsuffix=cpp=f90 -qzerosize -WF,-D__ibm__ -d -WF,-qlanglvl=classic -qlanglvl=95pure -qfree=f90 -qspillsize=32648 -qarch=pwr4 -qtune=pwr4 -Q -qhot
  NETCDF_INCLUDE = -I/afs/ipp-garching.mpg.de/home/j/joeckel/extra_space/@sys/netcdf/v3.6.0b6/include
  NETCDF_LIB     = -L/afs/ipp-garching.mpg.de/home/j/joeckel/extra_space/@sys/netcdf/v3.6.0b6/lib -lnetcdf90 -lnetcdf
endif

##############################################################################

### To implement the boxmodel on a new machine, you can simply overwrite
### the above definitions by activating the following block and entering
### appropriate values for your system:
### F90:            Fortran90 compiler
### F90FLAGS:       Fortran90 compiler options (including the cpp option!)
### NETCDF_INCLUDE: path to netcdf include files
### NETCDF_LIB:     path to netcdf library and library name

#F90            =
#F90FLAGS       =
#NETCDF_INCLUDE =
#NETCDF_LIB     =

##############################################################################

### if ASCII output is selected, it is likely that there is no netcdf library
### available. Therefore, the coresponding variables are set to "nothing":

ifeq ($(OUTPUT),ASCII)
  NETCDF_INCLUDE  =
  NETCDF_LIB      =
endif

##############################################################################

# targets
include main.mk

# list of dependencies (via USE statements)
include depend.mk

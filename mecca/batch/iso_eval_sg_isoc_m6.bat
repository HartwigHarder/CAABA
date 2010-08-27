# -*- Shell-script -*-

# Author: Sergey Gromov (2008)
# The shell variables defined here will be used by xmecca 
# when it is run in batch mode (i.e. not interactive).

 set apn          = 0             # number of aerosol phases [0...99, default=0]
 set gaseqnfile   = gas_mim1.eqn
 set rplfile      =               # no replacements
 # eval:
 set wanted       = "Tr && G && \!C && \!S && \!Cl && \!Br && \!I && \!Hg"
 set mcfct        = n                 # Monte-Carlo factor?
 set diagtracfile =                   # diagnostic tracers?
 set rxnrates     = n                 # calculate accumulated reaction rates?
 set tagdbl       = b                 # tagging, doubling, both, none ??
 set tagcfg       = c                 # tagging configuration to choose (c,z)
# set kppoption    = 4                 # k=kpp, 4=kp4, q=quit
 set kppoption    = k                 # k=kpp, 4=kp4, q=quit
 set integr       = rosenbrock_posdef # integrator
# set integr       = rosenbrock_mz     # integrator
#set integr       = rosenbrock_vec    # integrator
 set vlen         = 256               # only for kp4 and integr=rosenbrock_vec
 set decomp       = 1                 # remove indirect indexing
                                      # kp4: 0/1/2/3/q; kpp: y/n/q
 set deltmpkp4    = y                 # delete temporary kp4 files
 set latex        = n                 # latex list of reactions
 set graphviz     = n                 # graphviz plots?
 set deltmp       = y                 # delete temporary xmecca files?

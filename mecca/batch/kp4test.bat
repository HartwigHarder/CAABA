# -*- Shell-script -*-

# The shell variables defined here will be used by xmecca 
# when it is run in batch mode (i.e. not interactive).

 set apn          = 2                 # number of aerosol phases [0...99, default=0]
 set gaseqnfile   = gas.eqn
#set rplfile      =                   # no replacements
 set rplfile      = mbl               # replacement file
 set wanted       = "Tr && (G || (Aa && Mbl)) && \!I && \!Hg" # 17) Minimum MBL-chem, no I
 set mcfct        = n                 # Monte-Carlo factor?
 set diagtracfile =                   # diagnostic tracers?
 set rxnrates     = n                 # calculate accumulated reaction rates?
 set tagdbl       = n                 # tagging, doubling, both, none ??
 set kppoption    = 4                 # k=kpp, 4=kp4, q=quit
 set integr       = rosenbrock_mz     # integrator
 set decomp       = n                 # remove indirect indexing
 set deltmpkp4    = n                 # delete temporary kp4 files
 set latex        = n                 # latex list of reactions
 set graphviz     = n                 # graphviz plots?
 set deltmp       = n                 # delete temporary xmecca files?

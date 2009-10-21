# -*- Shell-script -*-

# The shell variables defined here will be used by xmecca 
# when it is run in batch mode (i.e. not interactive).

 set apn          = 2                 # number of aerosol phases [0...99, default=0]
 set rplfile      =                   # replacement file
 set wanted       = "Tr && (G || (Aa && Mbl)) && \!I && \!Hg" # 17) Minimum MBL-chem, no I
 set mcfct        = n                 # Monte-Carlo factor?
 set diagtracfile =                   # diagnostic tracers?
 set rxnrates     = y                 # calculate accumulated reaction rates?
 set tagdbl       = n                 # tagging, doubling, both, none ??
 set kppoption    = k                 # k=kpp, 4=kp4, q=quit
 set integr       = rosenbrock_posdef # integrator
 set decomp       = n                 # remove indirect indexing
 set latex        = n                 # latex list of reactions
 set graphviz     = y                 # graphviz plots
 set deltmp       = y                 # delete temporary xmecca files?

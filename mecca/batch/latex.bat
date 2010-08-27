# -*- Shell-script -*-

# The shell variables defined here will be used by xmecca 
# when it is run in batch mode (i.e. not interactive).

 set apn          = 1                 # number of aerosol phases [0...99, default=0]
 set gaseqnfile   = gas.eqn
 set rplfile      =                   # no replacements
 set wanted       = "\!Ara"
 set mcfct        = n                 # Monte-Carlo factor?
 set diagtracfile =                   # diagnostic tracers?
 set rxnrates     = n                 # calculate accumulated reaction rates?
 set tagdbl       = n                 # tagging, doubling, both, none ??
 set kppoption    = k                 # k=kpp, 4=kp4, q=quit
 set integr       = rosenbrock_posdef # integrator
 set decomp       = n                 # remove indirect indexing
 set latex        = y                 # latex list of reactions
 set graphviz     = y                 # graphviz plots?
 set deltmp       = y                 # delete temporary xmecca files?

# -*- Shell-script -*-

# Author: Patrick Joeckel (2008)
# The shell variables defined here will be used by xmecca 
# when it is run in batch mode (i.e. not interactive).

 set apn          = 0                 # number of aerosol phases [0...99, default=0]
 set rplfile      =                   # no replacements
 # eval:
 set wanted = "((Tr && G && \!Cl && \!Br && \!I) || (St && \!Het)) && \!Hg"
 set mcfct        = n                 # Monte-Carlo factor?
 set diagtracfile = gas_budgets       # diagnostic tracers?
 set rxnrates     = y                 # calculate accumulated reaction rates?
 set tagdbl       = n                 # tagging, doubling, both, none ??
 set kppoption    = k                 # k=kpp, 4=kp4, q=quit
 set integr       = rosenbrock_posdef # integrator
 set decomp       = n                 # remove indirect indexing
                                      # kp4: 0/1/2/3/q; kpp: y/n/q
 set latex        = n                 # latex list of reactions
 set graphviz     = n                 # graphviz plots
 set deltmp       = y                 # delete temporary xmecca files?

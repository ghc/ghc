TOP = ../..
include $(TOP)/mk/boilerplate.mk
PROG_ARGS = runtime_files/inverter 345 10n 4u 1
include $(TOP)/mk/target.mk


# From: Erik van der Meer [mailto:vandermeer@greenvalley.nl]
# Sent: 17 March 2000 08:36
# To: Simon Peyton-Jones
# Subject: SCS for nofib
# 
# 
# Dear Mr. Jones,
# 
# In our telephone conversation, I promised you the program I wrote for my
# masters thesis. It is called SCS, for SET Circuit Simulator. It largely
# relies on arrays, and arithmetic, both Rational (preprocessing) and Double
# (the actual work). It also uses many higher order functions. It is a
# prototype which hasn't been optimized, because the main focus of my work was
# finding out how these 'circuits' actually work. If I would have had time to
# optimize, I would have tried to use band matrices, and I would have used a
# library like linpack to do the actual work.
# 
# Anyway, SCS consists of:
# 
# Main.hs
# Simulate.hs
# LinearAlgebra.hs
# RandomFix.hs
# Parse.hs
# ParseLib.hs
# Types.hs
# 
# (RandomFix.hs is a slightly modified version of the random library in
# Hugs, because the Hugs & GHC random libraries clashed.)
# 
# SCS comes with two utility programs:
# 
# VerifyDT.hs
# GnuPlotInterface.hs
# 
# which both rely on Types.hs.
# 
# On UNIX, I would compile the programs, naming them scs, vdt and gpi, move
# them into the circuit directory, and then I would run SCRIPT.
# 
# I should mention that most circuits here are very small, and so I
# recommend simulating the inverter, which is the most realistic circuit,
# at a temperature of 1 kelvin:
# 
# scs inverter 345 1n 4u 1
# 
# On my computer (a pentium 100, running Linux) I think it took about half an
# hour. To shorten it, you could increase the time step:
# 
# scs inverter 345 10n 4u 1
# 
# (where applicable, numbers can be appended with any of afpnum kMGTPE:
# atto, femto, pico, nano, micro, milli, kilo, mega, giga, tera, peta, exa.)
# 
# Alternatively, you could decrease the end time, which in the example is
# 4 microseconds. 345 is the seed for the random generator, and 1 is the
# temperature in kelvin, in case you wondered.
# 
# Yours sincerely,
# 
# Erik van der Meer.

# --------------------------------------------------------------------


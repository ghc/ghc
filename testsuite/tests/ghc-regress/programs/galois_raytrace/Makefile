TOP = ..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -package text -package lang

all :: runtest

include $(TOP)/mk/test.mk

# DO NOT DELETE: Beginning of Haskell dependencies
CSG.o : CSG.hs
CSG.o : ./Construct.hi
CSG.o : ./Geometry.hi
CSG.o : ./Intersections.hi
CSG.o : ./Interval.hi
CSG.o : ./Misc.hi
Construct.o : Construct.hs
Construct.o : ./Geometry.hi
Data.o : Data.hs
Data.o : ./CSG.hi
Data.o : ./Geometry.hi
Data.o : ./Illumination.hi
Data.o : ./Primitives.hi
Data.o : ./Surface.hi
Eval.o : Eval.hs
Eval.o : ./Geometry.hi
Eval.o : ./CSG.hi
Eval.o : ./Surface.hi
Eval.o : ./Data.hi
Eval.o : ./Parse.hi
Geometry.o : Geometry.hs
Illumination.o : Illumination.hs
Illumination.o : ./Geometry.hi
Illumination.o : ./CSG.hi
Illumination.o : ./Surface.hi
Illumination.o : ./Misc.hi
Intersections.o : Intersections.hs
Intersections.o : ./Construct.hi
Intersections.o : ./Geometry.hi
Intersections.o : ./Interval.hi
Intersections.o : ./Misc.hi
Interval.o : Interval.hs
Interval.o : ./Geometry.hi
Main.o : Main.hs
Main.o : ./Parse.hi
Main.o : ./Eval.hi
Misc.o : Misc.hs
Parse.o : Parse.hs
Parse.o : ./Data.hi
Pixmap.o : Pixmap.hs
Primitives.o : Primitives.hs
RayTrace.o : RayTrace.hs
RayTrace.o : ./Illumination.hi
RayTrace.o : ./Surface.hi
Surface.o : Surface.hs
Surface.o : ./Geometry.hi
Surface.o : ./CSG.hi
Surface.o : ./Misc.hi
# DO NOT DELETE: End of Haskell dependencies

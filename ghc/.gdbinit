directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/runtime
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/runtime/storage
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/runtime/c-as-asm
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/runtime/io
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/runtime/main
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/runtime/prims
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/runtime/profiling
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/runtime/gmp
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/glaExts/MainIO
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/glaExts/PreludeErrIO
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/glaExts/PreludeGlaST
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/glaExts/PreludePrimIO
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/glaExts/Stdio
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/glaExts/PreludeDialogueIO
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/glaExts/ByteOps
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/Builtin
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/Core
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IO
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/PS
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/List
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/Prel
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/Text
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/TysBasic
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/Cls
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IArray
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IBool
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IChar
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IComplex
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IDouble
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IFloat
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IInt
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IInteger
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IList
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/IRatio
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/ITup0
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/ITup2
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/ITup3
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/ITup4
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/ITup5
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/TyArray
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/TyBool
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/TyComplex
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/TyIO
directory /local/grasp_tmp3/partain/ghc-BUILDS/working/ghc/lib/prelude/TyRatio

define pR1
print (sfp) (((STGRegisterTable)MainRegTable).rR1)
end
define pR2
print (sfp) (((STGRegisterTable)MainRegTable).rR2)
end
define pR3
print (sfp) (((STGRegisterTable)MainRegTable).rR3)
end
define pR4
print (sfp) (((STGRegisterTable)MainRegTable).rR4)
end
define pR5
print (sfp) (((STGRegisterTable)MainRegTable).rR5)
end
define pR6
print (sfp) (((STGRegisterTable)MainRegTable).rR6)
end
define pR7
print (sfp) (((STGRegisterTable)MainRegTable).rR7)
end
define pR8
print (sfp) (((STGRegisterTable)MainRegTable).rR8)
end
define pFlt1
print (StgFloat) (((STGRegisterTable)MainRegTable).rFlt1)
end
define pDbl1
print (StgDouble) (((STGRegisterTable)MainRegTable).rDbl1)
end

define pSpA
print (sfp) (((STGRegisterTable)MainRegTable).rSpA)
end
define pSuA
print (sfp) (((STGRegisterTable)MainRegTable).rSuA)
end
define pSpB
print (sfp) (((STGRegisterTable)MainRegTable).rSpB)
end
define pSuB
print (sfp) (((STGRegisterTable)MainRegTable).rSuB)
end

define pHp
print (sfp) (((STGRegisterTable)MainRegTable).rHp)
end

define pHpLim
print (sfp) (((STGRegisterTable)MainRegTable) .rHpLim)
end

define pn
call DEBUG_PRINT_NODE(Ret1)
end

define pt
call DEBUG_TREE(Ret1)
end

define pit
call DEBUG_INFO_TABLE(Ret1)
end

define pr
call DEBUG_REGS()
end

define pas
call DEBUG_ASTACK(32767)
end

define pbs
call DEBUG_BSTACK(32767)
end

define pus
call DEBUG_UPDATES(32767)
end

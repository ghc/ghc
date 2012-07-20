BEGIN {
  print "#define OFFSET(s_type, field) OFFSET_ ## s_type ## _ ## field"
  print "#define FIELD_SIZE(s_type, field) FIELD_SIZE_ ## s_type ## _ ## field"
  print "#define TYPE_SIZE(type) TYPE_SIZE_ ## type"
  print ""
}

/^0[0-9a-zA-Z]* C _*associate\$/ {
  sub(/_*associate\$/, "", $3)
  split($3, arr, "$")
  assoc[arr[2]] = arr[1]
  next
}

/^00*2 C _*starting\$[0-9]*\$[_0-9a-zA-Z]*$/ {
  sub(/_*starting\$/, "", $3)
  split($3, arr, "$")
  sub(/^0*/, "", $1)
  print "#define OFFSET_" assoc[arr[1]] "_" arr[2] " 0x0"
  next
}

/^0[0-9a-zA-Z]* C _*sizeof\$[0-9]*\$[_0-9a-zA-Z]*$/ {
  sub(/_*sizeof\$/, "", $3)
  split($3, arr, "$")
  sub(/^0*/, "", $1)
  print "#define OFFSET_" assoc[arr[1]] "_" arr[2] " 0x" $1
  next
}

/^0[0-9a-zA-Z]* C _*fieldsize\$[0-9]*\$[_0-9a-zA-Z]*$/ {
  sub(/_*fieldsize\$/, "", $3)
  split($3, arr, "$")
  sub(/^0*/, "", $1)
  print "#define FIELD_SIZE_" assoc[arr[1]] "_" arr[2] " 0x" $1 "UL"
  next
}

/^0[0-9a-zA-Z]* C _*nestedfieldsize\$[_0-9a-zA-Z]*\$[_0-9a-zA-Z]*$/ {
  sub(/_*nestedfieldsize\$/, "", $3)
  split($3, arr, "$")
  sub(/^0*/, "", $1)
  print "#define FIELD_SIZE_" arr[1] "_" arr[2] " 0x" $1 "UL"
  next
}

/^0[0-9a-zA-Z]* C _*SIZEOF\$[0-9]*$/ {
  sub(/_*SIZEOF\$/, "", $3)
  sub(/^0*/, "", $1)
  print "#define TYPE_SIZE_" assoc[$3] " 0x" $1
  next
}

{ print "// " $0 }

END {
    ## some indirect offsets
    print "#define OFFSET_StgHeader_prof_ccs (OFFSET_StgHeader_prof + OFFSET_StgProfHeader_ccs)"
    print "#define OFFSET_StgHeader_prof_hp_ldvw (OFFSET_StgHeader_prof + OFFSET_StgProfHeader_hp + 0)"
    print "#define OFFSET_StgTSO_prof_cccs (OFFSET_StgTSO_prof + OFFSET_StgTSOProfInfo_cccs)"
    print "#define OFFSET_RTS_FLAGS_ProfFlags_showCCSOnException (OFFSET_RTS_FLAGS_ProfFlags + OFFSET_PROFILING_FLAGS_showCCSOnException)"


    print "#define OFFSET_RTS_FLAGS_DebugFlags_apply (OFFSET_RTS_FLAGS_DebugFlags + OFFSET_DEBUG_FLAGS_apply)"
    print "#define OFFSET_RTS_FLAGS_DebugFlags_sanity (OFFSET_RTS_FLAGS_DebugFlags + OFFSET_DEBUG_FLAGS_sanity)"
    print "#define OFFSET_RTS_FLAGS_DebugFlags_weak (OFFSET_RTS_FLAGS_DebugFlags + OFFSET_DEBUG_FLAGS_weak)"
    print "#define OFFSET_RTS_FLAGS_GcFlags_initialStkSize (OFFSET_RTS_FLAGS_GcFlags + OFFSET_GC_FLAGS_initialStkSize)"
    print "#define OFFSET_RTS_FLAGS_MiscFlags_tickInterval (OFFSET_RTS_FLAGS_MiscFlags + OFFSET_MISC_FLAGS_tickInterval)"

    print "#define OFFSET_StgFunInfoExtraFwd_b_bitmap (OFFSET_StgFunInfoExtraFwd_b + 0)"
    print "#define OFFSET_StgFunInfoExtraRev_b_bitmap (OFFSET_StgFunInfoExtraRev_b + 0)"

    ## some indirect field sizes
    print "#define FIELD_SIZE_StgHeader_prof_ccs FIELD_SIZE_StgProfHeader_ccs"
    print "#define FIELD_SIZE_StgTSO_prof_cccs FIELD_SIZE_StgTSOProfInfo_cccs"
    print "#define FIELD_SIZE_RTS_FLAGS_ProfFlags_showCCSOnException FIELD_SIZE_PROFILING_FLAGS_showCCSOnException"
    print "#define FIELD_SIZE_RTS_FLAGS_DebugFlags_apply FIELD_SIZE_DEBUG_FLAGS_apply"
    print "#define FIELD_SIZE_RTS_FLAGS_DebugFlags_sanity FIELD_SIZE_DEBUG_FLAGS_sanity"
    print "#define FIELD_SIZE_RTS_FLAGS_DebugFlags_weak FIELD_SIZE_DEBUG_FLAGS_weak"
    print "#define FIELD_SIZE_RTS_FLAGS_GcFlags_initialStkSize FIELD_SIZE_GC_FLAGS_initialStkSize"
    print "#define FIELD_SIZE_RTS_FLAGS_MiscFlags_tickInterval FIELD_SIZE_MISC_FLAGS_tickInterval"
}

section: rts
synopsis: Heap profiles now count every small pinned object
issues: #28000
mrs: !16840
description: {
  Blocks of small pinned objects (for example ``ByteString`` payloads) were
  counted by the heap census as a single object, so ``+RTS -hT`` and the other
  heap profile modes reported almost none of their memory. The census now walks
  these blocks completely, including the block that each capability is still
  filling. Because a pinned block is kept or freed as a whole (#28005), dead
  pinned objects are included in the count until the garbage collector frees
  their block.
}

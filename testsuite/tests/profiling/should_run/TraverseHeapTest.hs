foreign import ccall unsafe "traverseHeapRunTests" c_traverseHeapRunTests
    :: IO ()

main = c_traverseHeapRunTests

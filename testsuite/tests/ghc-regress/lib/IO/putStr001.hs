-- !!! Testing output on stdout

-- stdout is buffered, so test if its buffer
-- is flushed upon program termination.

main = putStr "Hello, world\n"


import PrelIOBase
foreign import "ilxHello" unsafe ilxHello :: IO ()



seqIO :: IO () -> IO () -> IO ()
seqIO (IO m) (IO k) = IO ( \ s ->
  case m s of 
    (# new_s, a #) -> k new_s
  )


yes () = seqIO ilxHello (yes ())

main :: IO ()
main = yes ()


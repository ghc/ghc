module Test1_nostdlib  where
foreign import "ilxHello" unsafe ilxHello :: ()

ilx_main_no_stdlib = ilxHello

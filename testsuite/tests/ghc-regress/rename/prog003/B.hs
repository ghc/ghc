-- !!! Exporting a class method should not export the class too
module B where
import A
f :: Class a => a -> a
f = method

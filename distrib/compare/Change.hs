
module Change where

data FileChange = First  Change
                | Second Change
                | Change Change

data Change = DuplicateFile FilePath
            | ExtraFile FilePath
            | ExtraWay String
            | ExtraThing String
            | ThingVersionChanged String String String
            | PermissionsChanged FilePath FilePath String String
            | FileSizeChanged FilePath FilePath Integer Integer

isSizeChange :: FileChange -> Bool
isSizeChange (Change (FileSizeChanged {})) = True
isSizeChange _ = False

pprFileChange :: FileChange -> String
pprFileChange (First  p) = "First  " ++ pprChange p
pprFileChange (Second p) = "Second " ++ pprChange p
pprFileChange (Change p) = "Change " ++ pprChange p

pprChange :: Change -> String
pprChange (DuplicateFile fp) = "Duplicate file: " ++ show fp
pprChange (ExtraFile fp) = "Extra file: " ++ show fp
pprChange (ExtraWay w) = "Extra way: " ++ show w
pprChange (ExtraThing t) = "Extra thing: " ++ show t
pprChange (ThingVersionChanged t v1 v2)
    = "Version changed for " ++ show t ++ ":\n"
   ++ "    " ++ v1 ++ "  ->  " ++ v2
pprChange (PermissionsChanged fp1 fp2 p1 p2)
    = "Permissions changed:\n"
   ++ "    " ++ show fp1
   ++ "    " ++ show fp2
   ++ "    " ++ p1 ++ "  ->  " ++ p2
pprChange (FileSizeChanged fp1 fp2 s1 s2)
    = "Size changed:\n"
   ++ "    " ++ show fp1 ++ "\n"
   ++ "    " ++ show fp2 ++ "\n"
   ++ "    " ++ show s1 ++ "  ->  " ++ show s2


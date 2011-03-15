
module Problem where

data FileProblem = First  Problem
                 | Second Problem
                 | Change Problem

data Problem = DuplicateFile FilePath
             | ExtraFile FilePath
             | PermissionsChanged FilePath FilePath String String
             | FileSizeChanged FilePath FilePath Integer Integer

pprFileProblem :: FileProblem -> String
pprFileProblem (First  p) = "First  " ++ pprProblem p
pprFileProblem (Second p) = "Second " ++ pprProblem p
pprFileProblem (Change p) = "Change " ++ pprProblem p

pprProblem :: Problem -> String
pprProblem (DuplicateFile fp) = "Duplicate file: " ++ show fp
pprProblem (ExtraFile fp) = "Extra file: " ++ show fp
pprProblem (PermissionsChanged fp1 fp2 p1 p2)
    = "Permissions changed:\n"
   ++ "    " ++ show fp1
   ++ "    " ++ show fp2
   ++ "    " ++ p1 ++ "  ->  " ++ p2
pprProblem (FileSizeChanged fp1 fp2 s1 s2)
    = "Size changed:\n"
   ++ "    " ++ show fp1 ++ "\n"
   ++ "    " ++ show fp2 ++ "\n"
   ++ "    " ++ show s1 ++ "  ->  " ++ show s2


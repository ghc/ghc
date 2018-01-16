

module DPH.Plugin where
import DPH.Pipeline
import GhcPlugins
import StaticFlags
import Debug.Trace
import System.IO.Unsafe

plugin :: Plugin
plugin  = defaultPlugin 
        { installCoreToDos = install }


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos 
 = do   
        -- Initialize the staticflags so that we can pretty print core code.
        --   The pretty printers depend on static flags and will `error` if 
        --   we don't do this first.
        unsafePerformIO
         $ do   addOpt "-dsuppress-all"
                addOpt "-dsuppress-idinfo"
                addOpt "-dsuppress-uniques"
                addOpt "-dppr-case-as-let"
                addOpt "-dppr-cols200"

                initStaticOpts
                return (return ())

        -- Flatten out the tree of passes into a list to make it easier to handle.
        let todos' = normalizeCoreDoPasses todos

        -- Check if this module is being vectorised.
        let hasVectorisation
                   = any isCoreDoVectorisation todos'

        -- If the vectoriser is enabled then replace the standard GHC pipeline
        -- with our own.
        if hasVectorisation
           then return vectoriserPipeline
           else return todos'



-- CoreToDo -------------------------------------------------------------------
-- | Flatten `CoreDoPasses` and drop out `CoreDoNothing` const
normalizeCoreDoPasses :: [CoreToDo] -> [CoreToDo]
normalizeCoreDoPasses cc
 = case cc of
        [] -> []

        CoreDoPasses cs1 : cs2
           -> normalizeCoreDoPasses (cs1 ++ cs2)

        CoreDoNothing : cs
           -> normalizeCoreDoPasses cs

        c : cs
           -> c : normalizeCoreDoPasses cs


-- | Check if a `CoreToDo` is `CoreDoVectorisation`
isCoreDoVectorisation :: CoreToDo -> Bool
isCoreDoVectorisation cc
 = case cc of
        CoreDoVectorisation     -> True
        _                       -> False


-- The Plugin -----------------------------------------------------------------
-- | The main DPH Plugin.
dphPluginPass :: PluginPass
dphPluginPass modGuts
        = trace "******** PASS"
        $ dumpCore modGuts

dumpCore :: ModGuts -> CoreM ModGuts 
dumpCore guts
 = unsafePerformIO
 $ do   putStrLn $ "*** GUTS " ++ show (length $ mg_binds guts)

        return (return guts)


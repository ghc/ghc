
module DPH.Pipeline 
        (vectoriserPipeline)
where
import DPH.Pass.Summon
import DPH.Pass.Dump
import GhcPlugins

-- DPH Compilation phase numbers. 
-- These are defined in dph-base:include/fusion-phases.h
dphPhaseClosures        = 4
dphPhasePA              = 3
dphPhaseBackend         = 2
dphPhaseStream          = 1
dphPhaseInner           = 0


-- | Our vectoriser pipeline.
--   This replaces the standard compilation pipeline defined in 
--   SimplCore.lhs of the main compiler.
vectoriserPipeline :: [CoreToDo]
vectoriserPipeline
 = [    
        -- The vectoriser requires the desugared code to be pre-simplified
        CoreDoSimplify 10
                SimplMode 
                { sm_names      = ["Vectorise", "PreSimplify"]
                , sm_phase      = InitialPhase
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = False
                , sm_case_case  = False }

        -- Run the vectoriser.
   ,    CoreDoVectorisation 
   ,    CoreDoPluginPass "Dump" (passDump "9-vectorised")

        ---------------------
        -- In the following stages we inline the different combinator
        -- libraries one after another to give their rewrite rules a
        -- chance to fire. Each stage reduces the abstraction level
        -- of the code.

        -- Inline lifted applications and closure constructors.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Closures"]
                , sm_phase      = Phase dphPhaseClosures
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

   ,    CoreDoPluginPass "Dump"   (passDump "4-closures")
   ,    CoreDoPluginPass "Summon"  passSummon
   ,    CoreDoPluginPass "Dump"   (passDump "4-closures-summoned")


        -- Inline PArray and PData combinators.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "PArray"]
                , sm_phase      = Phase dphPhasePA
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

   ,    CoreDoPluginPass "Dump" (passDump "3-parray")


        -- Inline unlifted backend.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Backend"]
                , sm_phase      = Phase dphPhaseBackend
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

   ,    CoreDoPluginPass "Dump" (passDump "2-backend")


        -- Inline stream functions.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Stream"]
                , sm_phase      = Phase dphPhaseStream
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

   ,    CoreDoPluginPass "Dump" (passDump "1-stream")

        -- Inline inner loops and everything else.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Inner"]
                , sm_phase      = Phase dphPhaseInner
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

   ,    CoreDoPluginPass "Dump" (passDump "0-inner")


        ---------------------
        -- From this point onwards we've got the code expressed as
        -- imperative-style loops, and need to optimise this low-level code.

        -- Do Worker/Wrapper to try to eliminate boxings and unboxings
        -- from recursive functions.
   ,    CoreDoStrictness
   ,    CoreDoWorkerWrapper
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Post Worker-Wrapper"]
                , sm_phase      = Phase 0
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True }

        -- Do Constructor Specialisation.
        -- Data.Vector code relies on this.
   ,    CoreDoSpecConstr
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Post SpecConstr"]
                , sm_phase      = Phase 0
                , sm_rules      = False
                , sm_eta_expand = False
                , sm_inline     = False
                , sm_case_case  = True }

        -- Do Floating and CSE to shift unboxings outwards and combine common
        -- constants. 
        -- We do this near the end because it moves around our 'What'
        -- justifications and makes the core code harder to read.
   ,    CoreDoFloatOutwards
                FloatOutSwitches
                { floatOutLambdas               = Nothing
                , floatOutConstants             = False 
                , floatOutPartialApplications   = False }
   ,    CoreCSE
   ,    CoreDoFloatInwards

   ,    CoreDoStrictness
   ,    CoreDoStaticArgs

        -- Final simplification.
   ,    CoreDoSimplify 20
                SimplMode
                { sm_names      = ["Vectorise", "Final"]
                , sm_phase      = Phase 0
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True }

   ,    CoreDoPluginPass "Dump"   (passDump "7-final")
   ]

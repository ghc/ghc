
module Config
        ( Config(..)
        , defaultConfig)
where

-- | Program config
data Config
        = Config {
        -- What solver to use
          configSolverName      :: String

        -- System setup
        , configBodyCount       :: Int
        , configBodyMass        :: Double
        , configTimeStep        :: Double
        , configEpsilon         :: Double

        -- Initial conditions.
        , configStartDiscSize   :: Double
        , configStartSpeed      :: Double

        -- Terminating conditions.
        , configMaxSteps        :: Int
        
        -- Dump points to file
        , configDumpFinal       :: Maybe FilePath 

        -- Print points to stdout
        , configPrintFinal      :: Bool
        
        -- Print timings to stdout
        , configPrintTimings    :: Bool }
        

defaultConfig :: Config
defaultConfig
        = Config
        { configSolverName      = "nested-bh"
        , configBodyCount       = 100
        , configBodyMass        = 10
        , configTimeStep        = 1
        , configEpsilon         = 100
        , configStartDiscSize   = 50
        , configStartSpeed      = 0.5
        , configMaxSteps        = 10
        , configDumpFinal       = Nothing
        , configPrintFinal      = True
        , configPrintTimings    = False }
        

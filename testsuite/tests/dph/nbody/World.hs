{-# LANGUAGE BangPatterns #-}

module World
        ( World(..)
        , advanceWorld)
where
import Body
import qualified Data.Vector.Unboxed            as V

data World 
        = World
        { -- | Bodies in the simulation.
          worldBodies   :: !(V.Vector Body)

          -- | Number of steps taken in the simulation so far.
        , worldSteps    :: !Int }


-- | Advance the world forward in time.
advanceWorld 
        :: (V.Vector MassPoint  -> V.Vector Accel)
                                -- ^ Fn to compute accelerations of each point.
        -> Double               -- ^ Time step.
        -> World
        -> World

advanceWorld calcAccels timeStep world
 = let  -- Calculate the accelerations on each body.
        accels  = calcAccels 
                $ V.map massPointOfBody 
                $ worldBodies world

        -- Apply the accelerations to the bodies and advance them.
        bodies' = V.zipWith 
                (\body (ax, ay) 
                        -> advanceBody timeStep
                                (setAccelOfBody (-ax, -ay) body))
                (worldBodies world)
                accels

        -- Update the world.
        steps'  = worldSteps world + 1

   in   world   { worldBodies   = bodies'
                , worldSteps    = steps' }

        
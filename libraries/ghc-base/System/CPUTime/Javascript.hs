{-# LANGUAGE JavaScriptFFI #-}

module System.CPUTime.Javascript
  ( getCPUTime
  , getCpuTimePrecision
  )
where

import qualified System.CPUTime.Unsupported as I

getCpuTimePrecision :: IO Integer
getCpuTimePrecision = toInteger <$> js_cpuTimePrecision

getCPUTime :: IO Integer
getCPUTime = do
  t <- js_getCPUTime
  if t == -1 then I.getCPUTime
             else pure (1000 * round t)

foreign import javascript unsafe
  "(() => { return h$cpuTimePrecision(); })"
  js_cpuTimePrecision :: IO Int

foreign import javascript unsafe
  "(() => { return h$getCPUTime(); })"
  js_getCPUTime :: IO Double

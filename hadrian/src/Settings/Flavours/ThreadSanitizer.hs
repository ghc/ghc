module Settings.Flavours.ThreadSanitizer (threadSanitizerFlavour) where

import Flavour
import Settings.Flavours.Validate

threadSanitizerFlavour :: Flavour
threadSanitizerFlavour =
  enableThreadSanitizer (validateFlavour
    { name = "thread-sanitizer" })

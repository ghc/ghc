module T9225 where
-- Should be a parse error:
-- version numbers not allowed in package qualified imports
import "some-package-0.1.2.3" Some.Module

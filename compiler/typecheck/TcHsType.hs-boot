module TcHsType where

import HsTypes   ( LHsType )
import Name      ( Name )
import TcType    ( TcKind, Type )
import TcRnMonad ( TcM )

tcCheckLHsType :: LHsType Name -> TcKind -> TcM Type

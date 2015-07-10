module TcHsType where

import HsTypes   ( LHsType )
import Name      ( Name )
import TcType    ( Kind, Type )
import TcRnMonad ( TcM )

tcHsTypeApp :: (LHsType Name, [Name]) -> Kind -> TcM Type

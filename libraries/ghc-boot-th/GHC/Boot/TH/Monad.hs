{-# LANGUAGE Safe, OverloadedRecordDot #-}
{-# OPTIONS_HADDOCK not-home #-}
module GHC.Boot.TH.Monad
  ( module GHC.Internal.TH.Monad
  , forceMetaHandlersArgs
  ) where

import GHC.Internal.TH.Monad

import Control.DeepSeq (rnf, liftRnf)
import Prelude (seq, (.), ($))

import GHC.Boot.TH.Syntax
  ( rnfName, rnfType, rnfDec, rnfAnnLookup, rnfModule, rnfForeignSrcLang
  , rnfExtension, rnfDocLoc )

-- See Note [Exceptions in TH]
forceMetaHandlersArgs :: MetaHandlers -> MetaHandlers
forceMetaHandlersArgs mh = MetaHandlers
  { mLiftIO = mLiftIO mh
  , mFail = forceArg rnf (mFail mh)
  , mNewName = forceArg rnf mh.mNewName
  , mReport = forceArg rnf (forceArg rnf . mh.mReport)

  , mRecover = mRecover mh
  , mLookupName = forceArg rnf (forceArg rnf . mh.mLookupName)
  , mReify = forceArg rnfName mh.mReify
  , mReifyFixity = forceArg rnfName mh.mReifyFixity
  , mReifyType = forceArg rnfName mh.mReifyType
  , mReifyInstances = forceArg rnfName (forceArg (liftRnf rnfType) . mh.mReifyInstances)
  , mReifyRoles = forceArg rnfName mh.mReifyRoles

  , mReifyAnnotations = forceArg rnfAnnLookup (mReifyAnnotations mh)
  , mReifyModule = forceArg rnfModule mh.mReifyModule
  , mReifyConStrictness = forceArg rnfName mh.mReifyConStrictness
  , mLocation = mh.mLocation
  , mGetPackageRoot = mh.mGetPackageRoot
  , mAddDependentFile = forceArg rnf mh.mAddDependentFile
  , mAddDependentDirectory = forceArg rnf mh.mAddDependentDirectory
  , mAddTempFile = forceArg rnf mh.mAddTempFile
  , mAddTopDecls = forceArg (liftRnf rnfDec) mh.mAddTopDecls
  , mAddForeignFilePath = forceArg rnfForeignSrcLang (forceArg rnf . mh.mAddForeignFilePath)
  , mAddModFinalizer = \ f -> seq f $ mh.mAddModFinalizer f
  , mAddCorePlugin = forceArg rnf mh.mAddCorePlugin
  , mGetQ = mGetQ mh
  , mPutQ = \ k -> seq k (mPutQ mh k)
  , mIsExtEnabled = forceArg rnfExtension mh.mIsExtEnabled
  , mExtsEnabled = mh.mExtsEnabled
  , mPutDoc = forceArg rnfDocLoc (forceArg rnf . mh.mPutDoc)
  , mGetDoc = forceArg rnfDocLoc mh.mGetDoc
  }
  where
    forceArg :: (a -> ()) -> (a -> b) -> a -> b
    forceArg rnf_arg f x = rnf_arg x `seq` f x

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Sum where

import Network.AWS.Prelude (NFData, FromXML(..), Text, Generic, Typeable, Data,
        UTCTime, ISO8601, ToQuery(..), NonEmpty, List1, ByteString, Base64, (.@?), parseXMLList,
        may, (.!@), _Coerce, _Default, _Time, (=:), (.@), toQueryList, parseXMLList1, _List1, _Base64,
        ToHeader(..), ToByteString(..), ToText(..), FromText(..), parseXMLText, fromTextError,
        takeLowerText
       )

data AccountAttributeName
  = DefaultVPC
  | SupportedPlatforms
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountAttributeName where
    parser = takeLowerText >>= \case
        "default-vpc" -> pure DefaultVPC
        "supported-platforms" -> pure SupportedPlatforms
        e -> fromTextError $ "Failure parsing AccountAttributeName from value: '" <> e
           <> "'. Accepted values: default-vpc, supported-platforms"

instance ToText AccountAttributeName where
    toText = \case
        DefaultVPC -> "default-vpc"
        SupportedPlatforms -> "supported-platforms"

instance NFData       AccountAttributeName
instance ToByteString AccountAttributeName
instance ToQuery      AccountAttributeName
instance ToHeader     AccountAttributeName

data ActivityStatus
  = ASError'
  | ASFulfilled
  | ASPendingFulfillment
  | ASPendingTermination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActivityStatus where
    parser = takeLowerText >>= \case
        "error" -> pure ASError'
        "fulfilled" -> pure ASFulfilled
        "pending_fulfillment" -> pure ASPendingFulfillment
        "pending_termination" -> pure ASPendingTermination
        e -> fromTextError $ "Failure parsing ActivityStatus from value: '" <> e
           <> "'. Accepted values: error, fulfilled, pending_fulfillment, pending_termination"

instance ToText ActivityStatus where
    toText = \case
        ASError' -> "error"
        ASFulfilled -> "fulfilled"
        ASPendingFulfillment -> "pending_fulfillment"
        ASPendingTermination -> "pending_termination"

instance NFData       ActivityStatus
instance ToByteString ActivityStatus
instance ToQuery      ActivityStatus
instance ToHeader     ActivityStatus

instance FromXML ActivityStatus where
    parseXML = parseXMLText "ActivityStatus"

data AddressStatus
  = InClassic
  | InVPC
  | MoveInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AddressStatus where
    parser = takeLowerText >>= \case
        "inclassic" -> pure InClassic
        "invpc" -> pure InVPC
        "moveinprogress" -> pure MoveInProgress
        e -> fromTextError $ "Failure parsing AddressStatus from value: '" <> e
           <> "'. Accepted values: inclassic, invpc, moveinprogress"

instance ToText AddressStatus where
    toText = \case
        InClassic -> "InClassic"
        InVPC -> "InVpc"
        MoveInProgress -> "MoveInProgress"

instance NFData       AddressStatus
instance ToByteString AddressStatus
instance ToQuery      AddressStatus
instance ToHeader     AddressStatus

instance FromXML AddressStatus where
    parseXML = parseXMLText "AddressStatus"

data Affinity
  = ADefault
  | AHost
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Affinity where
    parser = takeLowerText >>= \case
        "default" -> pure ADefault
        "host" -> pure AHost
        e -> fromTextError $ "Failure parsing Affinity from value: '" <> e
           <> "'. Accepted values: default, host"

instance ToText Affinity where
    toText = \case
        ADefault -> "default"
        AHost -> "host"

instance NFData       Affinity
instance ToByteString Affinity
instance ToQuery      Affinity
instance ToHeader     Affinity

data AllocationState
  = ASAvailable
  | ASPermanentFailure
  | ASReleased
  | ASReleasedPermanentFailure
  | ASUnderAssessment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AllocationState where
    parser = takeLowerText >>= \case
        "available" -> pure ASAvailable
        "permanent-failure" -> pure ASPermanentFailure
        "released" -> pure ASReleased
        "released-permanent-failure" -> pure ASReleasedPermanentFailure
        "under-assessment" -> pure ASUnderAssessment
        e -> fromTextError $ "Failure parsing AllocationState from value: '" <> e
           <> "'. Accepted values: available, permanent-failure, released, released-permanent-failure, under-assessment"

instance ToText AllocationState where
    toText = \case
        ASAvailable -> "available"
        ASPermanentFailure -> "permanent-failure"
        ASReleased -> "released"
        ASReleasedPermanentFailure -> "released-permanent-failure"
        ASUnderAssessment -> "under-assessment"

instance NFData       AllocationState
instance ToByteString AllocationState
instance ToQuery      AllocationState
instance ToHeader     AllocationState

instance FromXML AllocationState where
    parseXML = parseXMLText "AllocationState"

data AllocationStrategy
  = ASDiversified
  | ASLowestPrice
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AllocationStrategy where
    parser = takeLowerText >>= \case
        "diversified" -> pure ASDiversified
        "lowestprice" -> pure ASLowestPrice
        e -> fromTextError $ "Failure parsing AllocationStrategy from value: '" <> e
           <> "'. Accepted values: diversified, lowestprice"

instance ToText AllocationStrategy where
    toText = \case
        ASDiversified -> "diversified"
        ASLowestPrice -> "lowestPrice"

instance NFData       AllocationStrategy
instance ToByteString AllocationStrategy
instance ToQuery      AllocationStrategy
instance ToHeader     AllocationStrategy

instance FromXML AllocationStrategy where
    parseXML = parseXMLText "AllocationStrategy"

data ArchitectureValues
  = I386
  | X86_64
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArchitectureValues where
    parser = takeLowerText >>= \case
        "i386" -> pure I386
        "x86_64" -> pure X86_64
        e -> fromTextError $ "Failure parsing ArchitectureValues from value: '" <> e
           <> "'. Accepted values: i386, x86_64"

instance ToText ArchitectureValues where
    toText = \case
        I386 -> "i386"
        X86_64 -> "x86_64"

instance NFData       ArchitectureValues
instance ToByteString ArchitectureValues
instance ToQuery      ArchitectureValues
instance ToHeader     ArchitectureValues

instance FromXML ArchitectureValues where
    parseXML = parseXMLText "ArchitectureValues"

data AttachmentStatus
  = AAttached
  | AAttaching
  | AAvailable
  | ABusy
  | ADetached
  | ADetaching
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AttachmentStatus where
    parser = takeLowerText >>= \case
        "attached" -> pure AAttached
        "attaching" -> pure AAttaching
        "available" -> pure AAvailable
        "busy" -> pure ABusy
        "detached" -> pure ADetached
        "detaching" -> pure ADetaching
        e -> fromTextError $ "Failure parsing AttachmentStatus from value: '" <> e
           <> "'. Accepted values: attached, attaching, available, busy, detached, detaching"

instance ToText AttachmentStatus where
    toText = \case
        AAttached -> "attached"
        AAttaching -> "attaching"
        AAvailable -> "available"
        ABusy -> "busy"
        ADetached -> "detached"
        ADetaching -> "detaching"

instance NFData       AttachmentStatus
instance ToByteString AttachmentStatus
instance ToQuery      AttachmentStatus
instance ToHeader     AttachmentStatus

instance FromXML AttachmentStatus where
    parseXML = parseXMLText "AttachmentStatus"

data AutoPlacement
  = ON
  | Off
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoPlacement where
    parser = takeLowerText >>= \case
        "on" -> pure ON
        "off" -> pure Off
        e -> fromTextError $ "Failure parsing AutoPlacement from value: '" <> e
           <> "'. Accepted values: on, off"

instance ToText AutoPlacement where
    toText = \case
        ON -> "on"
        Off -> "off"

instance NFData       AutoPlacement
instance ToByteString AutoPlacement
instance ToQuery      AutoPlacement
instance ToHeader     AutoPlacement

instance FromXML AutoPlacement where
    parseXML = parseXMLText "AutoPlacement"

data AvailabilityZoneState
  = AZSAvailable
  | AZSImpaired
  | AZSInformation
  | AZSUnavailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AvailabilityZoneState where
    parser = takeLowerText >>= \case
        "available" -> pure AZSAvailable
        "impaired" -> pure AZSImpaired
        "information" -> pure AZSInformation
        "unavailable" -> pure AZSUnavailable
        e -> fromTextError $ "Failure parsing AvailabilityZoneState from value: '" <> e
           <> "'. Accepted values: available, impaired, information, unavailable"

instance ToText AvailabilityZoneState where
    toText = \case
        AZSAvailable -> "available"
        AZSImpaired -> "impaired"
        AZSInformation -> "information"
        AZSUnavailable -> "unavailable"

instance NFData       AvailabilityZoneState
instance ToByteString AvailabilityZoneState
instance ToQuery      AvailabilityZoneState
instance ToHeader     AvailabilityZoneState

instance FromXML AvailabilityZoneState where
    parseXML = parseXMLText "AvailabilityZoneState"

data BatchState
  = BSActive
  | BSCancelled
  | BSCancelledRunning
  | BSCancelledTerminating
  | BSFailed
  | BSModifying
  | BSSubmitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BatchState where
    parser = takeLowerText >>= \case
        "active" -> pure BSActive
        "cancelled" -> pure BSCancelled
        "cancelled_running" -> pure BSCancelledRunning
        "cancelled_terminating" -> pure BSCancelledTerminating
        "failed" -> pure BSFailed
        "modifying" -> pure BSModifying
        "submitted" -> pure BSSubmitted
        e -> fromTextError $ "Failure parsing BatchState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelled_running, cancelled_terminating, failed, modifying, submitted"

instance ToText BatchState where
    toText = \case
        BSActive -> "active"
        BSCancelled -> "cancelled"
        BSCancelledRunning -> "cancelled_running"
        BSCancelledTerminating -> "cancelled_terminating"
        BSFailed -> "failed"
        BSModifying -> "modifying"
        BSSubmitted -> "submitted"

instance NFData       BatchState
instance ToByteString BatchState
instance ToQuery      BatchState
instance ToHeader     BatchState

instance FromXML BatchState where
    parseXML = parseXMLText "BatchState"

data BundleTaskState
  = BTSBundling
  | BTSCancelling
  | BTSComplete
  | BTSFailed
  | BTSPending
  | BTSStoring
  | BTSWaitingForShutdown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BundleTaskState where
    parser = takeLowerText >>= \case
        "bundling" -> pure BTSBundling
        "cancelling" -> pure BTSCancelling
        "complete" -> pure BTSComplete
        "failed" -> pure BTSFailed
        "pending" -> pure BTSPending
        "storing" -> pure BTSStoring
        "waiting-for-shutdown" -> pure BTSWaitingForShutdown
        e -> fromTextError $ "Failure parsing BundleTaskState from value: '" <> e
           <> "'. Accepted values: bundling, cancelling, complete, failed, pending, storing, waiting-for-shutdown"

instance ToText BundleTaskState where
    toText = \case
        BTSBundling -> "bundling"
        BTSCancelling -> "cancelling"
        BTSComplete -> "complete"
        BTSFailed -> "failed"
        BTSPending -> "pending"
        BTSStoring -> "storing"
        BTSWaitingForShutdown -> "waiting-for-shutdown"

instance NFData       BundleTaskState
instance ToByteString BundleTaskState
instance ToQuery      BundleTaskState
instance ToHeader     BundleTaskState

instance FromXML BundleTaskState where
    parseXML = parseXMLText "BundleTaskState"

data CancelBatchErrorCode
  = CBECFleetRequestIdDoesNotExist
  | CBECFleetRequestIdMalformed
  | CBECFleetRequestNotInCancellableState
  | CBECUnexpectedError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CancelBatchErrorCode where
    parser = takeLowerText >>= \case
        "fleetrequestiddoesnotexist" -> pure CBECFleetRequestIdDoesNotExist
        "fleetrequestidmalformed" -> pure CBECFleetRequestIdMalformed
        "fleetrequestnotincancellablestate" -> pure CBECFleetRequestNotInCancellableState
        "unexpectederror" -> pure CBECUnexpectedError
        e -> fromTextError $ "Failure parsing CancelBatchErrorCode from value: '" <> e
           <> "'. Accepted values: fleetrequestiddoesnotexist, fleetrequestidmalformed, fleetrequestnotincancellablestate, unexpectederror"

instance ToText CancelBatchErrorCode where
    toText = \case
        CBECFleetRequestIdDoesNotExist -> "fleetRequestIdDoesNotExist"
        CBECFleetRequestIdMalformed -> "fleetRequestIdMalformed"
        CBECFleetRequestNotInCancellableState -> "fleetRequestNotInCancellableState"
        CBECUnexpectedError -> "unexpectedError"

instance NFData       CancelBatchErrorCode
instance ToByteString CancelBatchErrorCode
instance ToQuery      CancelBatchErrorCode
instance ToHeader     CancelBatchErrorCode

instance FromXML CancelBatchErrorCode where
    parseXML = parseXMLText "CancelBatchErrorCode"

data CancelSpotInstanceRequestState
  = CSIRSActive
  | CSIRSCancelled
  | CSIRSClosed
  | CSIRSCompleted
  | CSIRSOpen
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CancelSpotInstanceRequestState where
    parser = takeLowerText >>= \case
        "active" -> pure CSIRSActive
        "cancelled" -> pure CSIRSCancelled
        "closed" -> pure CSIRSClosed
        "completed" -> pure CSIRSCompleted
        "open" -> pure CSIRSOpen
        e -> fromTextError $ "Failure parsing CancelSpotInstanceRequestState from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, completed, open"

instance ToText CancelSpotInstanceRequestState where
    toText = \case
        CSIRSActive -> "active"
        CSIRSCancelled -> "cancelled"
        CSIRSClosed -> "closed"
        CSIRSCompleted -> "completed"
        CSIRSOpen -> "open"

instance NFData       CancelSpotInstanceRequestState
instance ToByteString CancelSpotInstanceRequestState
instance ToQuery      CancelSpotInstanceRequestState
instance ToHeader     CancelSpotInstanceRequestState

instance FromXML CancelSpotInstanceRequestState where
    parseXML = parseXMLText "CancelSpotInstanceRequestState"

data ConnectionNotificationState
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionNotificationState where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing ConnectionNotificationState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText ConnectionNotificationState where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"

instance NFData       ConnectionNotificationState
instance ToByteString ConnectionNotificationState
instance ToQuery      ConnectionNotificationState
instance ToHeader     ConnectionNotificationState

instance FromXML ConnectionNotificationState where
    parseXML = parseXMLText "ConnectionNotificationState"

data ConnectionNotificationType =
  Topic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConnectionNotificationType where
    parser = takeLowerText >>= \case
        "topic" -> pure Topic
        e -> fromTextError $ "Failure parsing ConnectionNotificationType from value: '" <> e
           <> "'. Accepted values: topic"

instance ToText ConnectionNotificationType where
    toText = \case
        Topic -> "Topic"

instance NFData       ConnectionNotificationType
instance ToByteString ConnectionNotificationType
instance ToQuery      ConnectionNotificationType
instance ToHeader     ConnectionNotificationType

instance FromXML ConnectionNotificationType where
    parseXML = parseXMLText "ConnectionNotificationType"

data ContainerFormat =
  Ova
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContainerFormat where
    parser = takeLowerText >>= \case
        "ova" -> pure Ova
        e -> fromTextError $ "Failure parsing ContainerFormat from value: '" <> e
           <> "'. Accepted values: ova"

instance ToText ContainerFormat where
    toText = \case
        Ova -> "ova"

instance NFData       ContainerFormat
instance ToByteString ContainerFormat
instance ToQuery      ContainerFormat
instance ToHeader     ContainerFormat

instance FromXML ContainerFormat where
    parseXML = parseXMLText "ContainerFormat"

data ConversionTaskState
  = CTSActive
  | CTSCancelled
  | CTSCancelling
  | CTSCompleted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConversionTaskState where
    parser = takeLowerText >>= \case
        "active" -> pure CTSActive
        "cancelled" -> pure CTSCancelled
        "cancelling" -> pure CTSCancelling
        "completed" -> pure CTSCompleted
        e -> fromTextError $ "Failure parsing ConversionTaskState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelling, completed"

instance ToText ConversionTaskState where
    toText = \case
        CTSActive -> "active"
        CTSCancelled -> "cancelled"
        CTSCancelling -> "cancelling"
        CTSCompleted -> "completed"

instance NFData       ConversionTaskState
instance ToByteString ConversionTaskState
instance ToQuery      ConversionTaskState
instance ToHeader     ConversionTaskState

instance FromXML ConversionTaskState where
    parseXML = parseXMLText "ConversionTaskState"

data CurrencyCodeValues =
  Usd
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CurrencyCodeValues where
    parser = takeLowerText >>= \case
        "usd" -> pure Usd
        e -> fromTextError $ "Failure parsing CurrencyCodeValues from value: '" <> e
           <> "'. Accepted values: usd"

instance ToText CurrencyCodeValues where
    toText = \case
        Usd -> "USD"

instance NFData       CurrencyCodeValues
instance ToByteString CurrencyCodeValues
instance ToQuery      CurrencyCodeValues
instance ToHeader     CurrencyCodeValues

instance FromXML CurrencyCodeValues where
    parseXML = parseXMLText "CurrencyCodeValues"

data DatafeedSubscriptionState
  = DSSActive
  | DSSInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DatafeedSubscriptionState where
    parser = takeLowerText >>= \case
        "active" -> pure DSSActive
        "inactive" -> pure DSSInactive
        e -> fromTextError $ "Failure parsing DatafeedSubscriptionState from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText DatafeedSubscriptionState where
    toText = \case
        DSSActive -> "Active"
        DSSInactive -> "Inactive"

instance NFData       DatafeedSubscriptionState
instance ToByteString DatafeedSubscriptionState
instance ToQuery      DatafeedSubscriptionState
instance ToHeader     DatafeedSubscriptionState

instance FromXML DatafeedSubscriptionState where
    parseXML = parseXMLText "DatafeedSubscriptionState"

data DefaultTargetCapacityType
  = DTCTOnDemand
  | DTCTSpot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DefaultTargetCapacityType where
    parser = takeLowerText >>= \case
        "on-demand" -> pure DTCTOnDemand
        "spot" -> pure DTCTSpot
        e -> fromTextError $ "Failure parsing DefaultTargetCapacityType from value: '" <> e
           <> "'. Accepted values: on-demand, spot"

instance ToText DefaultTargetCapacityType where
    toText = \case
        DTCTOnDemand -> "on-demand"
        DTCTSpot -> "spot"

instance NFData       DefaultTargetCapacityType
instance ToByteString DefaultTargetCapacityType
instance ToQuery      DefaultTargetCapacityType
instance ToHeader     DefaultTargetCapacityType

instance FromXML DefaultTargetCapacityType where
    parseXML = parseXMLText "DefaultTargetCapacityType"

data DeleteFleetErrorCode
  = DFECFleetIdDoesNotExist
  | DFECFleetIdMalformed
  | DFECFleetNotInDeletableState
  | DFECUnexpectedError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeleteFleetErrorCode where
    parser = takeLowerText >>= \case
        "fleetiddoesnotexist" -> pure DFECFleetIdDoesNotExist
        "fleetidmalformed" -> pure DFECFleetIdMalformed
        "fleetnotindeletablestate" -> pure DFECFleetNotInDeletableState
        "unexpectederror" -> pure DFECUnexpectedError
        e -> fromTextError $ "Failure parsing DeleteFleetErrorCode from value: '" <> e
           <> "'. Accepted values: fleetiddoesnotexist, fleetidmalformed, fleetnotindeletablestate, unexpectederror"

instance ToText DeleteFleetErrorCode where
    toText = \case
        DFECFleetIdDoesNotExist -> "fleetIdDoesNotExist"
        DFECFleetIdMalformed -> "fleetIdMalformed"
        DFECFleetNotInDeletableState -> "fleetNotInDeletableState"
        DFECUnexpectedError -> "unexpectedError"

instance NFData       DeleteFleetErrorCode
instance ToByteString DeleteFleetErrorCode
instance ToQuery      DeleteFleetErrorCode
instance ToHeader     DeleteFleetErrorCode

instance FromXML DeleteFleetErrorCode where
    parseXML = parseXMLText "DeleteFleetErrorCode"

data DeviceType
  = EBS
  | InstanceStore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeviceType where
    parser = takeLowerText >>= \case
        "ebs" -> pure EBS
        "instance-store" -> pure InstanceStore
        e -> fromTextError $ "Failure parsing DeviceType from value: '" <> e
           <> "'. Accepted values: ebs, instance-store"

instance ToText DeviceType where
    toText = \case
        EBS -> "ebs"
        InstanceStore -> "instance-store"

instance NFData       DeviceType
instance ToByteString DeviceType
instance ToQuery      DeviceType
instance ToHeader     DeviceType

instance FromXML DeviceType where
    parseXML = parseXMLText "DeviceType"

data DiskImageFormat
  = Raw
  | VHD
  | VMDK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DiskImageFormat where
    parser = takeLowerText >>= \case
        "raw" -> pure Raw
        "vhd" -> pure VHD
        "vmdk" -> pure VMDK
        e -> fromTextError $ "Failure parsing DiskImageFormat from value: '" <> e
           <> "'. Accepted values: raw, vhd, vmdk"

instance ToText DiskImageFormat where
    toText = \case
        Raw -> "RAW"
        VHD -> "VHD"
        VMDK -> "VMDK"

instance NFData       DiskImageFormat
instance ToByteString DiskImageFormat
instance ToQuery      DiskImageFormat
instance ToHeader     DiskImageFormat

instance FromXML DiskImageFormat where
    parseXML = parseXMLText "DiskImageFormat"

data DomainType
  = DTStandard
  | DTVPC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DomainType where
    parser = takeLowerText >>= \case
        "standard" -> pure DTStandard
        "vpc" -> pure DTVPC
        e -> fromTextError $ "Failure parsing DomainType from value: '" <> e
           <> "'. Accepted values: standard, vpc"

instance ToText DomainType where
    toText = \case
        DTStandard -> "standard"
        DTVPC -> "vpc"

instance NFData       DomainType
instance ToByteString DomainType
instance ToQuery      DomainType
instance ToHeader     DomainType

instance FromXML DomainType where
    parseXML = parseXMLText "DomainType"

data ElasticGpuState =
  Attached
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ElasticGpuState where
    parser = takeLowerText >>= \case
        "attached" -> pure Attached
        e -> fromTextError $ "Failure parsing ElasticGpuState from value: '" <> e
           <> "'. Accepted values: attached"

instance ToText ElasticGpuState where
    toText = \case
        Attached -> "ATTACHED"

instance NFData       ElasticGpuState
instance ToByteString ElasticGpuState
instance ToQuery      ElasticGpuState
instance ToHeader     ElasticGpuState

instance FromXML ElasticGpuState where
    parseXML = parseXMLText "ElasticGpuState"

data ElasticGpuStatus
  = EGSImpaired
  | EGSOK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ElasticGpuStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure EGSImpaired
        "ok" -> pure EGSOK
        e -> fromTextError $ "Failure parsing ElasticGpuStatus from value: '" <> e
           <> "'. Accepted values: impaired, ok"

instance ToText ElasticGpuStatus where
    toText = \case
        EGSImpaired -> "IMPAIRED"
        EGSOK -> "OK"

instance NFData       ElasticGpuStatus
instance ToByteString ElasticGpuStatus
instance ToQuery      ElasticGpuStatus
instance ToHeader     ElasticGpuStatus

instance FromXML ElasticGpuStatus where
    parseXML = parseXMLText "ElasticGpuStatus"

data EventCode
  = InstanceReboot
  | InstanceRetirement
  | InstanceStop
  | SystemMaintenance
  | SystemReboot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventCode where
    parser = takeLowerText >>= \case
        "instance-reboot" -> pure InstanceReboot
        "instance-retirement" -> pure InstanceRetirement
        "instance-stop" -> pure InstanceStop
        "system-maintenance" -> pure SystemMaintenance
        "system-reboot" -> pure SystemReboot
        e -> fromTextError $ "Failure parsing EventCode from value: '" <> e
           <> "'. Accepted values: instance-reboot, instance-retirement, instance-stop, system-maintenance, system-reboot"

instance ToText EventCode where
    toText = \case
        InstanceReboot -> "instance-reboot"
        InstanceRetirement -> "instance-retirement"
        InstanceStop -> "instance-stop"
        SystemMaintenance -> "system-maintenance"
        SystemReboot -> "system-reboot"

instance NFData       EventCode
instance ToByteString EventCode
instance ToQuery      EventCode
instance ToHeader     EventCode

instance FromXML EventCode where
    parseXML = parseXMLText "EventCode"

data EventType
  = ETError'
  | ETFleetRequestChange
  | ETInstanceChange
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventType where
    parser = takeLowerText >>= \case
        "error" -> pure ETError'
        "fleetrequestchange" -> pure ETFleetRequestChange
        "instancechange" -> pure ETInstanceChange
        e -> fromTextError $ "Failure parsing EventType from value: '" <> e
           <> "'. Accepted values: error, fleetrequestchange, instancechange"

instance ToText EventType where
    toText = \case
        ETError' -> "error"
        ETFleetRequestChange -> "fleetRequestChange"
        ETInstanceChange -> "instanceChange"

instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance FromXML EventType where
    parseXML = parseXMLText "EventType"

data ExcessCapacityTerminationPolicy
  = ECTPDefault
  | ECTPNoTermination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExcessCapacityTerminationPolicy where
    parser = takeLowerText >>= \case
        "default" -> pure ECTPDefault
        "notermination" -> pure ECTPNoTermination
        e -> fromTextError $ "Failure parsing ExcessCapacityTerminationPolicy from value: '" <> e
           <> "'. Accepted values: default, notermination"

instance ToText ExcessCapacityTerminationPolicy where
    toText = \case
        ECTPDefault -> "default"
        ECTPNoTermination -> "noTermination"

instance NFData       ExcessCapacityTerminationPolicy
instance ToByteString ExcessCapacityTerminationPolicy
instance ToQuery      ExcessCapacityTerminationPolicy
instance ToHeader     ExcessCapacityTerminationPolicy

instance FromXML ExcessCapacityTerminationPolicy where
    parseXML = parseXMLText "ExcessCapacityTerminationPolicy"

data ExportEnvironment
  = Citrix
  | Microsoft
  | VMware
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportEnvironment where
    parser = takeLowerText >>= \case
        "citrix" -> pure Citrix
        "microsoft" -> pure Microsoft
        "vmware" -> pure VMware
        e -> fromTextError $ "Failure parsing ExportEnvironment from value: '" <> e
           <> "'. Accepted values: citrix, microsoft, vmware"

instance ToText ExportEnvironment where
    toText = \case
        Citrix -> "citrix"
        Microsoft -> "microsoft"
        VMware -> "vmware"

instance NFData       ExportEnvironment
instance ToByteString ExportEnvironment
instance ToQuery      ExportEnvironment
instance ToHeader     ExportEnvironment

instance FromXML ExportEnvironment where
    parseXML = parseXMLText "ExportEnvironment"

data ExportTaskState
  = ETSActive
  | ETSCancelled
  | ETSCancelling
  | ETSCompleted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportTaskState where
    parser = takeLowerText >>= \case
        "active" -> pure ETSActive
        "cancelled" -> pure ETSCancelled
        "cancelling" -> pure ETSCancelling
        "completed" -> pure ETSCompleted
        e -> fromTextError $ "Failure parsing ExportTaskState from value: '" <> e
           <> "'. Accepted values: active, cancelled, cancelling, completed"

instance ToText ExportTaskState where
    toText = \case
        ETSActive -> "active"
        ETSCancelled -> "cancelled"
        ETSCancelling -> "cancelling"
        ETSCompleted -> "completed"

instance NFData       ExportTaskState
instance ToByteString ExportTaskState
instance ToQuery      ExportTaskState
instance ToHeader     ExportTaskState

instance FromXML ExportTaskState where
    parseXML = parseXMLText "ExportTaskState"

data FleetActivityStatus
  = Error'
  | Fulfilled
  | PendingFulfillment
  | PendingTermination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetActivityStatus where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "fulfilled" -> pure Fulfilled
        "pending-fulfillment" -> pure PendingFulfillment
        "pending-termination" -> pure PendingTermination
        e -> fromTextError $ "Failure parsing FleetActivityStatus from value: '" <> e
           <> "'. Accepted values: error, fulfilled, pending-fulfillment, pending-termination"

instance ToText FleetActivityStatus where
    toText = \case
        Error' -> "error"
        Fulfilled -> "fulfilled"
        PendingFulfillment -> "pending-fulfillment"
        PendingTermination -> "pending-termination"

instance NFData       FleetActivityStatus
instance ToByteString FleetActivityStatus
instance ToQuery      FleetActivityStatus
instance ToHeader     FleetActivityStatus

instance FromXML FleetActivityStatus where
    parseXML = parseXMLText "FleetActivityStatus"

data FleetEventType
  = FETFleetChange
  | FETInstanceChange
  | FETServiceError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetEventType where
    parser = takeLowerText >>= \case
        "fleet-change" -> pure FETFleetChange
        "instance-change" -> pure FETInstanceChange
        "service-error" -> pure FETServiceError
        e -> fromTextError $ "Failure parsing FleetEventType from value: '" <> e
           <> "'. Accepted values: fleet-change, instance-change, service-error"

instance ToText FleetEventType where
    toText = \case
        FETFleetChange -> "fleet-change"
        FETInstanceChange -> "instance-change"
        FETServiceError -> "service-error"

instance NFData       FleetEventType
instance ToByteString FleetEventType
instance ToQuery      FleetEventType
instance ToHeader     FleetEventType

instance FromXML FleetEventType where
    parseXML = parseXMLText "FleetEventType"

data FleetExcessCapacityTerminationPolicy
  = NoTermination
  | Termination
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetExcessCapacityTerminationPolicy where
    parser = takeLowerText >>= \case
        "no-termination" -> pure NoTermination
        "termination" -> pure Termination
        e -> fromTextError $ "Failure parsing FleetExcessCapacityTerminationPolicy from value: '" <> e
           <> "'. Accepted values: no-termination, termination"

instance ToText FleetExcessCapacityTerminationPolicy where
    toText = \case
        NoTermination -> "no-termination"
        Termination -> "termination"

instance NFData       FleetExcessCapacityTerminationPolicy
instance ToByteString FleetExcessCapacityTerminationPolicy
instance ToQuery      FleetExcessCapacityTerminationPolicy
instance ToHeader     FleetExcessCapacityTerminationPolicy

instance FromXML FleetExcessCapacityTerminationPolicy where
    parseXML = parseXMLText "FleetExcessCapacityTerminationPolicy"

data FleetStateCode
  = FSCActive
  | FSCDeleted
  | FSCDeletedRunning
  | FSCDeletedTerminating
  | FSCFailed
  | FSCModifying
  | FSCSubmitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetStateCode where
    parser = takeLowerText >>= \case
        "active" -> pure FSCActive
        "deleted" -> pure FSCDeleted
        "deleted-running" -> pure FSCDeletedRunning
        "deleted-terminating" -> pure FSCDeletedTerminating
        "failed" -> pure FSCFailed
        "modifying" -> pure FSCModifying
        "submitted" -> pure FSCSubmitted
        e -> fromTextError $ "Failure parsing FleetStateCode from value: '" <> e
           <> "'. Accepted values: active, deleted, deleted-running, deleted-terminating, failed, modifying, submitted"

instance ToText FleetStateCode where
    toText = \case
        FSCActive -> "active"
        FSCDeleted -> "deleted"
        FSCDeletedRunning -> "deleted-running"
        FSCDeletedTerminating -> "deleted-terminating"
        FSCFailed -> "failed"
        FSCModifying -> "modifying"
        FSCSubmitted -> "submitted"

instance NFData       FleetStateCode
instance ToByteString FleetStateCode
instance ToQuery      FleetStateCode
instance ToHeader     FleetStateCode

instance FromXML FleetStateCode where
    parseXML = parseXMLText "FleetStateCode"

data FleetType
  = FTMaintain
  | FTRequest
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetType where
    parser = takeLowerText >>= \case
        "maintain" -> pure FTMaintain
        "request" -> pure FTRequest
        e -> fromTextError $ "Failure parsing FleetType from value: '" <> e
           <> "'. Accepted values: maintain, request"

instance ToText FleetType where
    toText = \case
        FTMaintain -> "maintain"
        FTRequest -> "request"

instance NFData       FleetType
instance ToByteString FleetType
instance ToQuery      FleetType
instance ToHeader     FleetType

instance FromXML FleetType where
    parseXML = parseXMLText "FleetType"

data FlowLogsResourceType
  = FLRTNetworkInterface
  | FLRTSubnet
  | FLRTVPC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FlowLogsResourceType where
    parser = takeLowerText >>= \case
        "networkinterface" -> pure FLRTNetworkInterface
        "subnet" -> pure FLRTSubnet
        "vpc" -> pure FLRTVPC
        e -> fromTextError $ "Failure parsing FlowLogsResourceType from value: '" <> e
           <> "'. Accepted values: networkinterface, subnet, vpc"

instance ToText FlowLogsResourceType where
    toText = \case
        FLRTNetworkInterface -> "NetworkInterface"
        FLRTSubnet -> "Subnet"
        FLRTVPC -> "VPC"

instance NFData       FlowLogsResourceType
instance ToByteString FlowLogsResourceType
instance ToQuery      FlowLogsResourceType
instance ToHeader     FlowLogsResourceType

data FpgaImageAttributeName
  = FIANDescription
  | FIANLoadPermission
  | FIANName
  | FIANProductCodes
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FpgaImageAttributeName where
    parser = takeLowerText >>= \case
        "description" -> pure FIANDescription
        "loadpermission" -> pure FIANLoadPermission
        "name" -> pure FIANName
        "productcodes" -> pure FIANProductCodes
        e -> fromTextError $ "Failure parsing FpgaImageAttributeName from value: '" <> e
           <> "'. Accepted values: description, loadpermission, name, productcodes"

instance ToText FpgaImageAttributeName where
    toText = \case
        FIANDescription -> "description"
        FIANLoadPermission -> "loadPermission"
        FIANName -> "name"
        FIANProductCodes -> "productCodes"

instance NFData       FpgaImageAttributeName
instance ToByteString FpgaImageAttributeName
instance ToQuery      FpgaImageAttributeName
instance ToHeader     FpgaImageAttributeName

data FpgaImageStateCode
  = FISCAvailable
  | FISCFailed
  | FISCPending
  | FISCUnavailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FpgaImageStateCode where
    parser = takeLowerText >>= \case
        "available" -> pure FISCAvailable
        "failed" -> pure FISCFailed
        "pending" -> pure FISCPending
        "unavailable" -> pure FISCUnavailable
        e -> fromTextError $ "Failure parsing FpgaImageStateCode from value: '" <> e
           <> "'. Accepted values: available, failed, pending, unavailable"

instance ToText FpgaImageStateCode where
    toText = \case
        FISCAvailable -> "available"
        FISCFailed -> "failed"
        FISCPending -> "pending"
        FISCUnavailable -> "unavailable"

instance NFData       FpgaImageStateCode
instance ToByteString FpgaImageStateCode
instance ToQuery      FpgaImageStateCode
instance ToHeader     FpgaImageStateCode

instance FromXML FpgaImageStateCode where
    parseXML = parseXMLText "FpgaImageStateCode"

data GatewayType =
  IPsec_1
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GatewayType where
    parser = takeLowerText >>= \case
        "ipsec.1" -> pure IPsec_1
        e -> fromTextError $ "Failure parsing GatewayType from value: '" <> e
           <> "'. Accepted values: ipsec.1"

instance ToText GatewayType where
    toText = \case
        IPsec_1 -> "ipsec.1"

instance NFData       GatewayType
instance ToByteString GatewayType
instance ToQuery      GatewayType
instance ToHeader     GatewayType

instance FromXML GatewayType where
    parseXML = parseXMLText "GatewayType"

data HostTenancy
  = HTDedicated
  | HTHost
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HostTenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure HTDedicated
        "host" -> pure HTHost
        e -> fromTextError $ "Failure parsing HostTenancy from value: '" <> e
           <> "'. Accepted values: dedicated, host"

instance ToText HostTenancy where
    toText = \case
        HTDedicated -> "dedicated"
        HTHost -> "host"

instance NFData       HostTenancy
instance ToByteString HostTenancy
instance ToQuery      HostTenancy
instance ToHeader     HostTenancy

data HypervisorType
  = Ovm
  | Xen
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HypervisorType where
    parser = takeLowerText >>= \case
        "ovm" -> pure Ovm
        "xen" -> pure Xen
        e -> fromTextError $ "Failure parsing HypervisorType from value: '" <> e
           <> "'. Accepted values: ovm, xen"

instance ToText HypervisorType where
    toText = \case
        Ovm -> "ovm"
        Xen -> "xen"

instance NFData       HypervisorType
instance ToByteString HypervisorType
instance ToQuery      HypervisorType
instance ToHeader     HypervisorType

instance FromXML HypervisorType where
    parseXML = parseXMLText "HypervisorType"

data IAMInstanceProfileAssociationState
  = Associated
  | Associating
  | Disassociated
  | Disassociating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IAMInstanceProfileAssociationState where
    parser = takeLowerText >>= \case
        "associated" -> pure Associated
        "associating" -> pure Associating
        "disassociated" -> pure Disassociated
        "disassociating" -> pure Disassociating
        e -> fromTextError $ "Failure parsing IAMInstanceProfileAssociationState from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating"

instance ToText IAMInstanceProfileAssociationState where
    toText = \case
        Associated -> "associated"
        Associating -> "associating"
        Disassociated -> "disassociated"
        Disassociating -> "disassociating"

instance NFData       IAMInstanceProfileAssociationState
instance ToByteString IAMInstanceProfileAssociationState
instance ToQuery      IAMInstanceProfileAssociationState
instance ToHeader     IAMInstanceProfileAssociationState

instance FromXML IAMInstanceProfileAssociationState where
    parseXML = parseXMLText "IAMInstanceProfileAssociationState"

data ImageAttributeName
  = BlockDeviceMapping
  | Description
  | Kernel
  | LaunchPermission
  | ProductCodes
  | RAMDisk
  | SRIOVNetSupport
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageAttributeName where
    parser = takeLowerText >>= \case
        "blockdevicemapping" -> pure BlockDeviceMapping
        "description" -> pure Description
        "kernel" -> pure Kernel
        "launchpermission" -> pure LaunchPermission
        "productcodes" -> pure ProductCodes
        "ramdisk" -> pure RAMDisk
        "sriovnetsupport" -> pure SRIOVNetSupport
        e -> fromTextError $ "Failure parsing ImageAttributeName from value: '" <> e
           <> "'. Accepted values: blockdevicemapping, description, kernel, launchpermission, productcodes, ramdisk, sriovnetsupport"

instance ToText ImageAttributeName where
    toText = \case
        BlockDeviceMapping -> "blockDeviceMapping"
        Description -> "description"
        Kernel -> "kernel"
        LaunchPermission -> "launchPermission"
        ProductCodes -> "productCodes"
        RAMDisk -> "ramdisk"
        SRIOVNetSupport -> "sriovNetSupport"

instance NFData       ImageAttributeName
instance ToByteString ImageAttributeName
instance ToQuery      ImageAttributeName
instance ToHeader     ImageAttributeName

data ImageState
  = ISAvailable
  | ISDeregistered
  | ISError'
  | ISFailed
  | ISInvalid
  | ISPending
  | ISTransient
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageState where
    parser = takeLowerText >>= \case
        "available" -> pure ISAvailable
        "deregistered" -> pure ISDeregistered
        "error" -> pure ISError'
        "failed" -> pure ISFailed
        "invalid" -> pure ISInvalid
        "pending" -> pure ISPending
        "transient" -> pure ISTransient
        e -> fromTextError $ "Failure parsing ImageState from value: '" <> e
           <> "'. Accepted values: available, deregistered, error, failed, invalid, pending, transient"

instance ToText ImageState where
    toText = \case
        ISAvailable -> "available"
        ISDeregistered -> "deregistered"
        ISError' -> "error"
        ISFailed -> "failed"
        ISInvalid -> "invalid"
        ISPending -> "pending"
        ISTransient -> "transient"

instance NFData       ImageState
instance ToByteString ImageState
instance ToQuery      ImageState
instance ToHeader     ImageState

instance FromXML ImageState where
    parseXML = parseXMLText "ImageState"

data ImageTypeValues
  = ITVKernel
  | ITVMachine
  | ITVRAMDisk
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageTypeValues where
    parser = takeLowerText >>= \case
        "kernel" -> pure ITVKernel
        "machine" -> pure ITVMachine
        "ramdisk" -> pure ITVRAMDisk
        e -> fromTextError $ "Failure parsing ImageTypeValues from value: '" <> e
           <> "'. Accepted values: kernel, machine, ramdisk"

instance ToText ImageTypeValues where
    toText = \case
        ITVKernel -> "kernel"
        ITVMachine -> "machine"
        ITVRAMDisk -> "ramdisk"

instance NFData       ImageTypeValues
instance ToByteString ImageTypeValues
instance ToQuery      ImageTypeValues
instance ToHeader     ImageTypeValues

instance FromXML ImageTypeValues where
    parseXML = parseXMLText "ImageTypeValues"

data InstanceAttributeName
  = IANBlockDeviceMapping
  | IANDisableAPITermination
  | IANEBSOptimized
  | IANEnaSupport
  | IANGroupSet
  | IANInstanceInitiatedShutdownBehavior
  | IANInstanceType
  | IANKernel
  | IANProductCodes
  | IANRAMDisk
  | IANRootDeviceName
  | IANSRIOVNetSupport
  | IANSourceDestCheck
  | IANUserData
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceAttributeName where
    parser = takeLowerText >>= \case
        "blockdevicemapping" -> pure IANBlockDeviceMapping
        "disableapitermination" -> pure IANDisableAPITermination
        "ebsoptimized" -> pure IANEBSOptimized
        "enasupport" -> pure IANEnaSupport
        "groupset" -> pure IANGroupSet
        "instanceinitiatedshutdownbehavior" -> pure IANInstanceInitiatedShutdownBehavior
        "instancetype" -> pure IANInstanceType
        "kernel" -> pure IANKernel
        "productcodes" -> pure IANProductCodes
        "ramdisk" -> pure IANRAMDisk
        "rootdevicename" -> pure IANRootDeviceName
        "sriovnetsupport" -> pure IANSRIOVNetSupport
        "sourcedestcheck" -> pure IANSourceDestCheck
        "userdata" -> pure IANUserData
        e -> fromTextError $ "Failure parsing InstanceAttributeName from value: '" <> e
           <> "'. Accepted values: blockdevicemapping, disableapitermination, ebsoptimized, enasupport, groupset, instanceinitiatedshutdownbehavior, instancetype, kernel, productcodes, ramdisk, rootdevicename, sriovnetsupport, sourcedestcheck, userdata"

instance ToText InstanceAttributeName where
    toText = \case
        IANBlockDeviceMapping -> "blockDeviceMapping"
        IANDisableAPITermination -> "disableApiTermination"
        IANEBSOptimized -> "ebsOptimized"
        IANEnaSupport -> "enaSupport"
        IANGroupSet -> "groupSet"
        IANInstanceInitiatedShutdownBehavior -> "instanceInitiatedShutdownBehavior"
        IANInstanceType -> "instanceType"
        IANKernel -> "kernel"
        IANProductCodes -> "productCodes"
        IANRAMDisk -> "ramdisk"
        IANRootDeviceName -> "rootDeviceName"
        IANSRIOVNetSupport -> "sriovNetSupport"
        IANSourceDestCheck -> "sourceDestCheck"
        IANUserData -> "userData"

instance NFData       InstanceAttributeName
instance ToByteString InstanceAttributeName
instance ToQuery      InstanceAttributeName
instance ToHeader     InstanceAttributeName

data InstanceHealthStatus
  = Healthy
  | Unhealthy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceHealthStatus where
    parser = takeLowerText >>= \case
        "healthy" -> pure Healthy
        "unhealthy" -> pure Unhealthy
        e -> fromTextError $ "Failure parsing InstanceHealthStatus from value: '" <> e
           <> "'. Accepted values: healthy, unhealthy"

instance ToText InstanceHealthStatus where
    toText = \case
        Healthy -> "healthy"
        Unhealthy -> "unhealthy"

instance NFData       InstanceHealthStatus
instance ToByteString InstanceHealthStatus
instance ToQuery      InstanceHealthStatus
instance ToHeader     InstanceHealthStatus

instance FromXML InstanceHealthStatus where
    parseXML = parseXMLText "InstanceHealthStatus"

data InstanceInterruptionBehavior
  = Hibernate
  | Stop
  | Terminate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceInterruptionBehavior where
    parser = takeLowerText >>= \case
        "hibernate" -> pure Hibernate
        "stop" -> pure Stop
        "terminate" -> pure Terminate
        e -> fromTextError $ "Failure parsing InstanceInterruptionBehavior from value: '" <> e
           <> "'. Accepted values: hibernate, stop, terminate"

instance ToText InstanceInterruptionBehavior where
    toText = \case
        Hibernate -> "hibernate"
        Stop -> "stop"
        Terminate -> "terminate"

instance NFData       InstanceInterruptionBehavior
instance ToByteString InstanceInterruptionBehavior
instance ToQuery      InstanceInterruptionBehavior
instance ToHeader     InstanceInterruptionBehavior

instance FromXML InstanceInterruptionBehavior where
    parseXML = parseXMLText "InstanceInterruptionBehavior"

data InstanceLifecycleType
  = ILTScheduled
  | ILTSpot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceLifecycleType where
    parser = takeLowerText >>= \case
        "scheduled" -> pure ILTScheduled
        "spot" -> pure ILTSpot
        e -> fromTextError $ "Failure parsing InstanceLifecycleType from value: '" <> e
           <> "'. Accepted values: scheduled, spot"

instance ToText InstanceLifecycleType where
    toText = \case
        ILTScheduled -> "scheduled"
        ILTSpot -> "spot"

instance NFData       InstanceLifecycleType
instance ToByteString InstanceLifecycleType
instance ToQuery      InstanceLifecycleType
instance ToHeader     InstanceLifecycleType

instance FromXML InstanceLifecycleType where
    parseXML = parseXMLText "InstanceLifecycleType"

data InstanceStateName
  = ISNPending
  | ISNRunning
  | ISNShuttingDown
  | ISNStopped
  | ISNStopping
  | ISNTerminated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceStateName where
    parser = takeLowerText >>= \case
        "pending" -> pure ISNPending
        "running" -> pure ISNRunning
        "shutting-down" -> pure ISNShuttingDown
        "stopped" -> pure ISNStopped
        "stopping" -> pure ISNStopping
        "terminated" -> pure ISNTerminated
        e -> fromTextError $ "Failure parsing InstanceStateName from value: '" <> e
           <> "'. Accepted values: pending, running, shutting-down, stopped, stopping, terminated"

instance ToText InstanceStateName where
    toText = \case
        ISNPending -> "pending"
        ISNRunning -> "running"
        ISNShuttingDown -> "shutting-down"
        ISNStopped -> "stopped"
        ISNStopping -> "stopping"
        ISNTerminated -> "terminated"

instance NFData       InstanceStateName
instance ToByteString InstanceStateName
instance ToQuery      InstanceStateName
instance ToHeader     InstanceStateName

instance FromXML InstanceStateName where
    parseXML = parseXMLText "InstanceStateName"

data InstanceType
  = C1_Medium
  | C1_XLarge
  | C3_2XLarge
  | C3_4XLarge
  | C3_8XLarge
  | C3_Large
  | C3_XLarge
  | C4_2XLarge
  | C4_4XLarge
  | C4_8XLarge
  | C4_Large
  | C4_XLarge
  | C5_18XLarge
  | C5_2XLarge
  | C5_4XLarge
  | C5_9XLarge
  | C5_Large
  | C5_XLarge
  | CC1_4XLarge
  | CC2_8XLarge
  | CG1_4XLarge
  | CR1_8XLarge
  | D2_2XLarge
  | D2_4XLarge
  | D2_8XLarge
  | D2_XLarge
  | F1_16XLarge
  | F1_2XLarge
  | G2_2XLarge
  | G2_8XLarge
  | G3_16XLarge
  | G3_4XLarge
  | G3_8XLarge
  | H1_16XLarge
  | H1_2XLarge
  | H1_4XLarge
  | H1_8XLarge
  | HI1_4XLarge
  | HS1_8XLarge
  | I2_2XLarge
  | I2_4XLarge
  | I2_8XLarge
  | I2_XLarge
  | I3_16XLarge
  | I3_2XLarge
  | I3_4XLarge
  | I3_8XLarge
  | I3_Large
  | I3_XLarge
  | M1_Large
  | M1_Medium
  | M1_Small
  | M1_XLarge
  | M2_2XLarge
  | M2_4XLarge
  | M2_XLarge
  | M3_2XLarge
  | M3_Large
  | M3_Medium
  | M3_XLarge
  | M4_10XLarge
  | M4_16XLarge
  | M4_2XLarge
  | M4_4XLarge
  | M4_Large
  | M4_XLarge
  | M5_12XLarge
  | M5_24XLarge
  | M5_2XLarge
  | M5_4XLarge
  | M5_Large
  | M5_XLarge
  | P2_16XLarge
  | P2_8XLarge
  | P2_XLarge
  | P3_16XLarge
  | P3_2XLarge
  | P3_8XLarge
  | R3_2XLarge
  | R3_4XLarge
  | R3_8XLarge
  | R3_Large
  | R3_XLarge
  | R4_16XLarge
  | R4_2XLarge
  | R4_4XLarge
  | R4_8XLarge
  | R4_Large
  | R4_XLarge
  | T1_Micro
  | T2_2XLarge
  | T2_Large
  | T2_Medium
  | T2_Micro
  | T2_Nano
  | T2_Small
  | T2_XLarge
  | X1_16XLarge
  | X1_32XLarge
  | X1e_16XLarge
  | X1e_2XLarge
  | X1e_32XLarge
  | X1e_4XLarge
  | X1e_8XLarge
  | X1e_XLarge
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceType where
    parser = takeLowerText >>= \case
        "c1.medium" -> pure C1_Medium
        "c1.xlarge" -> pure C1_XLarge
        "c3.2xlarge" -> pure C3_2XLarge
        "c3.4xlarge" -> pure C3_4XLarge
        "c3.8xlarge" -> pure C3_8XLarge
        "c3.large" -> pure C3_Large
        "c3.xlarge" -> pure C3_XLarge
        "c4.2xlarge" -> pure C4_2XLarge
        "c4.4xlarge" -> pure C4_4XLarge
        "c4.8xlarge" -> pure C4_8XLarge
        "c4.large" -> pure C4_Large
        "c4.xlarge" -> pure C4_XLarge
        "c5.18xlarge" -> pure C5_18XLarge
        "c5.2xlarge" -> pure C5_2XLarge
        "c5.4xlarge" -> pure C5_4XLarge
        "c5.9xlarge" -> pure C5_9XLarge
        "c5.large" -> pure C5_Large
        "c5.xlarge" -> pure C5_XLarge
        "cc1.4xlarge" -> pure CC1_4XLarge
        "cc2.8xlarge" -> pure CC2_8XLarge
        "cg1.4xlarge" -> pure CG1_4XLarge
        "cr1.8xlarge" -> pure CR1_8XLarge
        "d2.2xlarge" -> pure D2_2XLarge
        "d2.4xlarge" -> pure D2_4XLarge
        "d2.8xlarge" -> pure D2_8XLarge
        "d2.xlarge" -> pure D2_XLarge
        "f1.16xlarge" -> pure F1_16XLarge
        "f1.2xlarge" -> pure F1_2XLarge
        "g2.2xlarge" -> pure G2_2XLarge
        "g2.8xlarge" -> pure G2_8XLarge
        "g3.16xlarge" -> pure G3_16XLarge
        "g3.4xlarge" -> pure G3_4XLarge
        "g3.8xlarge" -> pure G3_8XLarge
        "h1.16xlarge" -> pure H1_16XLarge
        "h1.2xlarge" -> pure H1_2XLarge
        "h1.4xlarge" -> pure H1_4XLarge
        "h1.8xlarge" -> pure H1_8XLarge
        "hi1.4xlarge" -> pure HI1_4XLarge
        "hs1.8xlarge" -> pure HS1_8XLarge
        "i2.2xlarge" -> pure I2_2XLarge
        "i2.4xlarge" -> pure I2_4XLarge
        "i2.8xlarge" -> pure I2_8XLarge
        "i2.xlarge" -> pure I2_XLarge
        "i3.16xlarge" -> pure I3_16XLarge
        "i3.2xlarge" -> pure I3_2XLarge
        "i3.4xlarge" -> pure I3_4XLarge
        "i3.8xlarge" -> pure I3_8XLarge
        "i3.large" -> pure I3_Large
        "i3.xlarge" -> pure I3_XLarge
        "m1.large" -> pure M1_Large
        "m1.medium" -> pure M1_Medium
        "m1.small" -> pure M1_Small
        "m1.xlarge" -> pure M1_XLarge
        "m2.2xlarge" -> pure M2_2XLarge
        "m2.4xlarge" -> pure M2_4XLarge
        "m2.xlarge" -> pure M2_XLarge
        "m3.2xlarge" -> pure M3_2XLarge
        "m3.large" -> pure M3_Large
        "m3.medium" -> pure M3_Medium
        "m3.xlarge" -> pure M3_XLarge
        "m4.10xlarge" -> pure M4_10XLarge
        "m4.16xlarge" -> pure M4_16XLarge
        "m4.2xlarge" -> pure M4_2XLarge
        "m4.4xlarge" -> pure M4_4XLarge
        "m4.large" -> pure M4_Large
        "m4.xlarge" -> pure M4_XLarge
        "m5.12xlarge" -> pure M5_12XLarge
        "m5.24xlarge" -> pure M5_24XLarge
        "m5.2xlarge" -> pure M5_2XLarge
        "m5.4xlarge" -> pure M5_4XLarge
        "m5.large" -> pure M5_Large
        "m5.xlarge" -> pure M5_XLarge
        "p2.16xlarge" -> pure P2_16XLarge
        "p2.8xlarge" -> pure P2_8XLarge
        "p2.xlarge" -> pure P2_XLarge
        "p3.16xlarge" -> pure P3_16XLarge
        "p3.2xlarge" -> pure P3_2XLarge
        "p3.8xlarge" -> pure P3_8XLarge
        "r3.2xlarge" -> pure R3_2XLarge
        "r3.4xlarge" -> pure R3_4XLarge
        "r3.8xlarge" -> pure R3_8XLarge
        "r3.large" -> pure R3_Large
        "r3.xlarge" -> pure R3_XLarge
        "r4.16xlarge" -> pure R4_16XLarge
        "r4.2xlarge" -> pure R4_2XLarge
        "r4.4xlarge" -> pure R4_4XLarge
        "r4.8xlarge" -> pure R4_8XLarge
        "r4.large" -> pure R4_Large
        "r4.xlarge" -> pure R4_XLarge
        "t1.micro" -> pure T1_Micro
        "t2.2xlarge" -> pure T2_2XLarge
        "t2.large" -> pure T2_Large
        "t2.medium" -> pure T2_Medium
        "t2.micro" -> pure T2_Micro
        "t2.nano" -> pure T2_Nano
        "t2.small" -> pure T2_Small
        "t2.xlarge" -> pure T2_XLarge
        "x1.16xlarge" -> pure X1_16XLarge
        "x1.32xlarge" -> pure X1_32XLarge
        "x1e.16xlarge" -> pure X1e_16XLarge
        "x1e.2xlarge" -> pure X1e_2XLarge
        "x1e.32xlarge" -> pure X1e_32XLarge
        "x1e.4xlarge" -> pure X1e_4XLarge
        "x1e.8xlarge" -> pure X1e_8XLarge
        "x1e.xlarge" -> pure X1e_XLarge
        e -> fromTextError $ "Failure parsing InstanceType from value: '" <> e
           <> "'. Accepted values: c1.medium, c1.xlarge, c3.2xlarge, c3.4xlarge, c3.8xlarge, c3.large, c3.xlarge, c4.2xlarge, c4.4xlarge, c4.8xlarge, c4.large, c4.xlarge, c5.18xlarge, c5.2xlarge, c5.4xlarge, c5.9xlarge, c5.large, c5.xlarge, cc1.4xlarge, cc2.8xlarge, cg1.4xlarge, cr1.8xlarge, d2.2xlarge, d2.4xlarge, d2.8xlarge, d2.xlarge, f1.16xlarge, f1.2xlarge, g2.2xlarge, g2.8xlarge, g3.16xlarge, g3.4xlarge, g3.8xlarge, h1.16xlarge, h1.2xlarge, h1.4xlarge, h1.8xlarge, hi1.4xlarge, hs1.8xlarge, i2.2xlarge, i2.4xlarge, i2.8xlarge, i2.xlarge, i3.16xlarge, i3.2xlarge, i3.4xlarge, i3.8xlarge, i3.large, i3.xlarge, m1.large, m1.medium, m1.small, m1.xlarge, m2.2xlarge, m2.4xlarge, m2.xlarge, m3.2xlarge, m3.large, m3.medium, m3.xlarge, m4.10xlarge, m4.16xlarge, m4.2xlarge, m4.4xlarge, m4.large, m4.xlarge, m5.12xlarge, m5.24xlarge, m5.2xlarge, m5.4xlarge, m5.large, m5.xlarge, p2.16xlarge, p2.8xlarge, p2.xlarge, p3.16xlarge, p3.2xlarge, p3.8xlarge, r3.2xlarge, r3.4xlarge, r3.8xlarge, r3.large, r3.xlarge, r4.16xlarge, r4.2xlarge, r4.4xlarge, r4.8xlarge, r4.large, r4.xlarge, t1.micro, t2.2xlarge, t2.large, t2.medium, t2.micro, t2.nano, t2.small, t2.xlarge, x1.16xlarge, x1.32xlarge, x1e.16xlarge, x1e.2xlarge, x1e.32xlarge, x1e.4xlarge, x1e.8xlarge, x1e.xlarge"

instance ToText InstanceType where
    toText = \case
        C1_Medium -> "c1.medium"
        C1_XLarge -> "c1.xlarge"
        C3_2XLarge -> "c3.2xlarge"
        C3_4XLarge -> "c3.4xlarge"
        C3_8XLarge -> "c3.8xlarge"
        C3_Large -> "c3.large"
        C3_XLarge -> "c3.xlarge"
        C4_2XLarge -> "c4.2xlarge"
        C4_4XLarge -> "c4.4xlarge"
        C4_8XLarge -> "c4.8xlarge"
        C4_Large -> "c4.large"
        C4_XLarge -> "c4.xlarge"
        C5_18XLarge -> "c5.18xlarge"
        C5_2XLarge -> "c5.2xlarge"
        C5_4XLarge -> "c5.4xlarge"
        C5_9XLarge -> "c5.9xlarge"
        C5_Large -> "c5.large"
        C5_XLarge -> "c5.xlarge"
        CC1_4XLarge -> "cc1.4xlarge"
        CC2_8XLarge -> "cc2.8xlarge"
        CG1_4XLarge -> "cg1.4xlarge"
        CR1_8XLarge -> "cr1.8xlarge"
        D2_2XLarge -> "d2.2xlarge"
        D2_4XLarge -> "d2.4xlarge"
        D2_8XLarge -> "d2.8xlarge"
        D2_XLarge -> "d2.xlarge"
        F1_16XLarge -> "f1.16xlarge"
        F1_2XLarge -> "f1.2xlarge"
        G2_2XLarge -> "g2.2xlarge"
        G2_8XLarge -> "g2.8xlarge"
        G3_16XLarge -> "g3.16xlarge"
        G3_4XLarge -> "g3.4xlarge"
        G3_8XLarge -> "g3.8xlarge"
        H1_16XLarge -> "h1.16xlarge"
        H1_2XLarge -> "h1.2xlarge"
        H1_4XLarge -> "h1.4xlarge"
        H1_8XLarge -> "h1.8xlarge"
        HI1_4XLarge -> "hi1.4xlarge"
        HS1_8XLarge -> "hs1.8xlarge"
        I2_2XLarge -> "i2.2xlarge"
        I2_4XLarge -> "i2.4xlarge"
        I2_8XLarge -> "i2.8xlarge"
        I2_XLarge -> "i2.xlarge"
        I3_16XLarge -> "i3.16xlarge"
        I3_2XLarge -> "i3.2xlarge"
        I3_4XLarge -> "i3.4xlarge"
        I3_8XLarge -> "i3.8xlarge"
        I3_Large -> "i3.large"
        I3_XLarge -> "i3.xlarge"
        M1_Large -> "m1.large"
        M1_Medium -> "m1.medium"
        M1_Small -> "m1.small"
        M1_XLarge -> "m1.xlarge"
        M2_2XLarge -> "m2.2xlarge"
        M2_4XLarge -> "m2.4xlarge"
        M2_XLarge -> "m2.xlarge"
        M3_2XLarge -> "m3.2xlarge"
        M3_Large -> "m3.large"
        M3_Medium -> "m3.medium"
        M3_XLarge -> "m3.xlarge"
        M4_10XLarge -> "m4.10xlarge"
        M4_16XLarge -> "m4.16xlarge"
        M4_2XLarge -> "m4.2xlarge"
        M4_4XLarge -> "m4.4xlarge"
        M4_Large -> "m4.large"
        M4_XLarge -> "m4.xlarge"
        M5_12XLarge -> "m5.12xlarge"
        M5_24XLarge -> "m5.24xlarge"
        M5_2XLarge -> "m5.2xlarge"
        M5_4XLarge -> "m5.4xlarge"
        M5_Large -> "m5.large"
        M5_XLarge -> "m5.xlarge"
        P2_16XLarge -> "p2.16xlarge"
        P2_8XLarge -> "p2.8xlarge"
        P2_XLarge -> "p2.xlarge"
        P3_16XLarge -> "p3.16xlarge"
        P3_2XLarge -> "p3.2xlarge"
        P3_8XLarge -> "p3.8xlarge"
        R3_2XLarge -> "r3.2xlarge"
        R3_4XLarge -> "r3.4xlarge"
        R3_8XLarge -> "r3.8xlarge"
        R3_Large -> "r3.large"
        R3_XLarge -> "r3.xlarge"
        R4_16XLarge -> "r4.16xlarge"
        R4_2XLarge -> "r4.2xlarge"
        R4_4XLarge -> "r4.4xlarge"
        R4_8XLarge -> "r4.8xlarge"
        R4_Large -> "r4.large"
        R4_XLarge -> "r4.xlarge"
        T1_Micro -> "t1.micro"
        T2_2XLarge -> "t2.2xlarge"
        T2_Large -> "t2.large"
        T2_Medium -> "t2.medium"
        T2_Micro -> "t2.micro"
        T2_Nano -> "t2.nano"
        T2_Small -> "t2.small"
        T2_XLarge -> "t2.xlarge"
        X1_16XLarge -> "x1.16xlarge"
        X1_32XLarge -> "x1.32xlarge"
        X1e_16XLarge -> "x1e.16xlarge"
        X1e_2XLarge -> "x1e.2xlarge"
        X1e_32XLarge -> "x1e.32xlarge"
        X1e_4XLarge -> "x1e.4xlarge"
        X1e_8XLarge -> "x1e.8xlarge"
        X1e_XLarge -> "x1e.xlarge"

instance NFData       InstanceType
instance ToByteString InstanceType
instance ToQuery      InstanceType
instance ToHeader     InstanceType

instance FromXML InstanceType where
    parseXML = parseXMLText "InstanceType"

data InterfacePermissionType
  = EIPAssociate
  | InstanceAttach
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InterfacePermissionType where
    parser = takeLowerText >>= \case
        "eip-associate" -> pure EIPAssociate
        "instance-attach" -> pure InstanceAttach
        e -> fromTextError $ "Failure parsing InterfacePermissionType from value: '" <> e
           <> "'. Accepted values: eip-associate, instance-attach"

instance ToText InterfacePermissionType where
    toText = \case
        EIPAssociate -> "EIP-ASSOCIATE"
        InstanceAttach -> "INSTANCE-ATTACH"

instance NFData       InterfacePermissionType
instance ToByteString InterfacePermissionType
instance ToQuery      InterfacePermissionType
instance ToHeader     InterfacePermissionType

instance FromXML InterfacePermissionType where
    parseXML = parseXMLText "InterfacePermissionType"

data LaunchTemplateErrorCode
  = LaunchTemplateIdDoesNotExist
  | LaunchTemplateIdMalformed
  | LaunchTemplateNameDoesNotExist
  | LaunchTemplateNameMalformed
  | LaunchTemplateVersionDoesNotExist
  | UnexpectedError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LaunchTemplateErrorCode where
    parser = takeLowerText >>= \case
        "launchtemplateiddoesnotexist" -> pure LaunchTemplateIdDoesNotExist
        "launchtemplateidmalformed" -> pure LaunchTemplateIdMalformed
        "launchtemplatenamedoesnotexist" -> pure LaunchTemplateNameDoesNotExist
        "launchtemplatenamemalformed" -> pure LaunchTemplateNameMalformed
        "launchtemplateversiondoesnotexist" -> pure LaunchTemplateVersionDoesNotExist
        "unexpectederror" -> pure UnexpectedError
        e -> fromTextError $ "Failure parsing LaunchTemplateErrorCode from value: '" <> e
           <> "'. Accepted values: launchtemplateiddoesnotexist, launchtemplateidmalformed, launchtemplatenamedoesnotexist, launchtemplatenamemalformed, launchtemplateversiondoesnotexist, unexpectederror"

instance ToText LaunchTemplateErrorCode where
    toText = \case
        LaunchTemplateIdDoesNotExist -> "launchTemplateIdDoesNotExist"
        LaunchTemplateIdMalformed -> "launchTemplateIdMalformed"
        LaunchTemplateNameDoesNotExist -> "launchTemplateNameDoesNotExist"
        LaunchTemplateNameMalformed -> "launchTemplateNameMalformed"
        LaunchTemplateVersionDoesNotExist -> "launchTemplateVersionDoesNotExist"
        UnexpectedError -> "unexpectedError"

instance NFData       LaunchTemplateErrorCode
instance ToByteString LaunchTemplateErrorCode
instance ToQuery      LaunchTemplateErrorCode
instance ToHeader     LaunchTemplateErrorCode

instance FromXML LaunchTemplateErrorCode where
    parseXML = parseXMLText "LaunchTemplateErrorCode"

data ListingState
  = LAvailable
  | LCancelled
  | LPending
  | LSold
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ListingState where
    parser = takeLowerText >>= \case
        "available" -> pure LAvailable
        "cancelled" -> pure LCancelled
        "pending" -> pure LPending
        "sold" -> pure LSold
        e -> fromTextError $ "Failure parsing ListingState from value: '" <> e
           <> "'. Accepted values: available, cancelled, pending, sold"

instance ToText ListingState where
    toText = \case
        LAvailable -> "available"
        LCancelled -> "cancelled"
        LPending -> "pending"
        LSold -> "sold"

instance NFData       ListingState
instance ToByteString ListingState
instance ToQuery      ListingState
instance ToHeader     ListingState

instance FromXML ListingState where
    parseXML = parseXMLText "ListingState"

data ListingStatus
  = LSActive
  | LSCancelled
  | LSClosed
  | LSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ListingStatus where
    parser = takeLowerText >>= \case
        "active" -> pure LSActive
        "cancelled" -> pure LSCancelled
        "closed" -> pure LSClosed
        "pending" -> pure LSPending
        e -> fromTextError $ "Failure parsing ListingStatus from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, pending"

instance ToText ListingStatus where
    toText = \case
        LSActive -> "active"
        LSCancelled -> "cancelled"
        LSClosed -> "closed"
        LSPending -> "pending"

instance NFData       ListingStatus
instance ToByteString ListingStatus
instance ToQuery      ListingStatus
instance ToHeader     ListingStatus

instance FromXML ListingStatus where
    parseXML = parseXMLText "ListingStatus"

data MarketType =
  Spot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MarketType where
    parser = takeLowerText >>= \case
        "spot" -> pure Spot
        e -> fromTextError $ "Failure parsing MarketType from value: '" <> e
           <> "'. Accepted values: spot"

instance ToText MarketType where
    toText = \case
        Spot -> "spot"

instance NFData       MarketType
instance ToByteString MarketType
instance ToQuery      MarketType
instance ToHeader     MarketType

instance FromXML MarketType where
    parseXML = parseXMLText "MarketType"

data MonitoringState
  = MSDisabled
  | MSDisabling
  | MSEnabled
  | MSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MonitoringState where
    parser = takeLowerText >>= \case
        "disabled" -> pure MSDisabled
        "disabling" -> pure MSDisabling
        "enabled" -> pure MSEnabled
        "pending" -> pure MSPending
        e -> fromTextError $ "Failure parsing MonitoringState from value: '" <> e
           <> "'. Accepted values: disabled, disabling, enabled, pending"

instance ToText MonitoringState where
    toText = \case
        MSDisabled -> "disabled"
        MSDisabling -> "disabling"
        MSEnabled -> "enabled"
        MSPending -> "pending"

instance NFData       MonitoringState
instance ToByteString MonitoringState
instance ToQuery      MonitoringState
instance ToHeader     MonitoringState

instance FromXML MonitoringState where
    parseXML = parseXMLText "MonitoringState"

data MoveStatus
  = MovingToVPC
  | RestoringToClassic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MoveStatus where
    parser = takeLowerText >>= \case
        "movingtovpc" -> pure MovingToVPC
        "restoringtoclassic" -> pure RestoringToClassic
        e -> fromTextError $ "Failure parsing MoveStatus from value: '" <> e
           <> "'. Accepted values: movingtovpc, restoringtoclassic"

instance ToText MoveStatus where
    toText = \case
        MovingToVPC -> "movingToVpc"
        RestoringToClassic -> "restoringToClassic"

instance NFData       MoveStatus
instance ToByteString MoveStatus
instance ToQuery      MoveStatus
instance ToHeader     MoveStatus

instance FromXML MoveStatus where
    parseXML = parseXMLText "MoveStatus"

data NatGatewayState
  = NGSAvailable
  | NGSDeleted
  | NGSDeleting
  | NGSFailed
  | NGSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NatGatewayState where
    parser = takeLowerText >>= \case
        "available" -> pure NGSAvailable
        "deleted" -> pure NGSDeleted
        "deleting" -> pure NGSDeleting
        "failed" -> pure NGSFailed
        "pending" -> pure NGSPending
        e -> fromTextError $ "Failure parsing NatGatewayState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, failed, pending"

instance ToText NatGatewayState where
    toText = \case
        NGSAvailable -> "available"
        NGSDeleted -> "deleted"
        NGSDeleting -> "deleting"
        NGSFailed -> "failed"
        NGSPending -> "pending"

instance NFData       NatGatewayState
instance ToByteString NatGatewayState
instance ToQuery      NatGatewayState
instance ToHeader     NatGatewayState

instance FromXML NatGatewayState where
    parseXML = parseXMLText "NatGatewayState"

data NetworkInterfaceAttribute
  = NIAAttachment
  | NIADescription
  | NIAGroupSet
  | NIASourceDestCheck
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfaceAttribute where
    parser = takeLowerText >>= \case
        "attachment" -> pure NIAAttachment
        "description" -> pure NIADescription
        "groupset" -> pure NIAGroupSet
        "sourcedestcheck" -> pure NIASourceDestCheck
        e -> fromTextError $ "Failure parsing NetworkInterfaceAttribute from value: '" <> e
           <> "'. Accepted values: attachment, description, groupset, sourcedestcheck"

instance ToText NetworkInterfaceAttribute where
    toText = \case
        NIAAttachment -> "attachment"
        NIADescription -> "description"
        NIAGroupSet -> "groupSet"
        NIASourceDestCheck -> "sourceDestCheck"

instance NFData       NetworkInterfaceAttribute
instance ToByteString NetworkInterfaceAttribute
instance ToQuery      NetworkInterfaceAttribute
instance ToHeader     NetworkInterfaceAttribute

data NetworkInterfacePermissionStateCode
  = NIPSCGranted
  | NIPSCPending
  | NIPSCRevoked
  | NIPSCRevoking
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfacePermissionStateCode where
    parser = takeLowerText >>= \case
        "granted" -> pure NIPSCGranted
        "pending" -> pure NIPSCPending
        "revoked" -> pure NIPSCRevoked
        "revoking" -> pure NIPSCRevoking
        e -> fromTextError $ "Failure parsing NetworkInterfacePermissionStateCode from value: '" <> e
           <> "'. Accepted values: granted, pending, revoked, revoking"

instance ToText NetworkInterfacePermissionStateCode where
    toText = \case
        NIPSCGranted -> "granted"
        NIPSCPending -> "pending"
        NIPSCRevoked -> "revoked"
        NIPSCRevoking -> "revoking"

instance NFData       NetworkInterfacePermissionStateCode
instance ToByteString NetworkInterfacePermissionStateCode
instance ToQuery      NetworkInterfacePermissionStateCode
instance ToHeader     NetworkInterfacePermissionStateCode

instance FromXML NetworkInterfacePermissionStateCode where
    parseXML = parseXMLText "NetworkInterfacePermissionStateCode"

data NetworkInterfaceStatus
  = NISAssociated
  | NISAttaching
  | NISAvailable
  | NISDetaching
  | NISInUse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfaceStatus where
    parser = takeLowerText >>= \case
        "associated" -> pure NISAssociated
        "attaching" -> pure NISAttaching
        "available" -> pure NISAvailable
        "detaching" -> pure NISDetaching
        "in-use" -> pure NISInUse
        e -> fromTextError $ "Failure parsing NetworkInterfaceStatus from value: '" <> e
           <> "'. Accepted values: associated, attaching, available, detaching, in-use"

instance ToText NetworkInterfaceStatus where
    toText = \case
        NISAssociated -> "associated"
        NISAttaching -> "attaching"
        NISAvailable -> "available"
        NISDetaching -> "detaching"
        NISInUse -> "in-use"

instance NFData       NetworkInterfaceStatus
instance ToByteString NetworkInterfaceStatus
instance ToQuery      NetworkInterfaceStatus
instance ToHeader     NetworkInterfaceStatus

instance FromXML NetworkInterfaceStatus where
    parseXML = parseXMLText "NetworkInterfaceStatus"

data NetworkInterfaceType
  = NITInterface
  | NITNatGateway
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInterfaceType where
    parser = takeLowerText >>= \case
        "interface" -> pure NITInterface
        "natgateway" -> pure NITNatGateway
        e -> fromTextError $ "Failure parsing NetworkInterfaceType from value: '" <> e
           <> "'. Accepted values: interface, natgateway"

instance ToText NetworkInterfaceType where
    toText = \case
        NITInterface -> "interface"
        NITNatGateway -> "natGateway"

instance NFData       NetworkInterfaceType
instance ToByteString NetworkInterfaceType
instance ToQuery      NetworkInterfaceType
instance ToHeader     NetworkInterfaceType

instance FromXML NetworkInterfaceType where
    parseXML = parseXMLText "NetworkInterfaceType"

data OfferingClassType
  = OCTConvertible
  | OCTStandard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OfferingClassType where
    parser = takeLowerText >>= \case
        "convertible" -> pure OCTConvertible
        "standard" -> pure OCTStandard
        e -> fromTextError $ "Failure parsing OfferingClassType from value: '" <> e
           <> "'. Accepted values: convertible, standard"

instance ToText OfferingClassType where
    toText = \case
        OCTConvertible -> "convertible"
        OCTStandard -> "standard"

instance NFData       OfferingClassType
instance ToByteString OfferingClassType
instance ToQuery      OfferingClassType
instance ToHeader     OfferingClassType

instance FromXML OfferingClassType where
    parseXML = parseXMLText "OfferingClassType"

data OfferingTypeValues
  = AllUpfront
  | HeavyUtilization
  | LightUtilization
  | MediumUtilization
  | NoUpfront
  | PartialUpfront
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OfferingTypeValues where
    parser = takeLowerText >>= \case
        "all upfront" -> pure AllUpfront
        "heavy utilization" -> pure HeavyUtilization
        "light utilization" -> pure LightUtilization
        "medium utilization" -> pure MediumUtilization
        "no upfront" -> pure NoUpfront
        "partial upfront" -> pure PartialUpfront
        e -> fromTextError $ "Failure parsing OfferingTypeValues from value: '" <> e
           <> "'. Accepted values: all upfront, heavy utilization, light utilization, medium utilization, no upfront, partial upfront"

instance ToText OfferingTypeValues where
    toText = \case
        AllUpfront -> "All Upfront"
        HeavyUtilization -> "Heavy Utilization"
        LightUtilization -> "Light Utilization"
        MediumUtilization -> "Medium Utilization"
        NoUpfront -> "No Upfront"
        PartialUpfront -> "Partial Upfront"

instance NFData       OfferingTypeValues
instance ToByteString OfferingTypeValues
instance ToQuery      OfferingTypeValues
instance ToHeader     OfferingTypeValues

instance FromXML OfferingTypeValues where
    parseXML = parseXMLText "OfferingTypeValues"

data OperationType
  = Add
  | Remove
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationType where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: add, remove"

instance ToText OperationType where
    toText = \case
        Add -> "add"
        Remove -> "remove"

instance NFData       OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

data PaymentOption
  = POAllUpfront
  | PONoUpfront
  | POPartialUpfront
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PaymentOption where
    parser = takeLowerText >>= \case
        "allupfront" -> pure POAllUpfront
        "noupfront" -> pure PONoUpfront
        "partialupfront" -> pure POPartialUpfront
        e -> fromTextError $ "Failure parsing PaymentOption from value: '" <> e
           <> "'. Accepted values: allupfront, noupfront, partialupfront"

instance ToText PaymentOption where
    toText = \case
        POAllUpfront -> "AllUpfront"
        PONoUpfront -> "NoUpfront"
        POPartialUpfront -> "PartialUpfront"

instance NFData       PaymentOption
instance ToByteString PaymentOption
instance ToQuery      PaymentOption
instance ToHeader     PaymentOption

instance FromXML PaymentOption where
    parseXML = parseXMLText "PaymentOption"

data PermissionGroup =
  All
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PermissionGroup where
    parser = takeLowerText >>= \case
        "all" -> pure All
        e -> fromTextError $ "Failure parsing PermissionGroup from value: '" <> e
           <> "'. Accepted values: all"

instance ToText PermissionGroup where
    toText = \case
        All -> "all"

instance NFData       PermissionGroup
instance ToByteString PermissionGroup
instance ToQuery      PermissionGroup
instance ToHeader     PermissionGroup

instance FromXML PermissionGroup where
    parseXML = parseXMLText "PermissionGroup"

data PlacementGroupState
  = Available
  | Deleted
  | Deleting
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlacementGroupState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing PlacementGroupState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText PlacementGroupState where
    toText = \case
        Available -> "available"
        Deleted -> "deleted"
        Deleting -> "deleting"
        Pending -> "pending"

instance NFData       PlacementGroupState
instance ToByteString PlacementGroupState
instance ToQuery      PlacementGroupState
instance ToHeader     PlacementGroupState

instance FromXML PlacementGroupState where
    parseXML = parseXMLText "PlacementGroupState"

data PlacementStrategy
  = Cluster
  | Spread
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlacementStrategy where
    parser = takeLowerText >>= \case
        "cluster" -> pure Cluster
        "spread" -> pure Spread
        e -> fromTextError $ "Failure parsing PlacementStrategy from value: '" <> e
           <> "'. Accepted values: cluster, spread"

instance ToText PlacementStrategy where
    toText = \case
        Cluster -> "cluster"
        Spread -> "spread"

instance NFData       PlacementStrategy
instance ToByteString PlacementStrategy
instance ToQuery      PlacementStrategy
instance ToHeader     PlacementStrategy

instance FromXML PlacementStrategy where
    parseXML = parseXMLText "PlacementStrategy"

data PlatformValues =
  Windows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlatformValues where
    parser = takeLowerText >>= \case
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing PlatformValues from value: '" <> e
           <> "'. Accepted values: windows"

instance ToText PlatformValues where
    toText = \case
        Windows -> "Windows"

instance NFData       PlatformValues
instance ToByteString PlatformValues
instance ToQuery      PlatformValues
instance ToHeader     PlatformValues

instance FromXML PlatformValues where
    parseXML = parseXMLText "PlatformValues"

data PrincipalType
  = PTAccount
  | PTAll
  | PTOrganizationUnit
  | PTRole
  | PTService
  | PTUser
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PrincipalType where
    parser = takeLowerText >>= \case
        "account" -> pure PTAccount
        "all" -> pure PTAll
        "organizationunit" -> pure PTOrganizationUnit
        "role" -> pure PTRole
        "service" -> pure PTService
        "user" -> pure PTUser
        e -> fromTextError $ "Failure parsing PrincipalType from value: '" <> e
           <> "'. Accepted values: account, all, organizationunit, role, service, user"

instance ToText PrincipalType where
    toText = \case
        PTAccount -> "Account"
        PTAll -> "All"
        PTOrganizationUnit -> "OrganizationUnit"
        PTRole -> "Role"
        PTService -> "Service"
        PTUser -> "User"

instance NFData       PrincipalType
instance ToByteString PrincipalType
instance ToQuery      PrincipalType
instance ToHeader     PrincipalType

instance FromXML PrincipalType where
    parseXML = parseXMLText "PrincipalType"

data ProductCodeValues
  = Devpay
  | Marketplace
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductCodeValues where
    parser = takeLowerText >>= \case
        "devpay" -> pure Devpay
        "marketplace" -> pure Marketplace
        e -> fromTextError $ "Failure parsing ProductCodeValues from value: '" <> e
           <> "'. Accepted values: devpay, marketplace"

instance ToText ProductCodeValues where
    toText = \case
        Devpay -> "devpay"
        Marketplace -> "marketplace"

instance NFData       ProductCodeValues
instance ToByteString ProductCodeValues
instance ToQuery      ProductCodeValues
instance ToHeader     ProductCodeValues

instance FromXML ProductCodeValues where
    parseXML = parseXMLText "ProductCodeValues"

data RIProductDescription
  = RIDLinuxUnix
  | RIDLinuxUnixAmazonVPC
  | RIDWindows
  | RIDWindowsAmazonVPC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RIProductDescription where
    parser = takeLowerText >>= \case
        "linux/unix" -> pure RIDLinuxUnix
        "linux/unix (amazon vpc)" -> pure RIDLinuxUnixAmazonVPC
        "windows" -> pure RIDWindows
        "windows (amazon vpc)" -> pure RIDWindowsAmazonVPC
        e -> fromTextError $ "Failure parsing RIProductDescription from value: '" <> e
           <> "'. Accepted values: linux/unix, linux/unix (amazon vpc), windows, windows (amazon vpc)"

instance ToText RIProductDescription where
    toText = \case
        RIDLinuxUnix -> "Linux/UNIX"
        RIDLinuxUnixAmazonVPC -> "Linux/UNIX (Amazon VPC)"
        RIDWindows -> "Windows"
        RIDWindowsAmazonVPC -> "Windows (Amazon VPC)"

instance NFData       RIProductDescription
instance ToByteString RIProductDescription
instance ToQuery      RIProductDescription
instance ToHeader     RIProductDescription

instance FromXML RIProductDescription where
    parseXML = parseXMLText "RIProductDescription"

data RecurringChargeFrequency =
  Hourly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecurringChargeFrequency where
    parser = takeLowerText >>= \case
        "hourly" -> pure Hourly
        e -> fromTextError $ "Failure parsing RecurringChargeFrequency from value: '" <> e
           <> "'. Accepted values: hourly"

instance ToText RecurringChargeFrequency where
    toText = \case
        Hourly -> "Hourly"

instance NFData       RecurringChargeFrequency
instance ToByteString RecurringChargeFrequency
instance ToQuery      RecurringChargeFrequency
instance ToHeader     RecurringChargeFrequency

instance FromXML RecurringChargeFrequency where
    parseXML = parseXMLText "RecurringChargeFrequency"

data ReportInstanceReasonCodes
  = InstanceStuckInState
  | NotAcceptingCredentials
  | Other
  | PasswordNotAvailable
  | PerformanceEBSVolume
  | PerformanceInstanceStore
  | PerformanceNetwork
  | PerformanceOther
  | Unresponsive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportInstanceReasonCodes where
    parser = takeLowerText >>= \case
        "instance-stuck-in-state" -> pure InstanceStuckInState
        "not-accepting-credentials" -> pure NotAcceptingCredentials
        "other" -> pure Other
        "password-not-available" -> pure PasswordNotAvailable
        "performance-ebs-volume" -> pure PerformanceEBSVolume
        "performance-instance-store" -> pure PerformanceInstanceStore
        "performance-network" -> pure PerformanceNetwork
        "performance-other" -> pure PerformanceOther
        "unresponsive" -> pure Unresponsive
        e -> fromTextError $ "Failure parsing ReportInstanceReasonCodes from value: '" <> e
           <> "'. Accepted values: instance-stuck-in-state, not-accepting-credentials, other, password-not-available, performance-ebs-volume, performance-instance-store, performance-network, performance-other, unresponsive"

instance ToText ReportInstanceReasonCodes where
    toText = \case
        InstanceStuckInState -> "instance-stuck-in-state"
        NotAcceptingCredentials -> "not-accepting-credentials"
        Other -> "other"
        PasswordNotAvailable -> "password-not-available"
        PerformanceEBSVolume -> "performance-ebs-volume"
        PerformanceInstanceStore -> "performance-instance-store"
        PerformanceNetwork -> "performance-network"
        PerformanceOther -> "performance-other"
        Unresponsive -> "unresponsive"

instance NFData       ReportInstanceReasonCodes
instance ToByteString ReportInstanceReasonCodes
instance ToQuery      ReportInstanceReasonCodes
instance ToHeader     ReportInstanceReasonCodes

data ReportStatusType
  = RSTImpaired
  | RSTOK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportStatusType where
    parser = takeLowerText >>= \case
        "impaired" -> pure RSTImpaired
        "ok" -> pure RSTOK
        e -> fromTextError $ "Failure parsing ReportStatusType from value: '" <> e
           <> "'. Accepted values: impaired, ok"

instance ToText ReportStatusType where
    toText = \case
        RSTImpaired -> "impaired"
        RSTOK -> "ok"

instance NFData       ReportStatusType
instance ToByteString ReportStatusType
instance ToQuery      ReportStatusType
instance ToHeader     ReportStatusType

data ReservationState
  = RSActive
  | RSPaymentFailed
  | RSPaymentPending
  | RSRetired
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReservationState where
    parser = takeLowerText >>= \case
        "active" -> pure RSActive
        "payment-failed" -> pure RSPaymentFailed
        "payment-pending" -> pure RSPaymentPending
        "retired" -> pure RSRetired
        e -> fromTextError $ "Failure parsing ReservationState from value: '" <> e
           <> "'. Accepted values: active, payment-failed, payment-pending, retired"

instance ToText ReservationState where
    toText = \case
        RSActive -> "active"
        RSPaymentFailed -> "payment-failed"
        RSPaymentPending -> "payment-pending"
        RSRetired -> "retired"

instance NFData       ReservationState
instance ToByteString ReservationState
instance ToQuery      ReservationState
instance ToHeader     ReservationState

instance FromXML ReservationState where
    parseXML = parseXMLText "ReservationState"

data ReservedInstanceState
  = Active
  | PaymentFailed
  | PaymentPending
  | Retired
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReservedInstanceState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "payment-failed" -> pure PaymentFailed
        "payment-pending" -> pure PaymentPending
        "retired" -> pure Retired
        e -> fromTextError $ "Failure parsing ReservedInstanceState from value: '" <> e
           <> "'. Accepted values: active, payment-failed, payment-pending, retired"

instance ToText ReservedInstanceState where
    toText = \case
        Active -> "active"
        PaymentFailed -> "payment-failed"
        PaymentPending -> "payment-pending"
        Retired -> "retired"

instance NFData       ReservedInstanceState
instance ToByteString ReservedInstanceState
instance ToQuery      ReservedInstanceState
instance ToHeader     ReservedInstanceState

instance FromXML ReservedInstanceState where
    parseXML = parseXMLText "ReservedInstanceState"

data ResetFpgaImageAttributeName =
  LoadPermission
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResetFpgaImageAttributeName where
    parser = takeLowerText >>= \case
        "loadpermission" -> pure LoadPermission
        e -> fromTextError $ "Failure parsing ResetFpgaImageAttributeName from value: '" <> e
           <> "'. Accepted values: loadpermission"

instance ToText ResetFpgaImageAttributeName where
    toText = \case
        LoadPermission -> "loadPermission"

instance NFData       ResetFpgaImageAttributeName
instance ToByteString ResetFpgaImageAttributeName
instance ToQuery      ResetFpgaImageAttributeName
instance ToHeader     ResetFpgaImageAttributeName

data ResetImageAttributeName =
  RIANLaunchPermission
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResetImageAttributeName where
    parser = takeLowerText >>= \case
        "launchpermission" -> pure RIANLaunchPermission
        e -> fromTextError $ "Failure parsing ResetImageAttributeName from value: '" <> e
           <> "'. Accepted values: launchpermission"

instance ToText ResetImageAttributeName where
    toText = \case
        RIANLaunchPermission -> "launchPermission"

instance NFData       ResetImageAttributeName
instance ToByteString ResetImageAttributeName
instance ToQuery      ResetImageAttributeName
instance ToHeader     ResetImageAttributeName

data ResourceType
  = CustomerGateway
  | DHCPOptions
  | Image
  | Instance
  | InternetGateway
  | NetworkACL
  | NetworkInterface
  | ReservedInstances
  | RouteTable
  | SecurityGroup
  | Snapshot
  | SpotInstancesRequest
  | Subnet
  | VPC
  | VPNConnection
  | VPNGateway
  | Volume
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "customer-gateway" -> pure CustomerGateway
        "dhcp-options" -> pure DHCPOptions
        "image" -> pure Image
        "instance" -> pure Instance
        "internet-gateway" -> pure InternetGateway
        "network-acl" -> pure NetworkACL
        "network-interface" -> pure NetworkInterface
        "reserved-instances" -> pure ReservedInstances
        "route-table" -> pure RouteTable
        "security-group" -> pure SecurityGroup
        "snapshot" -> pure Snapshot
        "spot-instances-request" -> pure SpotInstancesRequest
        "subnet" -> pure Subnet
        "vpc" -> pure VPC
        "vpn-connection" -> pure VPNConnection
        "vpn-gateway" -> pure VPNGateway
        "volume" -> pure Volume
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: customer-gateway, dhcp-options, image, instance, internet-gateway, network-acl, network-interface, reserved-instances, route-table, security-group, snapshot, spot-instances-request, subnet, vpc, vpn-connection, vpn-gateway, volume"

instance ToText ResourceType where
    toText = \case
        CustomerGateway -> "customer-gateway"
        DHCPOptions -> "dhcp-options"
        Image -> "image"
        Instance -> "instance"
        InternetGateway -> "internet-gateway"
        NetworkACL -> "network-acl"
        NetworkInterface -> "network-interface"
        ReservedInstances -> "reserved-instances"
        RouteTable -> "route-table"
        SecurityGroup -> "security-group"
        Snapshot -> "snapshot"
        SpotInstancesRequest -> "spot-instances-request"
        Subnet -> "subnet"
        VPC -> "vpc"
        VPNConnection -> "vpn-connection"
        VPNGateway -> "vpn-gateway"
        Volume -> "volume"

instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromXML ResourceType where
    parseXML = parseXMLText "ResourceType"

data RouteOrigin
  = CreateRoute
  | CreateRouteTable
  | EnableVGWRoutePropagation
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RouteOrigin where
    parser = takeLowerText >>= \case
        "createroute" -> pure CreateRoute
        "createroutetable" -> pure CreateRouteTable
        "enablevgwroutepropagation" -> pure EnableVGWRoutePropagation
        e -> fromTextError $ "Failure parsing RouteOrigin from value: '" <> e
           <> "'. Accepted values: createroute, createroutetable, enablevgwroutepropagation"

instance ToText RouteOrigin where
    toText = \case
        CreateRoute -> "CreateRoute"
        CreateRouteTable -> "CreateRouteTable"
        EnableVGWRoutePropagation -> "EnableVgwRoutePropagation"

instance NFData       RouteOrigin
instance ToByteString RouteOrigin
instance ToQuery      RouteOrigin
instance ToHeader     RouteOrigin

instance FromXML RouteOrigin where
    parseXML = parseXMLText "RouteOrigin"

data RouteState
  = RActive
  | RBlackhole
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RouteState where
    parser = takeLowerText >>= \case
        "active" -> pure RActive
        "blackhole" -> pure RBlackhole
        e -> fromTextError $ "Failure parsing RouteState from value: '" <> e
           <> "'. Accepted values: active, blackhole"

instance ToText RouteState where
    toText = \case
        RActive -> "active"
        RBlackhole -> "blackhole"

instance NFData       RouteState
instance ToByteString RouteState
instance ToQuery      RouteState
instance ToHeader     RouteState

instance FromXML RouteState where
    parseXML = parseXMLText "RouteState"

data RuleAction
  = Allow
  | Deny
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RuleAction where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "deny" -> pure Deny
        e -> fromTextError $ "Failure parsing RuleAction from value: '" <> e
           <> "'. Accepted values: allow, deny"

instance ToText RuleAction where
    toText = \case
        Allow -> "allow"
        Deny -> "deny"

instance NFData       RuleAction
instance ToByteString RuleAction
instance ToQuery      RuleAction
instance ToHeader     RuleAction

instance FromXML RuleAction where
    parseXML = parseXMLText "RuleAction"

data Scope
  = AvailabilityZone
  | Region
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Scope where
    parser = takeLowerText >>= \case
        "availability zone" -> pure AvailabilityZone
        "region" -> pure Region
        e -> fromTextError $ "Failure parsing Scope from value: '" <> e
           <> "'. Accepted values: availability zone, region"

instance ToText Scope where
    toText = \case
        AvailabilityZone -> "Availability Zone"
        Region -> "Region"

instance NFData       Scope
instance ToByteString Scope
instance ToQuery      Scope
instance ToHeader     Scope

instance FromXML Scope where
    parseXML = parseXMLText "Scope"

data ServiceState
  = SerAvailable
  | SerDeleted
  | SerDeleting
  | SerFailed
  | SerPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceState where
    parser = takeLowerText >>= \case
        "available" -> pure SerAvailable
        "deleted" -> pure SerDeleted
        "deleting" -> pure SerDeleting
        "failed" -> pure SerFailed
        "pending" -> pure SerPending
        e -> fromTextError $ "Failure parsing ServiceState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, failed, pending"

instance ToText ServiceState where
    toText = \case
        SerAvailable -> "Available"
        SerDeleted -> "Deleted"
        SerDeleting -> "Deleting"
        SerFailed -> "Failed"
        SerPending -> "Pending"

instance NFData       ServiceState
instance ToByteString ServiceState
instance ToQuery      ServiceState
instance ToHeader     ServiceState

instance FromXML ServiceState where
    parseXML = parseXMLText "ServiceState"

data ServiceType
  = Gateway
  | Interface
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceType where
    parser = takeLowerText >>= \case
        "gateway" -> pure Gateway
        "interface" -> pure Interface
        e -> fromTextError $ "Failure parsing ServiceType from value: '" <> e
           <> "'. Accepted values: gateway, interface"

instance ToText ServiceType where
    toText = \case
        Gateway -> "Gateway"
        Interface -> "Interface"

instance NFData       ServiceType
instance ToByteString ServiceType
instance ToQuery      ServiceType
instance ToHeader     ServiceType

instance FromXML ServiceType where
    parseXML = parseXMLText "ServiceType"

data ShutdownBehavior
  = SBStop
  | SBTerminate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ShutdownBehavior where
    parser = takeLowerText >>= \case
        "stop" -> pure SBStop
        "terminate" -> pure SBTerminate
        e -> fromTextError $ "Failure parsing ShutdownBehavior from value: '" <> e
           <> "'. Accepted values: stop, terminate"

instance ToText ShutdownBehavior where
    toText = \case
        SBStop -> "stop"
        SBTerminate -> "terminate"

instance NFData       ShutdownBehavior
instance ToByteString ShutdownBehavior
instance ToQuery      ShutdownBehavior
instance ToHeader     ShutdownBehavior

instance FromXML ShutdownBehavior where
    parseXML = parseXMLText "ShutdownBehavior"

data SnapshotAttributeName
  = SANCreateVolumePermission
  | SANProductCodes
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnapshotAttributeName where
    parser = takeLowerText >>= \case
        "createvolumepermission" -> pure SANCreateVolumePermission
        "productcodes" -> pure SANProductCodes
        e -> fromTextError $ "Failure parsing SnapshotAttributeName from value: '" <> e
           <> "'. Accepted values: createvolumepermission, productcodes"

instance ToText SnapshotAttributeName where
    toText = \case
        SANCreateVolumePermission -> "createVolumePermission"
        SANProductCodes -> "productCodes"

instance NFData       SnapshotAttributeName
instance ToByteString SnapshotAttributeName
instance ToQuery      SnapshotAttributeName
instance ToHeader     SnapshotAttributeName

data SnapshotState
  = SSCompleted
  | SSError'
  | SSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnapshotState where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "error" -> pure SSError'
        "pending" -> pure SSPending
        e -> fromTextError $ "Failure parsing SnapshotState from value: '" <> e
           <> "'. Accepted values: completed, error, pending"

instance ToText SnapshotState where
    toText = \case
        SSCompleted -> "completed"
        SSError' -> "error"
        SSPending -> "pending"

instance NFData       SnapshotState
instance ToByteString SnapshotState
instance ToQuery      SnapshotState
instance ToHeader     SnapshotState

instance FromXML SnapshotState where
    parseXML = parseXMLText "SnapshotState"

data SpotAllocationStrategy
  = Diversified
  | LowestPrice
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotAllocationStrategy where
    parser = takeLowerText >>= \case
        "diversified" -> pure Diversified
        "lowest-price" -> pure LowestPrice
        e -> fromTextError $ "Failure parsing SpotAllocationStrategy from value: '" <> e
           <> "'. Accepted values: diversified, lowest-price"

instance ToText SpotAllocationStrategy where
    toText = \case
        Diversified -> "diversified"
        LowestPrice -> "lowest-price"

instance NFData       SpotAllocationStrategy
instance ToByteString SpotAllocationStrategy
instance ToQuery      SpotAllocationStrategy
instance ToHeader     SpotAllocationStrategy

instance FromXML SpotAllocationStrategy where
    parseXML = parseXMLText "SpotAllocationStrategy"

data SpotInstanceInterruptionBehavior
  = SIIBHibernate
  | SIIBStop
  | SIIBTerminate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotInstanceInterruptionBehavior where
    parser = takeLowerText >>= \case
        "hibernate" -> pure SIIBHibernate
        "stop" -> pure SIIBStop
        "terminate" -> pure SIIBTerminate
        e -> fromTextError $ "Failure parsing SpotInstanceInterruptionBehavior from value: '" <> e
           <> "'. Accepted values: hibernate, stop, terminate"

instance ToText SpotInstanceInterruptionBehavior where
    toText = \case
        SIIBHibernate -> "hibernate"
        SIIBStop -> "stop"
        SIIBTerminate -> "terminate"

instance NFData       SpotInstanceInterruptionBehavior
instance ToByteString SpotInstanceInterruptionBehavior
instance ToQuery      SpotInstanceInterruptionBehavior
instance ToHeader     SpotInstanceInterruptionBehavior

instance FromXML SpotInstanceInterruptionBehavior where
    parseXML = parseXMLText "SpotInstanceInterruptionBehavior"

data SpotInstanceState
  = SISActive
  | SISCancelled
  | SISClosed
  | SISFailed
  | SISOpen
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotInstanceState where
    parser = takeLowerText >>= \case
        "active" -> pure SISActive
        "cancelled" -> pure SISCancelled
        "closed" -> pure SISClosed
        "failed" -> pure SISFailed
        "open" -> pure SISOpen
        e -> fromTextError $ "Failure parsing SpotInstanceState from value: '" <> e
           <> "'. Accepted values: active, cancelled, closed, failed, open"

instance ToText SpotInstanceState where
    toText = \case
        SISActive -> "active"
        SISCancelled -> "cancelled"
        SISClosed -> "closed"
        SISFailed -> "failed"
        SISOpen -> "open"

instance NFData       SpotInstanceState
instance ToByteString SpotInstanceState
instance ToQuery      SpotInstanceState
instance ToHeader     SpotInstanceState

instance FromXML SpotInstanceState where
    parseXML = parseXMLText "SpotInstanceState"

data SpotInstanceType
  = OneTime
  | Persistent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotInstanceType where
    parser = takeLowerText >>= \case
        "one-time" -> pure OneTime
        "persistent" -> pure Persistent
        e -> fromTextError $ "Failure parsing SpotInstanceType from value: '" <> e
           <> "'. Accepted values: one-time, persistent"

instance ToText SpotInstanceType where
    toText = \case
        OneTime -> "one-time"
        Persistent -> "persistent"

instance NFData       SpotInstanceType
instance ToByteString SpotInstanceType
instance ToQuery      SpotInstanceType
instance ToHeader     SpotInstanceType

instance FromXML SpotInstanceType where
    parseXML = parseXMLText "SpotInstanceType"

data State
  = SAvailable
  | SDeleted
  | SDeleting
  | SExpired
  | SFailed
  | SPending
  | SPendingAcceptance
  | SRejected
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText State where
    parser = takeLowerText >>= \case
        "available" -> pure SAvailable
        "deleted" -> pure SDeleted
        "deleting" -> pure SDeleting
        "expired" -> pure SExpired
        "failed" -> pure SFailed
        "pending" -> pure SPending
        "pendingacceptance" -> pure SPendingAcceptance
        "rejected" -> pure SRejected
        e -> fromTextError $ "Failure parsing State from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, expired, failed, pending, pendingacceptance, rejected"

instance ToText State where
    toText = \case
        SAvailable -> "Available"
        SDeleted -> "Deleted"
        SDeleting -> "Deleting"
        SExpired -> "Expired"
        SFailed -> "Failed"
        SPending -> "Pending"
        SPendingAcceptance -> "PendingAcceptance"
        SRejected -> "Rejected"

instance NFData       State
instance ToByteString State
instance ToQuery      State
instance ToHeader     State

instance FromXML State where
    parseXML = parseXMLText "State"

data StatusName =
  Reachability
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusName where
    parser = takeLowerText >>= \case
        "reachability" -> pure Reachability
        e -> fromTextError $ "Failure parsing StatusName from value: '" <> e
           <> "'. Accepted values: reachability"

instance ToText StatusName where
    toText = \case
        Reachability -> "reachability"

instance NFData       StatusName
instance ToByteString StatusName
instance ToQuery      StatusName
instance ToHeader     StatusName

instance FromXML StatusName where
    parseXML = parseXMLText "StatusName"

data StatusType
  = STFailed
  | STInitializing
  | STInsufficientData
  | STPassed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusType where
    parser = takeLowerText >>= \case
        "failed" -> pure STFailed
        "initializing" -> pure STInitializing
        "insufficient-data" -> pure STInsufficientData
        "passed" -> pure STPassed
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: failed, initializing, insufficient-data, passed"

instance ToText StatusType where
    toText = \case
        STFailed -> "failed"
        STInitializing -> "initializing"
        STInsufficientData -> "insufficient-data"
        STPassed -> "passed"

instance NFData       StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

data SubnetCidrBlockStateCode
  = SCBSCAssociated
  | SCBSCAssociating
  | SCBSCDisassociated
  | SCBSCDisassociating
  | SCBSCFailed
  | SCBSCFailing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubnetCidrBlockStateCode where
    parser = takeLowerText >>= \case
        "associated" -> pure SCBSCAssociated
        "associating" -> pure SCBSCAssociating
        "disassociated" -> pure SCBSCDisassociated
        "disassociating" -> pure SCBSCDisassociating
        "failed" -> pure SCBSCFailed
        "failing" -> pure SCBSCFailing
        e -> fromTextError $ "Failure parsing SubnetCidrBlockStateCode from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating, failed, failing"

instance ToText SubnetCidrBlockStateCode where
    toText = \case
        SCBSCAssociated -> "associated"
        SCBSCAssociating -> "associating"
        SCBSCDisassociated -> "disassociated"
        SCBSCDisassociating -> "disassociating"
        SCBSCFailed -> "failed"
        SCBSCFailing -> "failing"

instance NFData       SubnetCidrBlockStateCode
instance ToByteString SubnetCidrBlockStateCode
instance ToQuery      SubnetCidrBlockStateCode
instance ToHeader     SubnetCidrBlockStateCode

instance FromXML SubnetCidrBlockStateCode where
    parseXML = parseXMLText "SubnetCidrBlockStateCode"

data SubnetState
  = SubAvailable
  | SubPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubnetState where
    parser = takeLowerText >>= \case
        "available" -> pure SubAvailable
        "pending" -> pure SubPending
        e -> fromTextError $ "Failure parsing SubnetState from value: '" <> e
           <> "'. Accepted values: available, pending"

instance ToText SubnetState where
    toText = \case
        SubAvailable -> "available"
        SubPending -> "pending"

instance NFData       SubnetState
instance ToByteString SubnetState
instance ToQuery      SubnetState
instance ToHeader     SubnetState

instance FromXML SubnetState where
    parseXML = parseXMLText "SubnetState"

data SummaryStatus
  = SSImpaired
  | SSInitializing
  | SSInsufficientData
  | SSNotApplicable
  | SSOK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SummaryStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure SSImpaired
        "initializing" -> pure SSInitializing
        "insufficient-data" -> pure SSInsufficientData
        "not-applicable" -> pure SSNotApplicable
        "ok" -> pure SSOK
        e -> fromTextError $ "Failure parsing SummaryStatus from value: '" <> e
           <> "'. Accepted values: impaired, initializing, insufficient-data, not-applicable, ok"

instance ToText SummaryStatus where
    toText = \case
        SSImpaired -> "impaired"
        SSInitializing -> "initializing"
        SSInsufficientData -> "insufficient-data"
        SSNotApplicable -> "not-applicable"
        SSOK -> "ok"

instance NFData       SummaryStatus
instance ToByteString SummaryStatus
instance ToQuery      SummaryStatus
instance ToHeader     SummaryStatus

instance FromXML SummaryStatus where
    parseXML = parseXMLText "SummaryStatus"

data TelemetryStatus
  = Down
  | UP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TelemetryStatus where
    parser = takeLowerText >>= \case
        "down" -> pure Down
        "up" -> pure UP
        e -> fromTextError $ "Failure parsing TelemetryStatus from value: '" <> e
           <> "'. Accepted values: down, up"

instance ToText TelemetryStatus where
    toText = \case
        Down -> "DOWN"
        UP -> "UP"

instance NFData       TelemetryStatus
instance ToByteString TelemetryStatus
instance ToQuery      TelemetryStatus
instance ToHeader     TelemetryStatus

instance FromXML TelemetryStatus where
    parseXML = parseXMLText "TelemetryStatus"

data Tenancy
  = Dedicated
  | Default
  | Host
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Tenancy where
    parser = takeLowerText >>= \case
        "dedicated" -> pure Dedicated
        "default" -> pure Default
        "host" -> pure Host
        e -> fromTextError $ "Failure parsing Tenancy from value: '" <> e
           <> "'. Accepted values: dedicated, default, host"

instance ToText Tenancy where
    toText = \case
        Dedicated -> "dedicated"
        Default -> "default"
        Host -> "host"

instance NFData       Tenancy
instance ToByteString Tenancy
instance ToQuery      Tenancy
instance ToHeader     Tenancy

instance FromXML Tenancy where
    parseXML = parseXMLText "Tenancy"

data TrafficType
  = TTAccept
  | TTAll
  | TTReject
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TrafficType where
    parser = takeLowerText >>= \case
        "accept" -> pure TTAccept
        "all" -> pure TTAll
        "reject" -> pure TTReject
        e -> fromTextError $ "Failure parsing TrafficType from value: '" <> e
           <> "'. Accepted values: accept, all, reject"

instance ToText TrafficType where
    toText = \case
        TTAccept -> "ACCEPT"
        TTAll -> "ALL"
        TTReject -> "REJECT"

instance NFData       TrafficType
instance ToByteString TrafficType
instance ToQuery      TrafficType
instance ToHeader     TrafficType

instance FromXML TrafficType where
    parseXML = parseXMLText "TrafficType"

data UnsuccessfulInstanceCreditSpecificationErrorCode
  = IncorrectInstanceState
  | InstanceCreditSpecification_NotSupported
  | InvalidInstanceId_Malformed
  | InvalidInstanceId_NotFound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UnsuccessfulInstanceCreditSpecificationErrorCode where
    parser = takeLowerText >>= \case
        "incorrectinstancestate" -> pure IncorrectInstanceState
        "instancecreditspecification.notsupported" -> pure InstanceCreditSpecification_NotSupported
        "invalidinstanceid.malformed" -> pure InvalidInstanceId_Malformed
        "invalidinstanceid.notfound" -> pure InvalidInstanceId_NotFound
        e -> fromTextError $ "Failure parsing UnsuccessfulInstanceCreditSpecificationErrorCode from value: '" <> e
           <> "'. Accepted values: incorrectinstancestate, instancecreditspecification.notsupported, invalidinstanceid.malformed, invalidinstanceid.notfound"

instance ToText UnsuccessfulInstanceCreditSpecificationErrorCode where
    toText = \case
        IncorrectInstanceState -> "IncorrectInstanceState"
        InstanceCreditSpecification_NotSupported -> "InstanceCreditSpecification.NotSupported"
        InvalidInstanceId_Malformed -> "InvalidInstanceID.Malformed"
        InvalidInstanceId_NotFound -> "InvalidInstanceID.NotFound"

instance NFData       UnsuccessfulInstanceCreditSpecificationErrorCode
instance ToByteString UnsuccessfulInstanceCreditSpecificationErrorCode
instance ToQuery      UnsuccessfulInstanceCreditSpecificationErrorCode
instance ToHeader     UnsuccessfulInstanceCreditSpecificationErrorCode

instance FromXML UnsuccessfulInstanceCreditSpecificationErrorCode where
    parseXML = parseXMLText "UnsuccessfulInstanceCreditSpecificationErrorCode"

data VPCAttributeName
  = EnableDNSHostnames
  | EnableDNSSupport
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCAttributeName where
    parser = takeLowerText >>= \case
        "enablednshostnames" -> pure EnableDNSHostnames
        "enablednssupport" -> pure EnableDNSSupport
        e -> fromTextError $ "Failure parsing VPCAttributeName from value: '" <> e
           <> "'. Accepted values: enablednshostnames, enablednssupport"

instance ToText VPCAttributeName where
    toText = \case
        EnableDNSHostnames -> "enableDnsHostnames"
        EnableDNSSupport -> "enableDnsSupport"

instance NFData       VPCAttributeName
instance ToByteString VPCAttributeName
instance ToQuery      VPCAttributeName
instance ToHeader     VPCAttributeName

data VPCCidrBlockStateCode
  = VCBSCAssociated
  | VCBSCAssociating
  | VCBSCDisassociated
  | VCBSCDisassociating
  | VCBSCFailed
  | VCBSCFailing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCCidrBlockStateCode where
    parser = takeLowerText >>= \case
        "associated" -> pure VCBSCAssociated
        "associating" -> pure VCBSCAssociating
        "disassociated" -> pure VCBSCDisassociated
        "disassociating" -> pure VCBSCDisassociating
        "failed" -> pure VCBSCFailed
        "failing" -> pure VCBSCFailing
        e -> fromTextError $ "Failure parsing VPCCidrBlockStateCode from value: '" <> e
           <> "'. Accepted values: associated, associating, disassociated, disassociating, failed, failing"

instance ToText VPCCidrBlockStateCode where
    toText = \case
        VCBSCAssociated -> "associated"
        VCBSCAssociating -> "associating"
        VCBSCDisassociated -> "disassociated"
        VCBSCDisassociating -> "disassociating"
        VCBSCFailed -> "failed"
        VCBSCFailing -> "failing"

instance NFData       VPCCidrBlockStateCode
instance ToByteString VPCCidrBlockStateCode
instance ToQuery      VPCCidrBlockStateCode
instance ToHeader     VPCCidrBlockStateCode

instance FromXML VPCCidrBlockStateCode where
    parseXML = parseXMLText "VPCCidrBlockStateCode"

data VPCEndpointType
  = VETGateway
  | VETInterface
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCEndpointType where
    parser = takeLowerText >>= \case
        "gateway" -> pure VETGateway
        "interface" -> pure VETInterface
        e -> fromTextError $ "Failure parsing VPCEndpointType from value: '" <> e
           <> "'. Accepted values: gateway, interface"

instance ToText VPCEndpointType where
    toText = \case
        VETGateway -> "Gateway"
        VETInterface -> "Interface"

instance NFData       VPCEndpointType
instance ToByteString VPCEndpointType
instance ToQuery      VPCEndpointType
instance ToHeader     VPCEndpointType

instance FromXML VPCEndpointType where
    parseXML = parseXMLText "VPCEndpointType"

data VPCPeeringConnectionStateReasonCode
  = VPCSRCActive
  | VPCSRCDeleted
  | VPCSRCDeleting
  | VPCSRCExpired
  | VPCSRCFailed
  | VPCSRCInitiatingRequest
  | VPCSRCPendingAcceptance
  | VPCSRCProvisioning
  | VPCSRCRejected
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCPeeringConnectionStateReasonCode where
    parser = takeLowerText >>= \case
        "active" -> pure VPCSRCActive
        "deleted" -> pure VPCSRCDeleted
        "deleting" -> pure VPCSRCDeleting
        "expired" -> pure VPCSRCExpired
        "failed" -> pure VPCSRCFailed
        "initiating-request" -> pure VPCSRCInitiatingRequest
        "pending-acceptance" -> pure VPCSRCPendingAcceptance
        "provisioning" -> pure VPCSRCProvisioning
        "rejected" -> pure VPCSRCRejected
        e -> fromTextError $ "Failure parsing VPCPeeringConnectionStateReasonCode from value: '" <> e
           <> "'. Accepted values: active, deleted, deleting, expired, failed, initiating-request, pending-acceptance, provisioning, rejected"

instance ToText VPCPeeringConnectionStateReasonCode where
    toText = \case
        VPCSRCActive -> "active"
        VPCSRCDeleted -> "deleted"
        VPCSRCDeleting -> "deleting"
        VPCSRCExpired -> "expired"
        VPCSRCFailed -> "failed"
        VPCSRCInitiatingRequest -> "initiating-request"
        VPCSRCPendingAcceptance -> "pending-acceptance"
        VPCSRCProvisioning -> "provisioning"
        VPCSRCRejected -> "rejected"

instance NFData       VPCPeeringConnectionStateReasonCode
instance ToByteString VPCPeeringConnectionStateReasonCode
instance ToQuery      VPCPeeringConnectionStateReasonCode
instance ToHeader     VPCPeeringConnectionStateReasonCode

instance FromXML VPCPeeringConnectionStateReasonCode where
    parseXML = parseXMLText "VPCPeeringConnectionStateReasonCode"

data VPCState
  = VPCSAvailable
  | VPCSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCState where
    parser = takeLowerText >>= \case
        "available" -> pure VPCSAvailable
        "pending" -> pure VPCSPending
        e -> fromTextError $ "Failure parsing VPCState from value: '" <> e
           <> "'. Accepted values: available, pending"

instance ToText VPCState where
    toText = \case
        VPCSAvailable -> "available"
        VPCSPending -> "pending"

instance NFData       VPCState
instance ToByteString VPCState
instance ToQuery      VPCState
instance ToHeader     VPCState

instance FromXML VPCState where
    parseXML = parseXMLText "VPCState"

data VPCTenancy =
  VTDefault
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPCTenancy where
    parser = takeLowerText >>= \case
        "default" -> pure VTDefault
        e -> fromTextError $ "Failure parsing VPCTenancy from value: '" <> e
           <> "'. Accepted values: default"

instance ToText VPCTenancy where
    toText = \case
        VTDefault -> "default"

instance NFData       VPCTenancy
instance ToByteString VPCTenancy
instance ToQuery      VPCTenancy
instance ToHeader     VPCTenancy

data VPNState
  = VSAvailable
  | VSDeleted
  | VSDeleting
  | VSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPNState where
    parser = takeLowerText >>= \case
        "available" -> pure VSAvailable
        "deleted" -> pure VSDeleted
        "deleting" -> pure VSDeleting
        "pending" -> pure VSPending
        e -> fromTextError $ "Failure parsing VPNState from value: '" <> e
           <> "'. Accepted values: available, deleted, deleting, pending"

instance ToText VPNState where
    toText = \case
        VSAvailable -> "available"
        VSDeleted -> "deleted"
        VSDeleting -> "deleting"
        VSPending -> "pending"

instance NFData       VPNState
instance ToByteString VPNState
instance ToQuery      VPNState
instance ToHeader     VPNState

instance FromXML VPNState where
    parseXML = parseXMLText "VPNState"

data VPNStaticRouteSource =
  Static
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VPNStaticRouteSource where
    parser = takeLowerText >>= \case
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing VPNStaticRouteSource from value: '" <> e
           <> "'. Accepted values: static"

instance ToText VPNStaticRouteSource where
    toText = \case
        Static -> "Static"

instance NFData       VPNStaticRouteSource
instance ToByteString VPNStaticRouteSource
instance ToQuery      VPNStaticRouteSource
instance ToHeader     VPNStaticRouteSource

instance FromXML VPNStaticRouteSource where
    parseXML = parseXMLText "VPNStaticRouteSource"

data VirtualizationType
  = HVM
  | Paravirtual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VirtualizationType where
    parser = takeLowerText >>= \case
        "hvm" -> pure HVM
        "paravirtual" -> pure Paravirtual
        e -> fromTextError $ "Failure parsing VirtualizationType from value: '" <> e
           <> "'. Accepted values: hvm, paravirtual"

instance ToText VirtualizationType where
    toText = \case
        HVM -> "hvm"
        Paravirtual -> "paravirtual"

instance NFData       VirtualizationType
instance ToByteString VirtualizationType
instance ToQuery      VirtualizationType
instance ToHeader     VirtualizationType

instance FromXML VirtualizationType where
    parseXML = parseXMLText "VirtualizationType"

data VolumeAttachmentState
  = VAttached
  | VAttaching
  | VBusy
  | VDetached
  | VDetaching
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeAttachmentState where
    parser = takeLowerText >>= \case
        "attached" -> pure VAttached
        "attaching" -> pure VAttaching
        "busy" -> pure VBusy
        "detached" -> pure VDetached
        "detaching" -> pure VDetaching
        e -> fromTextError $ "Failure parsing VolumeAttachmentState from value: '" <> e
           <> "'. Accepted values: attached, attaching, busy, detached, detaching"

instance ToText VolumeAttachmentState where
    toText = \case
        VAttached -> "attached"
        VAttaching -> "attaching"
        VBusy -> "busy"
        VDetached -> "detached"
        VDetaching -> "detaching"

instance NFData       VolumeAttachmentState
instance ToByteString VolumeAttachmentState
instance ToQuery      VolumeAttachmentState
instance ToHeader     VolumeAttachmentState

instance FromXML VolumeAttachmentState where
    parseXML = parseXMLText "VolumeAttachmentState"

data VolumeAttributeName
  = VANAutoEnableIO
  | VANProductCodes
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeAttributeName where
    parser = takeLowerText >>= \case
        "autoenableio" -> pure VANAutoEnableIO
        "productcodes" -> pure VANProductCodes
        e -> fromTextError $ "Failure parsing VolumeAttributeName from value: '" <> e
           <> "'. Accepted values: autoenableio, productcodes"

instance ToText VolumeAttributeName where
    toText = \case
        VANAutoEnableIO -> "autoEnableIO"
        VANProductCodes -> "productCodes"

instance NFData       VolumeAttributeName
instance ToByteString VolumeAttributeName
instance ToQuery      VolumeAttributeName
instance ToHeader     VolumeAttributeName

data VolumeModificationState
  = Completed
  | Failed
  | Modifying
  | Optimizing
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeModificationState where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "modifying" -> pure Modifying
        "optimizing" -> pure Optimizing
        e -> fromTextError $ "Failure parsing VolumeModificationState from value: '" <> e
           <> "'. Accepted values: completed, failed, modifying, optimizing"

instance ToText VolumeModificationState where
    toText = \case
        Completed -> "completed"
        Failed -> "failed"
        Modifying -> "modifying"
        Optimizing -> "optimizing"

instance NFData       VolumeModificationState
instance ToByteString VolumeModificationState
instance ToQuery      VolumeModificationState
instance ToHeader     VolumeModificationState

instance FromXML VolumeModificationState where
    parseXML = parseXMLText "VolumeModificationState"

data VolumeState
  = VAvailable
  | VCreating
  | VDeleted
  | VDeleting
  | VError'
  | VInUse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeState where
    parser = takeLowerText >>= \case
        "available" -> pure VAvailable
        "creating" -> pure VCreating
        "deleted" -> pure VDeleted
        "deleting" -> pure VDeleting
        "error" -> pure VError'
        "in-use" -> pure VInUse
        e -> fromTextError $ "Failure parsing VolumeState from value: '" <> e
           <> "'. Accepted values: available, creating, deleted, deleting, error, in-use"

instance ToText VolumeState where
    toText = \case
        VAvailable -> "available"
        VCreating -> "creating"
        VDeleted -> "deleted"
        VDeleting -> "deleting"
        VError' -> "error"
        VInUse -> "in-use"

instance NFData       VolumeState
instance ToByteString VolumeState
instance ToQuery      VolumeState
instance ToHeader     VolumeState

instance FromXML VolumeState where
    parseXML = parseXMLText "VolumeState"

data VolumeStatusInfoStatus
  = Impaired
  | InsufficientData
  | OK
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeStatusInfoStatus where
    parser = takeLowerText >>= \case
        "impaired" -> pure Impaired
        "insufficient-data" -> pure InsufficientData
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing VolumeStatusInfoStatus from value: '" <> e
           <> "'. Accepted values: impaired, insufficient-data, ok"

instance ToText VolumeStatusInfoStatus where
    toText = \case
        Impaired -> "impaired"
        InsufficientData -> "insufficient-data"
        OK -> "ok"

instance NFData       VolumeStatusInfoStatus
instance ToByteString VolumeStatusInfoStatus
instance ToQuery      VolumeStatusInfoStatus
instance ToHeader     VolumeStatusInfoStatus

instance FromXML VolumeStatusInfoStatus where
    parseXML = parseXMLText "VolumeStatusInfoStatus"

data VolumeStatusName
  = IOEnabled
  | IOPerformance
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeStatusName where
    parser = takeLowerText >>= \case
        "io-enabled" -> pure IOEnabled
        "io-performance" -> pure IOPerformance
        e -> fromTextError $ "Failure parsing VolumeStatusName from value: '" <> e
           <> "'. Accepted values: io-enabled, io-performance"

instance ToText VolumeStatusName where
    toText = \case
        IOEnabled -> "io-enabled"
        IOPerformance -> "io-performance"

instance NFData       VolumeStatusName
instance ToByteString VolumeStatusName
instance ToQuery      VolumeStatusName
instance ToHeader     VolumeStatusName

instance FromXML VolumeStatusName where
    parseXML = parseXMLText "VolumeStatusName"

data VolumeType
  = GP2
  | IO1
  | SC1
  | ST1
  | Standard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2" -> pure GP2
        "io1" -> pure IO1
        "sc1" -> pure SC1
        "st1" -> pure ST1
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing VolumeType from value: '" <> e
           <> "'. Accepted values: gp2, io1, sc1, st1, standard"

instance ToText VolumeType where
    toText = \case
        GP2 -> "gp2"
        IO1 -> "io1"
        SC1 -> "sc1"
        ST1 -> "st1"
        Standard -> "standard"

instance NFData       VolumeType
instance ToByteString VolumeType
instance ToQuery      VolumeType
instance ToHeader     VolumeType

instance FromXML VolumeType where
    parseXML = parseXMLText "VolumeType"

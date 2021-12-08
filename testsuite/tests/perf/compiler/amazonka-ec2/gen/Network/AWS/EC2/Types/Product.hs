{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Product where

import Network.AWS.EC2.Types.Sum
import Network.AWS.Lens (Lens', lens, mapping, (#))
import Network.AWS.Prelude (NFData, FromXML(..), Text, Generic, Typeable, Data,
        UTCTime, ISO8601, ToQuery(..), NonEmpty, List1, ByteString, Base64, (.@?), parseXMLList,
        may, (.!@), _Coerce, _Default, _Time, (=:), (.@), toQueryList, parseXMLList1, _List1, _Base64
       )

-- | Describes an account attribute.
--
--
--
-- /See:/ 'accountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { _aaAttributeValues :: !(Maybe [AccountAttributeValue])
  , _aaAttributeName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaAttributeValues' - One or more values for the account attribute.
--
-- * 'aaAttributeName' - The name of the account attribute.
accountAttribute
    :: AccountAttribute
accountAttribute =
  AccountAttribute' {_aaAttributeValues = Nothing, _aaAttributeName = Nothing}


-- | One or more values for the account attribute.
aaAttributeValues :: Lens' AccountAttribute [AccountAttributeValue]
aaAttributeValues = lens _aaAttributeValues (\ s a -> s{_aaAttributeValues = a}) . _Default . _Coerce

-- | The name of the account attribute.
aaAttributeName :: Lens' AccountAttribute (Maybe Text)
aaAttributeName = lens _aaAttributeName (\ s a -> s{_aaAttributeName = a})

instance FromXML AccountAttribute where
        parseXML x
          = AccountAttribute' <$>
              (x .@? "attributeValueSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "attributeName")

instance NFData AccountAttribute where

-- | Describes a value of an account attribute.
--
--
--
-- /See:/ 'accountAttributeValue' smart constructor.
newtype AccountAttributeValue = AccountAttributeValue'
  { _aavAttributeValue :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aavAttributeValue' - The value of the attribute.
accountAttributeValue
    :: AccountAttributeValue
accountAttributeValue = AccountAttributeValue' {_aavAttributeValue = Nothing}


-- | The value of the attribute.
aavAttributeValue :: Lens' AccountAttributeValue (Maybe Text)
aavAttributeValue = lens _aavAttributeValue (\ s a -> s{_aavAttributeValue = a})

instance FromXML AccountAttributeValue where
        parseXML x
          = AccountAttributeValue' <$> (x .@? "attributeValue")

instance NFData AccountAttributeValue where

-- | Describes a running instance in a Spot Fleet.
--
--
--
-- /See:/ 'activeInstance' smart constructor.
data ActiveInstance = ActiveInstance'
  { _aiInstanceId            :: !(Maybe Text)
  , _aiInstanceHealth        :: !(Maybe InstanceHealthStatus)
  , _aiInstanceType          :: !(Maybe Text)
  , _aiSpotInstanceRequestId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActiveInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiInstanceId' - The ID of the instance.
--
-- * 'aiInstanceHealth' - The health status of the instance. If the status of either the instance status check or the system status check is @impaired@ , the health status of the instance is @unhealthy@ . Otherwise, the health status is @healthy@ .
--
-- * 'aiInstanceType' - The instance type.
--
-- * 'aiSpotInstanceRequestId' - The ID of the Spot Instance request.
activeInstance
    :: ActiveInstance
activeInstance =
  ActiveInstance'
    { _aiInstanceId = Nothing
    , _aiInstanceHealth = Nothing
    , _aiInstanceType = Nothing
    , _aiSpotInstanceRequestId = Nothing
    }


-- | The ID of the instance.
aiInstanceId :: Lens' ActiveInstance (Maybe Text)
aiInstanceId = lens _aiInstanceId (\ s a -> s{_aiInstanceId = a})

-- | The health status of the instance. If the status of either the instance status check or the system status check is @impaired@ , the health status of the instance is @unhealthy@ . Otherwise, the health status is @healthy@ .
aiInstanceHealth :: Lens' ActiveInstance (Maybe InstanceHealthStatus)
aiInstanceHealth = lens _aiInstanceHealth (\ s a -> s{_aiInstanceHealth = a})

-- | The instance type.
aiInstanceType :: Lens' ActiveInstance (Maybe Text)
aiInstanceType = lens _aiInstanceType (\ s a -> s{_aiInstanceType = a})

-- | The ID of the Spot Instance request.
aiSpotInstanceRequestId :: Lens' ActiveInstance (Maybe Text)
aiSpotInstanceRequestId = lens _aiSpotInstanceRequestId (\ s a -> s{_aiSpotInstanceRequestId = a})

instance FromXML ActiveInstance where
        parseXML x
          = ActiveInstance' <$>
              (x .@? "instanceId") <*> (x .@? "instanceHealth") <*>
                (x .@? "instanceType")
                <*> (x .@? "spotInstanceRequestId")

instance NFData ActiveInstance where

-- | Describes an Elastic IP address.
--
--
--
-- /See:/ 'address' smart constructor.
data Address = Address'
  { _aAssociationId           :: !(Maybe Text)
  , _aInstanceId              :: !(Maybe Text)
  , _aNetworkInterfaceOwnerId :: !(Maybe Text)
  , _aAllocationId            :: !(Maybe Text)
  , _aDomain                  :: !(Maybe DomainType)
  , _aNetworkInterfaceId      :: !(Maybe Text)
  , _aPrivateIPAddress        :: !(Maybe Text)
  , _aPublicIP                :: !(Maybe Text)
  , _aTags                    :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAssociationId' - The ID representing the association of the address with an instance in a VPC.
--
-- * 'aInstanceId' - The ID of the instance that the address is associated with (if any).
--
-- * 'aNetworkInterfaceOwnerId' - The ID of the AWS account that owns the network interface.
--
-- * 'aAllocationId' - The ID representing the allocation of the address for use with EC2-VPC.
--
-- * 'aDomain' - Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
--
-- * 'aNetworkInterfaceId' - The ID of the network interface.
--
-- * 'aPrivateIPAddress' - The private IP address associated with the Elastic IP address.
--
-- * 'aPublicIP' - The Elastic IP address.
--
-- * 'aTags' - Any tags assigned to the Elastic IP address.
address
    :: Address
address =
  Address'
    { _aAssociationId = Nothing
    , _aInstanceId = Nothing
    , _aNetworkInterfaceOwnerId = Nothing
    , _aAllocationId = Nothing
    , _aDomain = Nothing
    , _aNetworkInterfaceId = Nothing
    , _aPrivateIPAddress = Nothing
    , _aPublicIP = Nothing
    , _aTags = Nothing
    }


-- | The ID representing the association of the address with an instance in a VPC.
aAssociationId :: Lens' Address (Maybe Text)
aAssociationId = lens _aAssociationId (\ s a -> s{_aAssociationId = a})

-- | The ID of the instance that the address is associated with (if any).
aInstanceId :: Lens' Address (Maybe Text)
aInstanceId = lens _aInstanceId (\ s a -> s{_aInstanceId = a})

-- | The ID of the AWS account that owns the network interface.
aNetworkInterfaceOwnerId :: Lens' Address (Maybe Text)
aNetworkInterfaceOwnerId = lens _aNetworkInterfaceOwnerId (\ s a -> s{_aNetworkInterfaceOwnerId = a})

-- | The ID representing the allocation of the address for use with EC2-VPC.
aAllocationId :: Lens' Address (Maybe Text)
aAllocationId = lens _aAllocationId (\ s a -> s{_aAllocationId = a})

-- | Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
aDomain :: Lens' Address (Maybe DomainType)
aDomain = lens _aDomain (\ s a -> s{_aDomain = a})

-- | The ID of the network interface.
aNetworkInterfaceId :: Lens' Address (Maybe Text)
aNetworkInterfaceId = lens _aNetworkInterfaceId (\ s a -> s{_aNetworkInterfaceId = a})

-- | The private IP address associated with the Elastic IP address.
aPrivateIPAddress :: Lens' Address (Maybe Text)
aPrivateIPAddress = lens _aPrivateIPAddress (\ s a -> s{_aPrivateIPAddress = a})

-- | The Elastic IP address.
aPublicIP :: Lens' Address (Maybe Text)
aPublicIP = lens _aPublicIP (\ s a -> s{_aPublicIP = a})

-- | Any tags assigned to the Elastic IP address.
aTags :: Lens' Address [Tag]
aTags = lens _aTags (\ s a -> s{_aTags = a}) . _Default . _Coerce

instance FromXML Address where
        parseXML x
          = Address' <$>
              (x .@? "associationId") <*> (x .@? "instanceId") <*>
                (x .@? "networkInterfaceOwnerId")
                <*> (x .@? "allocationId")
                <*> (x .@? "domain")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "publicIp")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData Address where

-- | Describes a principal.
--
--
--
-- /See:/ 'allowedPrincipal' smart constructor.
data AllowedPrincipal = AllowedPrincipal'
  { _apPrincipalType :: !(Maybe PrincipalType)
  , _apPrincipal     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllowedPrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPrincipalType' - The type of principal.
--
-- * 'apPrincipal' - The Amazon Resource Name (ARN) of the principal.
allowedPrincipal
    :: AllowedPrincipal
allowedPrincipal =
  AllowedPrincipal' {_apPrincipalType = Nothing, _apPrincipal = Nothing}


-- | The type of principal.
apPrincipalType :: Lens' AllowedPrincipal (Maybe PrincipalType)
apPrincipalType = lens _apPrincipalType (\ s a -> s{_apPrincipalType = a})

-- | The Amazon Resource Name (ARN) of the principal.
apPrincipal :: Lens' AllowedPrincipal (Maybe Text)
apPrincipal = lens _apPrincipal (\ s a -> s{_apPrincipal = a})

instance FromXML AllowedPrincipal where
        parseXML x
          = AllowedPrincipal' <$>
              (x .@? "principalType") <*> (x .@? "principal")

instance NFData AllowedPrincipal where

-- | Describes a value for a resource attribute that is a Boolean value.
--
--
--
-- /See:/ 'attributeBooleanValue' smart constructor.
newtype AttributeBooleanValue = AttributeBooleanValue'
  { _abvValue :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeBooleanValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abvValue' - The attribute value. The valid values are @true@ or @false@ .
attributeBooleanValue
    :: AttributeBooleanValue
attributeBooleanValue = AttributeBooleanValue' {_abvValue = Nothing}


-- | The attribute value. The valid values are @true@ or @false@ .
abvValue :: Lens' AttributeBooleanValue (Maybe Bool)
abvValue = lens _abvValue (\ s a -> s{_abvValue = a})

instance FromXML AttributeBooleanValue where
        parseXML x
          = AttributeBooleanValue' <$> (x .@? "value")

instance NFData AttributeBooleanValue where

instance ToQuery AttributeBooleanValue where
        toQuery AttributeBooleanValue'{..}
          = mconcat ["Value" =: _abvValue]

-- | Describes a value for a resource attribute that is a String.
--
--
--
-- /See:/ 'attributeValue' smart constructor.
newtype AttributeValue = AttributeValue'
  { _avValue :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avValue' - The attribute value. The value is case-sensitive.
attributeValue
    :: AttributeValue
attributeValue = AttributeValue' {_avValue = Nothing}


-- | The attribute value. The value is case-sensitive.
avValue :: Lens' AttributeValue (Maybe Text)
avValue = lens _avValue (\ s a -> s{_avValue = a})

instance FromXML AttributeValue where
        parseXML x = AttributeValue' <$> (x .@? "value")

instance NFData AttributeValue where

instance ToQuery AttributeValue where
        toQuery AttributeValue'{..}
          = mconcat ["Value" =: _avValue]

-- | Describes an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azState      :: !(Maybe AvailabilityZoneState)
  , _azRegionName :: !(Maybe Text)
  , _azZoneName   :: !(Maybe Text)
  , _azMessages   :: !(Maybe [AvailabilityZoneMessage])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azState' - The state of the Availability Zone.
--
-- * 'azRegionName' - The name of the region.
--
-- * 'azZoneName' - The name of the Availability Zone.
--
-- * 'azMessages' - Any messages about the Availability Zone.
availabilityZone
    :: AvailabilityZone
availabilityZone =
  AvailabilityZone'
    { _azState = Nothing
    , _azRegionName = Nothing
    , _azZoneName = Nothing
    , _azMessages = Nothing
    }


-- | The state of the Availability Zone.
azState :: Lens' AvailabilityZone (Maybe AvailabilityZoneState)
azState = lens _azState (\ s a -> s{_azState = a})

-- | The name of the region.
azRegionName :: Lens' AvailabilityZone (Maybe Text)
azRegionName = lens _azRegionName (\ s a -> s{_azRegionName = a})

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\ s a -> s{_azZoneName = a})

-- | Any messages about the Availability Zone.
azMessages :: Lens' AvailabilityZone [AvailabilityZoneMessage]
azMessages = lens _azMessages (\ s a -> s{_azMessages = a}) . _Default . _Coerce

instance FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' <$>
              (x .@? "zoneState") <*> (x .@? "regionName") <*>
                (x .@? "zoneName")
                <*>
                (x .@? "messageSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData AvailabilityZone where

-- | Describes a message about an Availability Zone.
--
--
--
-- /See:/ 'availabilityZoneMessage' smart constructor.
newtype AvailabilityZoneMessage = AvailabilityZoneMessage'
  { _azmMessage :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZoneMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azmMessage' - The message about the Availability Zone.
availabilityZoneMessage
    :: AvailabilityZoneMessage
availabilityZoneMessage = AvailabilityZoneMessage' {_azmMessage = Nothing}


-- | The message about the Availability Zone.
azmMessage :: Lens' AvailabilityZoneMessage (Maybe Text)
azmMessage = lens _azmMessage (\ s a -> s{_azmMessage = a})

instance FromXML AvailabilityZoneMessage where
        parseXML x
          = AvailabilityZoneMessage' <$> (x .@? "message")

instance NFData AvailabilityZoneMessage where

-- | The capacity information for instances launched onto the Dedicated Host.
--
--
--
-- /See:/ 'availableCapacity' smart constructor.
data AvailableCapacity = AvailableCapacity'
  { _acAvailableInstanceCapacity :: !(Maybe [InstanceCapacity])
  , _acAvailableVCPUs            :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailableCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acAvailableInstanceCapacity' - The total number of instances that the Dedicated Host supports.
--
-- * 'acAvailableVCPUs' - The number of vCPUs available on the Dedicated Host.
availableCapacity
    :: AvailableCapacity
availableCapacity =
  AvailableCapacity'
    {_acAvailableInstanceCapacity = Nothing, _acAvailableVCPUs = Nothing}


-- | The total number of instances that the Dedicated Host supports.
acAvailableInstanceCapacity :: Lens' AvailableCapacity [InstanceCapacity]
acAvailableInstanceCapacity = lens _acAvailableInstanceCapacity (\ s a -> s{_acAvailableInstanceCapacity = a}) . _Default . _Coerce

-- | The number of vCPUs available on the Dedicated Host.
acAvailableVCPUs :: Lens' AvailableCapacity (Maybe Int)
acAvailableVCPUs = lens _acAvailableVCPUs (\ s a -> s{_acAvailableVCPUs = a})

instance FromXML AvailableCapacity where
        parseXML x
          = AvailableCapacity' <$>
              (x .@? "availableInstanceCapacity" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "availableVCpus")

instance NFData AvailableCapacity where

-- | /See:/ 'blobAttributeValue' smart constructor.
newtype BlobAttributeValue = BlobAttributeValue'
  { _bavValue :: Maybe Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BlobAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bavValue' - Undocumented member.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
blobAttributeValue
    :: BlobAttributeValue
blobAttributeValue = BlobAttributeValue' {_bavValue = Nothing}


-- | Undocumented member.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
bavValue :: Lens' BlobAttributeValue (Maybe ByteString)
bavValue = lens _bavValue (\ s a -> s{_bavValue = a}) . mapping _Base64

instance NFData BlobAttributeValue where

instance ToQuery BlobAttributeValue where
        toQuery BlobAttributeValue'{..}
          = mconcat ["Value" =: _bavValue]

-- | Describes a block device mapping.
--
--
--
-- /See:/ 'blockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { _bdmVirtualName :: !(Maybe Text)
  , _bdmNoDevice    :: !(Maybe Text)
  , _bdmEBS         :: !(Maybe EBSBlockDevice)
  , _bdmDeviceName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmVirtualName' - The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ .The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume. Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
--
-- * 'bdmNoDevice' - Suppresses the specified device included in the block device mapping of the AMI.
--
-- * 'bdmEBS' - Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- * 'bdmDeviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
blockDeviceMapping
    :: Text -- ^ 'bdmDeviceName'
    -> BlockDeviceMapping
blockDeviceMapping pDeviceName_ =
  BlockDeviceMapping'
    { _bdmVirtualName = Nothing
    , _bdmNoDevice = Nothing
    , _bdmEBS = Nothing
    , _bdmDeviceName = pDeviceName_
    }


-- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ .The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume. Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a})

-- | Suppresses the specified device included in the block device mapping of the AMI.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a})

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBSBlockDevice)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a})

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a})

instance FromXML BlockDeviceMapping where
        parseXML x
          = BlockDeviceMapping' <$>
              (x .@? "virtualName") <*> (x .@? "noDevice") <*>
                (x .@? "ebs")
                <*> (x .@ "deviceName")

instance NFData BlockDeviceMapping where

instance ToQuery BlockDeviceMapping where
        toQuery BlockDeviceMapping'{..}
          = mconcat
              ["VirtualName" =: _bdmVirtualName,
               "NoDevice" =: _bdmNoDevice, "Ebs" =: _bdmEBS,
               "DeviceName" =: _bdmDeviceName]

-- | Describes a bundle task.
--
--
--
-- /See:/ 'bundleTask' smart constructor.
data BundleTask = BundleTask'
  { _btBundleTaskError :: !(Maybe BundleTaskError)
  , _btBundleId        :: !Text
  , _btInstanceId      :: !Text
  , _btProgress        :: !Text
  , _btStartTime       :: !ISO8601
  , _btState           :: !BundleTaskState
  , _btStorage         :: !Storage
  , _btUpdateTime      :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BundleTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'btBundleTaskError' - If the task fails, a description of the error.
--
-- * 'btBundleId' - The ID of the bundle task.
--
-- * 'btInstanceId' - The ID of the instance associated with this bundle task.
--
-- * 'btProgress' - The level of task completion, as a percent (for example, 20%).
--
-- * 'btStartTime' - The time this task started.
--
-- * 'btState' - The state of the task.
--
-- * 'btStorage' - The Amazon S3 storage locations.
--
-- * 'btUpdateTime' - The time of the most recent update for the task.
bundleTask
    :: Text -- ^ 'btBundleId'
    -> Text -- ^ 'btInstanceId'
    -> Text -- ^ 'btProgress'
    -> UTCTime -- ^ 'btStartTime'
    -> BundleTaskState -- ^ 'btState'
    -> Storage -- ^ 'btStorage'
    -> UTCTime -- ^ 'btUpdateTime'
    -> BundleTask
bundleTask pBundleId_ pInstanceId_ pProgress_ pStartTime_ pState_ pStorage_ pUpdateTime_ =
  BundleTask'
    { _btBundleTaskError = Nothing
    , _btBundleId = pBundleId_
    , _btInstanceId = pInstanceId_
    , _btProgress = pProgress_
    , _btStartTime = _Time # pStartTime_
    , _btState = pState_
    , _btStorage = pStorage_
    , _btUpdateTime = _Time # pUpdateTime_
    }


-- | If the task fails, a description of the error.
btBundleTaskError :: Lens' BundleTask (Maybe BundleTaskError)
btBundleTaskError = lens _btBundleTaskError (\ s a -> s{_btBundleTaskError = a})

-- | The ID of the bundle task.
btBundleId :: Lens' BundleTask Text
btBundleId = lens _btBundleId (\ s a -> s{_btBundleId = a})

-- | The ID of the instance associated with this bundle task.
btInstanceId :: Lens' BundleTask Text
btInstanceId = lens _btInstanceId (\ s a -> s{_btInstanceId = a})

-- | The level of task completion, as a percent (for example, 20%).
btProgress :: Lens' BundleTask Text
btProgress = lens _btProgress (\ s a -> s{_btProgress = a})

-- | The time this task started.
btStartTime :: Lens' BundleTask UTCTime
btStartTime = lens _btStartTime (\ s a -> s{_btStartTime = a}) . _Time

-- | The state of the task.
btState :: Lens' BundleTask BundleTaskState
btState = lens _btState (\ s a -> s{_btState = a})

-- | The Amazon S3 storage locations.
btStorage :: Lens' BundleTask Storage
btStorage = lens _btStorage (\ s a -> s{_btStorage = a})

-- | The time of the most recent update for the task.
btUpdateTime :: Lens' BundleTask UTCTime
btUpdateTime = lens _btUpdateTime (\ s a -> s{_btUpdateTime = a}) . _Time

instance FromXML BundleTask where
        parseXML x
          = BundleTask' <$>
              (x .@? "error") <*> (x .@ "bundleId") <*>
                (x .@ "instanceId")
                <*> (x .@ "progress")
                <*> (x .@ "startTime")
                <*> (x .@ "state")
                <*> (x .@ "storage")
                <*> (x .@ "updateTime")

instance NFData BundleTask where

-- | Describes an error for 'BundleInstance' .
--
--
--
-- /See:/ 'bundleTaskError' smart constructor.
data BundleTaskError = BundleTaskError'
  { _bteCode    :: !(Maybe Text)
  , _bteMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BundleTaskError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bteCode' - The error code.
--
-- * 'bteMessage' - The error message.
bundleTaskError
    :: BundleTaskError
bundleTaskError = BundleTaskError' {_bteCode = Nothing, _bteMessage = Nothing}


-- | The error code.
bteCode :: Lens' BundleTaskError (Maybe Text)
bteCode = lens _bteCode (\ s a -> s{_bteCode = a})

-- | The error message.
bteMessage :: Lens' BundleTaskError (Maybe Text)
bteMessage = lens _bteMessage (\ s a -> s{_bteMessage = a})

instance FromXML BundleTaskError where
        parseXML x
          = BundleTaskError' <$>
              (x .@? "code") <*> (x .@? "message")

instance NFData BundleTaskError where

-- | The CPU options for the instance.
--
--
--
-- /See:/ 'cpuOptions' smart constructor.
data CPUOptions = CPUOptions'
  { _coCoreCount      :: !(Maybe Int)
  , _coThreadsPerCore :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CPUOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCoreCount' - The number of CPU cores for the instance.
--
-- * 'coThreadsPerCore' - The number of threads per CPU core.
cpuOptions
    :: CPUOptions
cpuOptions = CPUOptions' {_coCoreCount = Nothing, _coThreadsPerCore = Nothing}


-- | The number of CPU cores for the instance.
coCoreCount :: Lens' CPUOptions (Maybe Int)
coCoreCount = lens _coCoreCount (\ s a -> s{_coCoreCount = a})

-- | The number of threads per CPU core.
coThreadsPerCore :: Lens' CPUOptions (Maybe Int)
coThreadsPerCore = lens _coThreadsPerCore (\ s a -> s{_coThreadsPerCore = a})

instance FromXML CPUOptions where
        parseXML x
          = CPUOptions' <$>
              (x .@? "coreCount") <*> (x .@? "threadsPerCore")

instance NFData CPUOptions where

-- | The CPU options for the instance. Both the core count and threads per core must be specified in the request.
--
--
--
-- /See:/ 'cpuOptionsRequest' smart constructor.
data CPUOptionsRequest = CPUOptionsRequest'
  { _corCoreCount      :: !(Maybe Int)
  , _corThreadsPerCore :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CPUOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corCoreCount' - The number of CPU cores for the instance.
--
-- * 'corThreadsPerCore' - The number of threads per CPU core. To disable Intel Hyper-Threading Technology for the instance, specify a value of @1@ . Otherwise, specify the default value of @2@ .
cpuOptionsRequest
    :: CPUOptionsRequest
cpuOptionsRequest =
  CPUOptionsRequest' {_corCoreCount = Nothing, _corThreadsPerCore = Nothing}


-- | The number of CPU cores for the instance.
corCoreCount :: Lens' CPUOptionsRequest (Maybe Int)
corCoreCount = lens _corCoreCount (\ s a -> s{_corCoreCount = a})

-- | The number of threads per CPU core. To disable Intel Hyper-Threading Technology for the instance, specify a value of @1@ . Otherwise, specify the default value of @2@ .
corThreadsPerCore :: Lens' CPUOptionsRequest (Maybe Int)
corThreadsPerCore = lens _corThreadsPerCore (\ s a -> s{_corThreadsPerCore = a})

instance NFData CPUOptionsRequest where

instance ToQuery CPUOptionsRequest where
        toQuery CPUOptionsRequest'{..}
          = mconcat
              ["CoreCount" =: _corCoreCount,
               "ThreadsPerCore" =: _corThreadsPerCore]

-- | Describes a Spot Fleet error.
--
--
--
-- /See:/ 'cancelSpotFleetRequestsError' smart constructor.
data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError'
  { _csfreCode    :: !CancelBatchErrorCode
  , _csfreMessage :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelSpotFleetRequestsError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfreCode' - The error code.
--
-- * 'csfreMessage' - The description for the error code.
cancelSpotFleetRequestsError
    :: CancelBatchErrorCode -- ^ 'csfreCode'
    -> Text -- ^ 'csfreMessage'
    -> CancelSpotFleetRequestsError
cancelSpotFleetRequestsError pCode_ pMessage_ =
  CancelSpotFleetRequestsError' {_csfreCode = pCode_, _csfreMessage = pMessage_}


-- | The error code.
csfreCode :: Lens' CancelSpotFleetRequestsError CancelBatchErrorCode
csfreCode = lens _csfreCode (\ s a -> s{_csfreCode = a})

-- | The description for the error code.
csfreMessage :: Lens' CancelSpotFleetRequestsError Text
csfreMessage = lens _csfreMessage (\ s a -> s{_csfreMessage = a})

instance FromXML CancelSpotFleetRequestsError where
        parseXML x
          = CancelSpotFleetRequestsError' <$>
              (x .@ "code") <*> (x .@ "message")

instance NFData CancelSpotFleetRequestsError where

-- | Describes a Spot Fleet request that was not successfully canceled.
--
--
--
-- /See:/ 'cancelSpotFleetRequestsErrorItem' smart constructor.
data CancelSpotFleetRequestsErrorItem = CancelSpotFleetRequestsErrorItem'
  { _csfreiError              :: !CancelSpotFleetRequestsError
  , _csfreiSpotFleetRequestId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelSpotFleetRequestsErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfreiError' - The error.
--
-- * 'csfreiSpotFleetRequestId' - The ID of the Spot Fleet request.
cancelSpotFleetRequestsErrorItem
    :: CancelSpotFleetRequestsError -- ^ 'csfreiError'
    -> Text -- ^ 'csfreiSpotFleetRequestId'
    -> CancelSpotFleetRequestsErrorItem
cancelSpotFleetRequestsErrorItem pError_ pSpotFleetRequestId_ =
  CancelSpotFleetRequestsErrorItem'
    {_csfreiError = pError_, _csfreiSpotFleetRequestId = pSpotFleetRequestId_}


-- | The error.
csfreiError :: Lens' CancelSpotFleetRequestsErrorItem CancelSpotFleetRequestsError
csfreiError = lens _csfreiError (\ s a -> s{_csfreiError = a})

-- | The ID of the Spot Fleet request.
csfreiSpotFleetRequestId :: Lens' CancelSpotFleetRequestsErrorItem Text
csfreiSpotFleetRequestId = lens _csfreiSpotFleetRequestId (\ s a -> s{_csfreiSpotFleetRequestId = a})

instance FromXML CancelSpotFleetRequestsErrorItem
         where
        parseXML x
          = CancelSpotFleetRequestsErrorItem' <$>
              (x .@ "error") <*> (x .@ "spotFleetRequestId")

instance NFData CancelSpotFleetRequestsErrorItem
         where

-- | Describes a Spot Fleet request that was successfully canceled.
--
--
--
-- /See:/ 'cancelSpotFleetRequestsSuccessItem' smart constructor.
data CancelSpotFleetRequestsSuccessItem = CancelSpotFleetRequestsSuccessItem'
  { _csfrsiCurrentSpotFleetRequestState  :: !BatchState
  , _csfrsiPreviousSpotFleetRequestState :: !BatchState
  , _csfrsiSpotFleetRequestId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelSpotFleetRequestsSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfrsiCurrentSpotFleetRequestState' - The current state of the Spot Fleet request.
--
-- * 'csfrsiPreviousSpotFleetRequestState' - The previous state of the Spot Fleet request.
--
-- * 'csfrsiSpotFleetRequestId' - The ID of the Spot Fleet request.
cancelSpotFleetRequestsSuccessItem
    :: BatchState -- ^ 'csfrsiCurrentSpotFleetRequestState'
    -> BatchState -- ^ 'csfrsiPreviousSpotFleetRequestState'
    -> Text -- ^ 'csfrsiSpotFleetRequestId'
    -> CancelSpotFleetRequestsSuccessItem
cancelSpotFleetRequestsSuccessItem pCurrentSpotFleetRequestState_ pPreviousSpotFleetRequestState_ pSpotFleetRequestId_ =
  CancelSpotFleetRequestsSuccessItem'
    { _csfrsiCurrentSpotFleetRequestState = pCurrentSpotFleetRequestState_
    , _csfrsiPreviousSpotFleetRequestState = pPreviousSpotFleetRequestState_
    , _csfrsiSpotFleetRequestId = pSpotFleetRequestId_
    }


-- | The current state of the Spot Fleet request.
csfrsiCurrentSpotFleetRequestState :: Lens' CancelSpotFleetRequestsSuccessItem BatchState
csfrsiCurrentSpotFleetRequestState = lens _csfrsiCurrentSpotFleetRequestState (\ s a -> s{_csfrsiCurrentSpotFleetRequestState = a})

-- | The previous state of the Spot Fleet request.
csfrsiPreviousSpotFleetRequestState :: Lens' CancelSpotFleetRequestsSuccessItem BatchState
csfrsiPreviousSpotFleetRequestState = lens _csfrsiPreviousSpotFleetRequestState (\ s a -> s{_csfrsiPreviousSpotFleetRequestState = a})

-- | The ID of the Spot Fleet request.
csfrsiSpotFleetRequestId :: Lens' CancelSpotFleetRequestsSuccessItem Text
csfrsiSpotFleetRequestId = lens _csfrsiSpotFleetRequestId (\ s a -> s{_csfrsiSpotFleetRequestId = a})

instance FromXML CancelSpotFleetRequestsSuccessItem
         where
        parseXML x
          = CancelSpotFleetRequestsSuccessItem' <$>
              (x .@ "currentSpotFleetRequestState") <*>
                (x .@ "previousSpotFleetRequestState")
                <*> (x .@ "spotFleetRequestId")

instance NFData CancelSpotFleetRequestsSuccessItem
         where

-- | Describes a request to cancel a Spot Instance.
--
--
--
-- /See:/ 'cancelledSpotInstanceRequest' smart constructor.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest'
  { _csirState                 :: !(Maybe CancelSpotInstanceRequestState)
  , _csirSpotInstanceRequestId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelledSpotInstanceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csirState' - The state of the Spot Instance request.
--
-- * 'csirSpotInstanceRequestId' - The ID of the Spot Instance request.
cancelledSpotInstanceRequest
    :: CancelledSpotInstanceRequest
cancelledSpotInstanceRequest =
  CancelledSpotInstanceRequest'
    {_csirState = Nothing, _csirSpotInstanceRequestId = Nothing}


-- | The state of the Spot Instance request.
csirState :: Lens' CancelledSpotInstanceRequest (Maybe CancelSpotInstanceRequestState)
csirState = lens _csirState (\ s a -> s{_csirState = a})

-- | The ID of the Spot Instance request.
csirSpotInstanceRequestId :: Lens' CancelledSpotInstanceRequest (Maybe Text)
csirSpotInstanceRequestId = lens _csirSpotInstanceRequestId (\ s a -> s{_csirSpotInstanceRequestId = a})

instance FromXML CancelledSpotInstanceRequest where
        parseXML x
          = CancelledSpotInstanceRequest' <$>
              (x .@? "state") <*> (x .@? "spotInstanceRequestId")

instance NFData CancelledSpotInstanceRequest where

-- | Describes an IPv4 CIDR block.
--
--
--
-- /See:/ 'cidrBlock' smart constructor.
newtype CidrBlock = CidrBlock'
  { _cbCidrBlock :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbCidrBlock' - The IPv4 CIDR block.
cidrBlock
    :: CidrBlock
cidrBlock = CidrBlock' {_cbCidrBlock = Nothing}


-- | The IPv4 CIDR block.
cbCidrBlock :: Lens' CidrBlock (Maybe Text)
cbCidrBlock = lens _cbCidrBlock (\ s a -> s{_cbCidrBlock = a})

instance FromXML CidrBlock where
        parseXML x = CidrBlock' <$> (x .@? "cidrBlock")

instance NFData CidrBlock where

-- | Describes the ClassicLink DNS support status of a VPC.
--
--
--
-- /See:/ 'classicLinkDNSSupport' smart constructor.
data ClassicLinkDNSSupport = ClassicLinkDNSSupport'
  { _cldsVPCId                   :: !(Maybe Text)
  , _cldsClassicLinkDNSSupported :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClassicLinkDNSSupport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cldsVPCId' - The ID of the VPC.
--
-- * 'cldsClassicLinkDNSSupported' - Indicates whether ClassicLink DNS support is enabled for the VPC.
classicLinkDNSSupport
    :: ClassicLinkDNSSupport
classicLinkDNSSupport =
  ClassicLinkDNSSupport'
    {_cldsVPCId = Nothing, _cldsClassicLinkDNSSupported = Nothing}


-- | The ID of the VPC.
cldsVPCId :: Lens' ClassicLinkDNSSupport (Maybe Text)
cldsVPCId = lens _cldsVPCId (\ s a -> s{_cldsVPCId = a})

-- | Indicates whether ClassicLink DNS support is enabled for the VPC.
cldsClassicLinkDNSSupported :: Lens' ClassicLinkDNSSupport (Maybe Bool)
cldsClassicLinkDNSSupported = lens _cldsClassicLinkDNSSupported (\ s a -> s{_cldsClassicLinkDNSSupported = a})

instance FromXML ClassicLinkDNSSupport where
        parseXML x
          = ClassicLinkDNSSupport' <$>
              (x .@? "vpcId") <*> (x .@? "classicLinkDnsSupported")

instance NFData ClassicLinkDNSSupport where

-- | Describes a linked EC2-Classic instance.
--
--
--
-- /See:/ 'classicLinkInstance' smart constructor.
data ClassicLinkInstance = ClassicLinkInstance'
  { _cliInstanceId :: !(Maybe Text)
  , _cliGroups     :: !(Maybe [GroupIdentifier])
  , _cliVPCId      :: !(Maybe Text)
  , _cliTags       :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClassicLinkInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cliInstanceId' - The ID of the instance.
--
-- * 'cliGroups' - A list of security groups.
--
-- * 'cliVPCId' - The ID of the VPC.
--
-- * 'cliTags' - Any tags assigned to the instance.
classicLinkInstance
    :: ClassicLinkInstance
classicLinkInstance =
  ClassicLinkInstance'
    { _cliInstanceId = Nothing
    , _cliGroups = Nothing
    , _cliVPCId = Nothing
    , _cliTags = Nothing
    }


-- | The ID of the instance.
cliInstanceId :: Lens' ClassicLinkInstance (Maybe Text)
cliInstanceId = lens _cliInstanceId (\ s a -> s{_cliInstanceId = a})

-- | A list of security groups.
cliGroups :: Lens' ClassicLinkInstance [GroupIdentifier]
cliGroups = lens _cliGroups (\ s a -> s{_cliGroups = a}) . _Default . _Coerce

-- | The ID of the VPC.
cliVPCId :: Lens' ClassicLinkInstance (Maybe Text)
cliVPCId = lens _cliVPCId (\ s a -> s{_cliVPCId = a})

-- | Any tags assigned to the instance.
cliTags :: Lens' ClassicLinkInstance [Tag]
cliTags = lens _cliTags (\ s a -> s{_cliTags = a}) . _Default . _Coerce

instance FromXML ClassicLinkInstance where
        parseXML x
          = ClassicLinkInstance' <$>
              (x .@? "instanceId") <*>
                (x .@? "groupSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "vpcId")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData ClassicLinkInstance where

-- | Describes a Classic Load Balancer.
--
--
--
-- /See:/ 'classicLoadBalancer' smart constructor.
newtype ClassicLoadBalancer = ClassicLoadBalancer'
  { _clbName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClassicLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbName' - The name of the load balancer.
classicLoadBalancer
    :: Text -- ^ 'clbName'
    -> ClassicLoadBalancer
classicLoadBalancer pName_ = ClassicLoadBalancer' {_clbName = pName_}


-- | The name of the load balancer.
clbName :: Lens' ClassicLoadBalancer Text
clbName = lens _clbName (\ s a -> s{_clbName = a})

instance FromXML ClassicLoadBalancer where
        parseXML x = ClassicLoadBalancer' <$> (x .@ "name")

instance NFData ClassicLoadBalancer where

instance ToQuery ClassicLoadBalancer where
        toQuery ClassicLoadBalancer'{..}
          = mconcat ["Name" =: _clbName]

-- | Describes the Classic Load Balancers to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these Classic Load Balancers.
--
--
--
-- /See:/ 'classicLoadBalancersConfig' smart constructor.
newtype ClassicLoadBalancersConfig = ClassicLoadBalancersConfig'
  { _clbcClassicLoadBalancers :: List1 ClassicLoadBalancer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClassicLoadBalancersConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clbcClassicLoadBalancers' - One or more Classic Load Balancers.
classicLoadBalancersConfig
    :: NonEmpty ClassicLoadBalancer -- ^ 'clbcClassicLoadBalancers'
    -> ClassicLoadBalancersConfig
classicLoadBalancersConfig pClassicLoadBalancers_ =
  ClassicLoadBalancersConfig'
    {_clbcClassicLoadBalancers = _List1 # pClassicLoadBalancers_}


-- | One or more Classic Load Balancers.
clbcClassicLoadBalancers :: Lens' ClassicLoadBalancersConfig (NonEmpty ClassicLoadBalancer)
clbcClassicLoadBalancers = lens _clbcClassicLoadBalancers (\ s a -> s{_clbcClassicLoadBalancers = a}) . _List1

instance FromXML ClassicLoadBalancersConfig where
        parseXML x
          = ClassicLoadBalancersConfig' <$>
              (x .@? "classicLoadBalancers" .!@ mempty >>=
                 parseXMLList1 "item")

instance NFData ClassicLoadBalancersConfig where

instance ToQuery ClassicLoadBalancersConfig where
        toQuery ClassicLoadBalancersConfig'{..}
          = mconcat
              [toQueryList "ClassicLoadBalancers"
                 _clbcClassicLoadBalancers]

-- | Describes the client-specific data.
--
--
--
-- /See:/ 'clientData' smart constructor.
data ClientData = ClientData'
  { _cdUploadStart :: !(Maybe ISO8601)
  , _cdUploadSize  :: !(Maybe Double)
  , _cdUploadEnd   :: !(Maybe ISO8601)
  , _cdComment     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClientData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdUploadStart' - The time that the disk upload starts.
--
-- * 'cdUploadSize' - The size of the uploaded disk image, in GiB.
--
-- * 'cdUploadEnd' - The time that the disk upload ends.
--
-- * 'cdComment' - A user-defined comment about the disk upload.
clientData
    :: ClientData
clientData =
  ClientData'
    { _cdUploadStart = Nothing
    , _cdUploadSize = Nothing
    , _cdUploadEnd = Nothing
    , _cdComment = Nothing
    }


-- | The time that the disk upload starts.
cdUploadStart :: Lens' ClientData (Maybe UTCTime)
cdUploadStart = lens _cdUploadStart (\ s a -> s{_cdUploadStart = a}) . mapping _Time

-- | The size of the uploaded disk image, in GiB.
cdUploadSize :: Lens' ClientData (Maybe Double)
cdUploadSize = lens _cdUploadSize (\ s a -> s{_cdUploadSize = a})

-- | The time that the disk upload ends.
cdUploadEnd :: Lens' ClientData (Maybe UTCTime)
cdUploadEnd = lens _cdUploadEnd (\ s a -> s{_cdUploadEnd = a}) . mapping _Time

-- | A user-defined comment about the disk upload.
cdComment :: Lens' ClientData (Maybe Text)
cdComment = lens _cdComment (\ s a -> s{_cdComment = a})

instance NFData ClientData where

instance ToQuery ClientData where
        toQuery ClientData'{..}
          = mconcat
              ["UploadStart" =: _cdUploadStart,
               "UploadSize" =: _cdUploadSize,
               "UploadEnd" =: _cdUploadEnd, "Comment" =: _cdComment]

-- | Describes a connection notification for a VPC endpoint or VPC endpoint service.
--
--
--
-- /See:/ 'connectionNotification' smart constructor.
data ConnectionNotification = ConnectionNotification'
  { _cnConnectionNotificationState :: !(Maybe ConnectionNotificationState)
  , _cnConnectionNotificationType  :: !(Maybe ConnectionNotificationType)
  , _cnConnectionEvents            :: !(Maybe [Text])
  , _cnServiceId                   :: !(Maybe Text)
  , _cnVPCEndpointId               :: !(Maybe Text)
  , _cnConnectionNotificationId    :: !(Maybe Text)
  , _cnConnectionNotificationARN   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConnectionNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnConnectionNotificationState' - The state of the notification.
--
-- * 'cnConnectionNotificationType' - The type of notification.
--
-- * 'cnConnectionEvents' - The events for the notification. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
--
-- * 'cnServiceId' - The ID of the endpoint service.
--
-- * 'cnVPCEndpointId' - The ID of the VPC endpoint.
--
-- * 'cnConnectionNotificationId' - The ID of the notification.
--
-- * 'cnConnectionNotificationARN' - The ARN of the SNS topic for the notification.
connectionNotification
    :: ConnectionNotification
connectionNotification =
  ConnectionNotification'
    { _cnConnectionNotificationState = Nothing
    , _cnConnectionNotificationType = Nothing
    , _cnConnectionEvents = Nothing
    , _cnServiceId = Nothing
    , _cnVPCEndpointId = Nothing
    , _cnConnectionNotificationId = Nothing
    , _cnConnectionNotificationARN = Nothing
    }


-- | The state of the notification.
cnConnectionNotificationState :: Lens' ConnectionNotification (Maybe ConnectionNotificationState)
cnConnectionNotificationState = lens _cnConnectionNotificationState (\ s a -> s{_cnConnectionNotificationState = a})

-- | The type of notification.
cnConnectionNotificationType :: Lens' ConnectionNotification (Maybe ConnectionNotificationType)
cnConnectionNotificationType = lens _cnConnectionNotificationType (\ s a -> s{_cnConnectionNotificationType = a})

-- | The events for the notification. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
cnConnectionEvents :: Lens' ConnectionNotification [Text]
cnConnectionEvents = lens _cnConnectionEvents (\ s a -> s{_cnConnectionEvents = a}) . _Default . _Coerce

-- | The ID of the endpoint service.
cnServiceId :: Lens' ConnectionNotification (Maybe Text)
cnServiceId = lens _cnServiceId (\ s a -> s{_cnServiceId = a})

-- | The ID of the VPC endpoint.
cnVPCEndpointId :: Lens' ConnectionNotification (Maybe Text)
cnVPCEndpointId = lens _cnVPCEndpointId (\ s a -> s{_cnVPCEndpointId = a})

-- | The ID of the notification.
cnConnectionNotificationId :: Lens' ConnectionNotification (Maybe Text)
cnConnectionNotificationId = lens _cnConnectionNotificationId (\ s a -> s{_cnConnectionNotificationId = a})

-- | The ARN of the SNS topic for the notification.
cnConnectionNotificationARN :: Lens' ConnectionNotification (Maybe Text)
cnConnectionNotificationARN = lens _cnConnectionNotificationARN (\ s a -> s{_cnConnectionNotificationARN = a})

instance FromXML ConnectionNotification where
        parseXML x
          = ConnectionNotification' <$>
              (x .@? "connectionNotificationState") <*>
                (x .@? "connectionNotificationType")
                <*>
                (x .@? "connectionEvents" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "serviceId")
                <*> (x .@? "vpcEndpointId")
                <*> (x .@? "connectionNotificationId")
                <*> (x .@? "connectionNotificationArn")

instance NFData ConnectionNotification where

-- | Describes a conversion task.
--
--
--
-- /See:/ 'conversionTask' smart constructor.
data ConversionTask = ConversionTask'
  { _ctImportInstance   :: !(Maybe ImportInstanceTaskDetails)
  , _ctState            :: !(Maybe ConversionTaskState)
  , _ctStatusMessage    :: !(Maybe Text)
  , _ctImportVolume     :: !(Maybe ImportVolumeTaskDetails)
  , _ctConversionTaskId :: !(Maybe Text)
  , _ctExpirationTime   :: !(Maybe Text)
  , _ctTags             :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConversionTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctImportInstance' - If the task is for importing an instance, this contains information about the import instance task.
--
-- * 'ctState' - The state of the conversion task.
--
-- * 'ctStatusMessage' - The status message related to the conversion task.
--
-- * 'ctImportVolume' - If the task is for importing a volume, this contains information about the import volume task.
--
-- * 'ctConversionTaskId' - The ID of the conversion task.
--
-- * 'ctExpirationTime' - The time when the task expires. If the upload isn't complete before the expiration time, we automatically cancel the task.
--
-- * 'ctTags' - Any tags assigned to the task.
conversionTask
    :: ConversionTask
conversionTask =
  ConversionTask'
    { _ctImportInstance = Nothing
    , _ctState = Nothing
    , _ctStatusMessage = Nothing
    , _ctImportVolume = Nothing
    , _ctConversionTaskId = Nothing
    , _ctExpirationTime = Nothing
    , _ctTags = Nothing
    }


-- | If the task is for importing an instance, this contains information about the import instance task.
ctImportInstance :: Lens' ConversionTask (Maybe ImportInstanceTaskDetails)
ctImportInstance = lens _ctImportInstance (\ s a -> s{_ctImportInstance = a})

-- | The state of the conversion task.
ctState :: Lens' ConversionTask (Maybe ConversionTaskState)
ctState = lens _ctState (\ s a -> s{_ctState = a})

-- | The status message related to the conversion task.
ctStatusMessage :: Lens' ConversionTask (Maybe Text)
ctStatusMessage = lens _ctStatusMessage (\ s a -> s{_ctStatusMessage = a})

-- | If the task is for importing a volume, this contains information about the import volume task.
ctImportVolume :: Lens' ConversionTask (Maybe ImportVolumeTaskDetails)
ctImportVolume = lens _ctImportVolume (\ s a -> s{_ctImportVolume = a})

-- | The ID of the conversion task.
ctConversionTaskId :: Lens' ConversionTask (Maybe Text)
ctConversionTaskId = lens _ctConversionTaskId (\ s a -> s{_ctConversionTaskId = a})

-- | The time when the task expires. If the upload isn't complete before the expiration time, we automatically cancel the task.
ctExpirationTime :: Lens' ConversionTask (Maybe Text)
ctExpirationTime = lens _ctExpirationTime (\ s a -> s{_ctExpirationTime = a})

-- | Any tags assigned to the task.
ctTags :: Lens' ConversionTask [Tag]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a}) . _Default . _Coerce

instance FromXML ConversionTask where
        parseXML x
          = ConversionTask' <$>
              (x .@? "importInstance") <*> (x .@? "state") <*>
                (x .@? "statusMessage")
                <*> (x .@? "importVolume")
                <*> (x .@? "conversionTaskId")
                <*> (x .@? "expirationTime")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData ConversionTask where

-- | Describes the user or group to be added or removed from the permissions for a volume.
--
--
--
-- /See:/ 'createVolumePermission' smart constructor.
data CreateVolumePermission = CreateVolumePermission'
  { _cvpGroup  :: !(Maybe PermissionGroup)
  , _cvpUserId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVolumePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpGroup' - The specific group that is to be added or removed from a volume's list of create volume permissions.
--
-- * 'cvpUserId' - The specific AWS account ID that is to be added or removed from a volume's list of create volume permissions.
createVolumePermission
    :: CreateVolumePermission
createVolumePermission =
  CreateVolumePermission' {_cvpGroup = Nothing, _cvpUserId = Nothing}


-- | The specific group that is to be added or removed from a volume's list of create volume permissions.
cvpGroup :: Lens' CreateVolumePermission (Maybe PermissionGroup)
cvpGroup = lens _cvpGroup (\ s a -> s{_cvpGroup = a})

-- | The specific AWS account ID that is to be added or removed from a volume's list of create volume permissions.
cvpUserId :: Lens' CreateVolumePermission (Maybe Text)
cvpUserId = lens _cvpUserId (\ s a -> s{_cvpUserId = a})

instance FromXML CreateVolumePermission where
        parseXML x
          = CreateVolumePermission' <$>
              (x .@? "group") <*> (x .@? "userId")

instance NFData CreateVolumePermission where

instance ToQuery CreateVolumePermission where
        toQuery CreateVolumePermission'{..}
          = mconcat
              ["Group" =: _cvpGroup, "UserId" =: _cvpUserId]

-- | Describes modifications to the permissions for a volume.
--
--
--
-- /See:/ 'createVolumePermissionModifications' smart constructor.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications'
  { _cvpmRemove :: !(Maybe [CreateVolumePermission])
  , _cvpmAdd    :: !(Maybe [CreateVolumePermission])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVolumePermissionModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpmRemove' - Removes a specific AWS account ID or group from a volume's list of create volume permissions.
--
-- * 'cvpmAdd' - Adds a specific AWS account ID or group to a volume's list of create volume permissions.
createVolumePermissionModifications
    :: CreateVolumePermissionModifications
createVolumePermissionModifications =
  CreateVolumePermissionModifications'
    {_cvpmRemove = Nothing, _cvpmAdd = Nothing}


-- | Removes a specific AWS account ID or group from a volume's list of create volume permissions.
cvpmRemove :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmRemove = lens _cvpmRemove (\ s a -> s{_cvpmRemove = a}) . _Default . _Coerce

-- | Adds a specific AWS account ID or group to a volume's list of create volume permissions.
cvpmAdd :: Lens' CreateVolumePermissionModifications [CreateVolumePermission]
cvpmAdd = lens _cvpmAdd (\ s a -> s{_cvpmAdd = a}) . _Default . _Coerce

instance NFData CreateVolumePermissionModifications
         where

instance ToQuery CreateVolumePermissionModifications
         where
        toQuery CreateVolumePermissionModifications'{..}
          = mconcat
              [toQuery (toQueryList "Remove" <$> _cvpmRemove),
               toQuery (toQueryList "Add" <$> _cvpmAdd)]

-- | Describes the credit option for CPU usage of a T2 instance.
--
--
--
-- /See:/ 'creditSpecification' smart constructor.
newtype CreditSpecification = CreditSpecification'
  { _csCPUCredits :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreditSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCPUCredits' - The credit option for CPU usage of a T2 instance.
creditSpecification
    :: CreditSpecification
creditSpecification = CreditSpecification' {_csCPUCredits = Nothing}


-- | The credit option for CPU usage of a T2 instance.
csCPUCredits :: Lens' CreditSpecification (Maybe Text)
csCPUCredits = lens _csCPUCredits (\ s a -> s{_csCPUCredits = a})

instance FromXML CreditSpecification where
        parseXML x
          = CreditSpecification' <$> (x .@? "cpuCredits")

instance NFData CreditSpecification where

-- | The credit option for CPU usage of a T2 instance.
--
--
--
-- /See:/ 'creditSpecificationRequest' smart constructor.
newtype CreditSpecificationRequest = CreditSpecificationRequest'
  { _csrCPUCredits :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreditSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrCPUCredits' - The credit option for CPU usage of a T2 instance. Valid values are @standard@ and @unlimited@ .
creditSpecificationRequest
    :: Text -- ^ 'csrCPUCredits'
    -> CreditSpecificationRequest
creditSpecificationRequest pCPUCredits_ =
  CreditSpecificationRequest' {_csrCPUCredits = pCPUCredits_}


-- | The credit option for CPU usage of a T2 instance. Valid values are @standard@ and @unlimited@ .
csrCPUCredits :: Lens' CreditSpecificationRequest Text
csrCPUCredits = lens _csrCPUCredits (\ s a -> s{_csrCPUCredits = a})

instance NFData CreditSpecificationRequest where

instance ToQuery CreditSpecificationRequest where
        toQuery CreditSpecificationRequest'{..}
          = mconcat ["CpuCredits" =: _csrCPUCredits]

-- | Describes a customer gateway.
--
--
--
-- /See:/ 'customerGateway' smart constructor.
data CustomerGateway = CustomerGateway'
  { _cgTags              :: !(Maybe [Tag])
  , _cgBGPASN            :: !Text
  , _cgCustomerGatewayId :: !Text
  , _cgIPAddress         :: !Text
  , _cgState             :: !Text
  , _cgType              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomerGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgTags' - Any tags assigned to the customer gateway.
--
-- * 'cgBGPASN' - The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
--
-- * 'cgCustomerGatewayId' - The ID of the customer gateway.
--
-- * 'cgIPAddress' - The Internet-routable IP address of the customer gateway's outside interface.
--
-- * 'cgState' - The current state of the customer gateway (@pending | available | deleting | deleted@ ).
--
-- * 'cgType' - The type of VPN connection the customer gateway supports (@ipsec.1@ ).
customerGateway
    :: Text -- ^ 'cgBGPASN'
    -> Text -- ^ 'cgCustomerGatewayId'
    -> Text -- ^ 'cgIPAddress'
    -> Text -- ^ 'cgState'
    -> Text -- ^ 'cgType'
    -> CustomerGateway
customerGateway pBGPASN_ pCustomerGatewayId_ pIPAddress_ pState_ pType_ =
  CustomerGateway'
    { _cgTags = Nothing
    , _cgBGPASN = pBGPASN_
    , _cgCustomerGatewayId = pCustomerGatewayId_
    , _cgIPAddress = pIPAddress_
    , _cgState = pState_
    , _cgType = pType_
    }


-- | Any tags assigned to the customer gateway.
cgTags :: Lens' CustomerGateway [Tag]
cgTags = lens _cgTags (\ s a -> s{_cgTags = a}) . _Default . _Coerce

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
cgBGPASN :: Lens' CustomerGateway Text
cgBGPASN = lens _cgBGPASN (\ s a -> s{_cgBGPASN = a})

-- | The ID of the customer gateway.
cgCustomerGatewayId :: Lens' CustomerGateway Text
cgCustomerGatewayId = lens _cgCustomerGatewayId (\ s a -> s{_cgCustomerGatewayId = a})

-- | The Internet-routable IP address of the customer gateway's outside interface.
cgIPAddress :: Lens' CustomerGateway Text
cgIPAddress = lens _cgIPAddress (\ s a -> s{_cgIPAddress = a})

-- | The current state of the customer gateway (@pending | available | deleting | deleted@ ).
cgState :: Lens' CustomerGateway Text
cgState = lens _cgState (\ s a -> s{_cgState = a})

-- | The type of VPN connection the customer gateway supports (@ipsec.1@ ).
cgType :: Lens' CustomerGateway Text
cgType = lens _cgType (\ s a -> s{_cgType = a})

instance FromXML CustomerGateway where
        parseXML x
          = CustomerGateway' <$>
              (x .@? "tagSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@ "bgpAsn")
                <*> (x .@ "customerGatewayId")
                <*> (x .@ "ipAddress")
                <*> (x .@ "state")
                <*> (x .@ "type")

instance NFData CustomerGateway where

-- | Describes a DHCP configuration option.
--
--
--
-- /See:/ 'dhcpConfiguration' smart constructor.
data DHCPConfiguration = DHCPConfiguration'
  { _dcValues :: !(Maybe [AttributeValue])
  , _dcKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DHCPConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcValues' - One or more values for the DHCP option.
--
-- * 'dcKey' - The name of a DHCP option.
dhcpConfiguration
    :: DHCPConfiguration
dhcpConfiguration = DHCPConfiguration' {_dcValues = Nothing, _dcKey = Nothing}


-- | One or more values for the DHCP option.
dcValues :: Lens' DHCPConfiguration [AttributeValue]
dcValues = lens _dcValues (\ s a -> s{_dcValues = a}) . _Default . _Coerce

-- | The name of a DHCP option.
dcKey :: Lens' DHCPConfiguration (Maybe Text)
dcKey = lens _dcKey (\ s a -> s{_dcKey = a})

instance FromXML DHCPConfiguration where
        parseXML x
          = DHCPConfiguration' <$>
              (x .@? "valueSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "key")

instance NFData DHCPConfiguration where

-- | Describes a set of DHCP options.
--
--
--
-- /See:/ 'dhcpOptions' smart constructor.
data DHCPOptions = DHCPOptions'
  { _doDHCPConfigurations :: !(Maybe [DHCPConfiguration])
  , _doDHCPOptionsId      :: !(Maybe Text)
  , _doTags               :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DHCPOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doDHCPConfigurations' - One or more DHCP options in the set.
--
-- * 'doDHCPOptionsId' - The ID of the set of DHCP options.
--
-- * 'doTags' - Any tags assigned to the DHCP options set.
dhcpOptions
    :: DHCPOptions
dhcpOptions =
  DHCPOptions'
    { _doDHCPConfigurations = Nothing
    , _doDHCPOptionsId = Nothing
    , _doTags = Nothing
    }


-- | One or more DHCP options in the set.
doDHCPConfigurations :: Lens' DHCPOptions [DHCPConfiguration]
doDHCPConfigurations = lens _doDHCPConfigurations (\ s a -> s{_doDHCPConfigurations = a}) . _Default . _Coerce

-- | The ID of the set of DHCP options.
doDHCPOptionsId :: Lens' DHCPOptions (Maybe Text)
doDHCPOptionsId = lens _doDHCPOptionsId (\ s a -> s{_doDHCPOptionsId = a})

-- | Any tags assigned to the DHCP options set.
doTags :: Lens' DHCPOptions [Tag]
doTags = lens _doTags (\ s a -> s{_doTags = a}) . _Default . _Coerce

instance FromXML DHCPOptions where
        parseXML x
          = DHCPOptions' <$>
              (x .@? "dhcpConfigurationSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "dhcpOptionsId")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData DHCPOptions where

-- | Describes a DNS entry.
--
--
--
-- /See:/ 'dnsEntry' smart constructor.
data DNSEntry = DNSEntry'
  { _deHostedZoneId :: !(Maybe Text)
  , _deDNSName      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DNSEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deHostedZoneId' - The ID of the private hosted zone.
--
-- * 'deDNSName' - The DNS name.
dnsEntry
    :: DNSEntry
dnsEntry = DNSEntry' {_deHostedZoneId = Nothing, _deDNSName = Nothing}


-- | The ID of the private hosted zone.
deHostedZoneId :: Lens' DNSEntry (Maybe Text)
deHostedZoneId = lens _deHostedZoneId (\ s a -> s{_deHostedZoneId = a})

-- | The DNS name.
deDNSName :: Lens' DNSEntry (Maybe Text)
deDNSName = lens _deDNSName (\ s a -> s{_deDNSName = a})

instance FromXML DNSEntry where
        parseXML x
          = DNSEntry' <$>
              (x .@? "hostedZoneId") <*> (x .@? "dnsName")

instance NFData DNSEntry where

-- | Describes an EC2 Fleet error.
--
--
--
-- /See:/ 'deleteFleetError' smart constructor.
data DeleteFleetError = DeleteFleetError'
  { _dfeCode    :: !(Maybe DeleteFleetErrorCode)
  , _dfeMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleetError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfeCode' - The error code.
--
-- * 'dfeMessage' - The description for the error code.
deleteFleetError
    :: DeleteFleetError
deleteFleetError = DeleteFleetError' {_dfeCode = Nothing, _dfeMessage = Nothing}


-- | The error code.
dfeCode :: Lens' DeleteFleetError (Maybe DeleteFleetErrorCode)
dfeCode = lens _dfeCode (\ s a -> s{_dfeCode = a})

-- | The description for the error code.
dfeMessage :: Lens' DeleteFleetError (Maybe Text)
dfeMessage = lens _dfeMessage (\ s a -> s{_dfeMessage = a})

instance FromXML DeleteFleetError where
        parseXML x
          = DeleteFleetError' <$>
              (x .@? "code") <*> (x .@? "message")

instance NFData DeleteFleetError where

-- | Describes an EC2 Fleet that was not successfully deleted.
--
--
--
-- /See:/ 'deleteFleetErrorItem' smart constructor.
data DeleteFleetErrorItem = DeleteFleetErrorItem'
  { _dfeiError   :: !(Maybe DeleteFleetError)
  , _dfeiFleetId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleetErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfeiError' - The error.
--
-- * 'dfeiFleetId' - The ID of the EC2 Fleet.
deleteFleetErrorItem
    :: DeleteFleetErrorItem
deleteFleetErrorItem =
  DeleteFleetErrorItem' {_dfeiError = Nothing, _dfeiFleetId = Nothing}


-- | The error.
dfeiError :: Lens' DeleteFleetErrorItem (Maybe DeleteFleetError)
dfeiError = lens _dfeiError (\ s a -> s{_dfeiError = a})

-- | The ID of the EC2 Fleet.
dfeiFleetId :: Lens' DeleteFleetErrorItem (Maybe Text)
dfeiFleetId = lens _dfeiFleetId (\ s a -> s{_dfeiFleetId = a})

instance FromXML DeleteFleetErrorItem where
        parseXML x
          = DeleteFleetErrorItem' <$>
              (x .@? "error") <*> (x .@? "fleetId")

instance NFData DeleteFleetErrorItem where

-- | Describes an EC2 Fleet that was successfully deleted.
--
--
--
-- /See:/ 'deleteFleetSuccessItem' smart constructor.
data DeleteFleetSuccessItem = DeleteFleetSuccessItem'
  { _dfsiCurrentFleetState  :: !(Maybe FleetStateCode)
  , _dfsiPreviousFleetState :: !(Maybe FleetStateCode)
  , _dfsiFleetId            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFleetSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsiCurrentFleetState' - The current state of the EC2 Fleet.
--
-- * 'dfsiPreviousFleetState' - The previous state of the EC2 Fleet.
--
-- * 'dfsiFleetId' - The ID of the EC2 Fleet.
deleteFleetSuccessItem
    :: DeleteFleetSuccessItem
deleteFleetSuccessItem =
  DeleteFleetSuccessItem'
    { _dfsiCurrentFleetState = Nothing
    , _dfsiPreviousFleetState = Nothing
    , _dfsiFleetId = Nothing
    }


-- | The current state of the EC2 Fleet.
dfsiCurrentFleetState :: Lens' DeleteFleetSuccessItem (Maybe FleetStateCode)
dfsiCurrentFleetState = lens _dfsiCurrentFleetState (\ s a -> s{_dfsiCurrentFleetState = a})

-- | The previous state of the EC2 Fleet.
dfsiPreviousFleetState :: Lens' DeleteFleetSuccessItem (Maybe FleetStateCode)
dfsiPreviousFleetState = lens _dfsiPreviousFleetState (\ s a -> s{_dfsiPreviousFleetState = a})

-- | The ID of the EC2 Fleet.
dfsiFleetId :: Lens' DeleteFleetSuccessItem (Maybe Text)
dfsiFleetId = lens _dfsiFleetId (\ s a -> s{_dfsiFleetId = a})

instance FromXML DeleteFleetSuccessItem where
        parseXML x
          = DeleteFleetSuccessItem' <$>
              (x .@? "currentFleetState") <*>
                (x .@? "previousFleetState")
                <*> (x .@? "fleetId")

instance NFData DeleteFleetSuccessItem where

-- | Describes a launch template version that could not be deleted.
--
--
--
-- /See:/ 'deleteLaunchTemplateVersionsResponseErrorItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseErrorItem = DeleteLaunchTemplateVersionsResponseErrorItem'
  { _dltvreiLaunchTemplateName :: !(Maybe Text)
  , _dltvreiLaunchTemplateId   :: !(Maybe Text)
  , _dltvreiVersionNumber      :: !(Maybe Integer)
  , _dltvreiResponseError      :: !(Maybe ResponseError)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLaunchTemplateVersionsResponseErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltvreiLaunchTemplateName' - The name of the launch template.
--
-- * 'dltvreiLaunchTemplateId' - The ID of the launch template.
--
-- * 'dltvreiVersionNumber' - The version number of the launch template.
--
-- * 'dltvreiResponseError' - Information about the error.
deleteLaunchTemplateVersionsResponseErrorItem
    :: DeleteLaunchTemplateVersionsResponseErrorItem
deleteLaunchTemplateVersionsResponseErrorItem =
  DeleteLaunchTemplateVersionsResponseErrorItem'
    { _dltvreiLaunchTemplateName = Nothing
    , _dltvreiLaunchTemplateId = Nothing
    , _dltvreiVersionNumber = Nothing
    , _dltvreiResponseError = Nothing
    }


-- | The name of the launch template.
dltvreiLaunchTemplateName :: Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Maybe Text)
dltvreiLaunchTemplateName = lens _dltvreiLaunchTemplateName (\ s a -> s{_dltvreiLaunchTemplateName = a})

-- | The ID of the launch template.
dltvreiLaunchTemplateId :: Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Maybe Text)
dltvreiLaunchTemplateId = lens _dltvreiLaunchTemplateId (\ s a -> s{_dltvreiLaunchTemplateId = a})

-- | The version number of the launch template.
dltvreiVersionNumber :: Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Maybe Integer)
dltvreiVersionNumber = lens _dltvreiVersionNumber (\ s a -> s{_dltvreiVersionNumber = a})

-- | Information about the error.
dltvreiResponseError :: Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Maybe ResponseError)
dltvreiResponseError = lens _dltvreiResponseError (\ s a -> s{_dltvreiResponseError = a})

instance FromXML
           DeleteLaunchTemplateVersionsResponseErrorItem
         where
        parseXML x
          = DeleteLaunchTemplateVersionsResponseErrorItem' <$>
              (x .@? "launchTemplateName") <*>
                (x .@? "launchTemplateId")
                <*> (x .@? "versionNumber")
                <*> (x .@? "responseError")

instance NFData
           DeleteLaunchTemplateVersionsResponseErrorItem
         where

-- | Describes a launch template version that was successfully deleted.
--
--
--
-- /See:/ 'deleteLaunchTemplateVersionsResponseSuccessItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseSuccessItem = DeleteLaunchTemplateVersionsResponseSuccessItem'
  { _dltvrsiLaunchTemplateName :: !(Maybe Text)
  , _dltvrsiLaunchTemplateId   :: !(Maybe Text)
  , _dltvrsiVersionNumber      :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLaunchTemplateVersionsResponseSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltvrsiLaunchTemplateName' - The name of the launch template.
--
-- * 'dltvrsiLaunchTemplateId' - The ID of the launch template.
--
-- * 'dltvrsiVersionNumber' - The version number of the launch template.
deleteLaunchTemplateVersionsResponseSuccessItem
    :: DeleteLaunchTemplateVersionsResponseSuccessItem
deleteLaunchTemplateVersionsResponseSuccessItem =
  DeleteLaunchTemplateVersionsResponseSuccessItem'
    { _dltvrsiLaunchTemplateName = Nothing
    , _dltvrsiLaunchTemplateId = Nothing
    , _dltvrsiVersionNumber = Nothing
    }


-- | The name of the launch template.
dltvrsiLaunchTemplateName :: Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Maybe Text)
dltvrsiLaunchTemplateName = lens _dltvrsiLaunchTemplateName (\ s a -> s{_dltvrsiLaunchTemplateName = a})

-- | The ID of the launch template.
dltvrsiLaunchTemplateId :: Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Maybe Text)
dltvrsiLaunchTemplateId = lens _dltvrsiLaunchTemplateId (\ s a -> s{_dltvrsiLaunchTemplateId = a})

-- | The version number of the launch template.
dltvrsiVersionNumber :: Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Maybe Integer)
dltvrsiVersionNumber = lens _dltvrsiVersionNumber (\ s a -> s{_dltvrsiVersionNumber = a})

instance FromXML
           DeleteLaunchTemplateVersionsResponseSuccessItem
         where
        parseXML x
          = DeleteLaunchTemplateVersionsResponseSuccessItem'
              <$>
              (x .@? "launchTemplateName") <*>
                (x .@? "launchTemplateId")
                <*> (x .@? "versionNumber")

instance NFData
           DeleteLaunchTemplateVersionsResponseSuccessItem
         where

-- | Describes a disk image.
--
--
--
-- /See:/ 'diskImage' smart constructor.
data DiskImage = DiskImage'
  { _diImage       :: !(Maybe DiskImageDetail)
  , _diVolume      :: !(Maybe VolumeDetail)
  , _diDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiskImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diImage' - Information about the disk image.
--
-- * 'diVolume' - Information about the volume.
--
-- * 'diDescription' - A description of the disk image.
diskImage
    :: DiskImage
diskImage =
  DiskImage' {_diImage = Nothing, _diVolume = Nothing, _diDescription = Nothing}


-- | Information about the disk image.
diImage :: Lens' DiskImage (Maybe DiskImageDetail)
diImage = lens _diImage (\ s a -> s{_diImage = a})

-- | Information about the volume.
diVolume :: Lens' DiskImage (Maybe VolumeDetail)
diVolume = lens _diVolume (\ s a -> s{_diVolume = a})

-- | A description of the disk image.
diDescription :: Lens' DiskImage (Maybe Text)
diDescription = lens _diDescription (\ s a -> s{_diDescription = a})

instance NFData DiskImage where

instance ToQuery DiskImage where
        toQuery DiskImage'{..}
          = mconcat
              ["Image" =: _diImage, "Volume" =: _diVolume,
               "Description" =: _diDescription]

-- | Describes a disk image.
--
--
--
-- /See:/ 'diskImageDescription' smart constructor.
data DiskImageDescription = DiskImageDescription'
  { _dSize              :: !(Maybe Integer)
  , _dChecksum          :: !(Maybe Text)
  , _dFormat            :: !(Maybe DiskImageFormat)
  , _dImportManifestURL :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiskImageDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSize' - The size of the disk image, in GiB.
--
-- * 'dChecksum' - The checksum computed for the disk image.
--
-- * 'dFormat' - The disk image format.
--
-- * 'dImportManifestURL' - A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ . For information about the import manifest referenced by this API action, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
diskImageDescription
    :: DiskImageDescription
diskImageDescription =
  DiskImageDescription'
    { _dSize = Nothing
    , _dChecksum = Nothing
    , _dFormat = Nothing
    , _dImportManifestURL = Nothing
    }


-- | The size of the disk image, in GiB.
dSize :: Lens' DiskImageDescription (Maybe Integer)
dSize = lens _dSize (\ s a -> s{_dSize = a})

-- | The checksum computed for the disk image.
dChecksum :: Lens' DiskImageDescription (Maybe Text)
dChecksum = lens _dChecksum (\ s a -> s{_dChecksum = a})

-- | The disk image format.
dFormat :: Lens' DiskImageDescription (Maybe DiskImageFormat)
dFormat = lens _dFormat (\ s a -> s{_dFormat = a})

-- | A presigned URL for the import manifest stored in Amazon S3. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ . For information about the import manifest referenced by this API action, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
dImportManifestURL :: Lens' DiskImageDescription (Maybe Text)
dImportManifestURL = lens _dImportManifestURL (\ s a -> s{_dImportManifestURL = a})

instance FromXML DiskImageDescription where
        parseXML x
          = DiskImageDescription' <$>
              (x .@? "size") <*> (x .@? "checksum") <*>
                (x .@? "format")
                <*> (x .@? "importManifestUrl")

instance NFData DiskImageDescription where

-- | Describes a disk image.
--
--
--
-- /See:/ 'diskImageDetail' smart constructor.
data DiskImageDetail = DiskImageDetail'
  { _didBytes             :: !Integer
  , _didFormat            :: !DiskImageFormat
  , _didImportManifestURL :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiskImageDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'didBytes' - The size of the disk image, in GiB.
--
-- * 'didFormat' - The disk image format.
--
-- * 'didImportManifestURL' - A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ . For information about the import manifest referenced by this API action, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
diskImageDetail
    :: Integer -- ^ 'didBytes'
    -> DiskImageFormat -- ^ 'didFormat'
    -> Text -- ^ 'didImportManifestURL'
    -> DiskImageDetail
diskImageDetail pBytes_ pFormat_ pImportManifestURL_ =
  DiskImageDetail'
    { _didBytes = pBytes_
    , _didFormat = pFormat_
    , _didImportManifestURL = pImportManifestURL_
    }


-- | The size of the disk image, in GiB.
didBytes :: Lens' DiskImageDetail Integer
didBytes = lens _didBytes (\ s a -> s{_didBytes = a})

-- | The disk image format.
didFormat :: Lens' DiskImageDetail DiskImageFormat
didFormat = lens _didFormat (\ s a -> s{_didFormat = a})

-- | A presigned URL for the import manifest stored in Amazon S3 and presented here as an Amazon S3 presigned URL. For information about creating a presigned URL for an Amazon S3 object, read the "Query String Request Authentication Alternative" section of the <http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html Authenticating REST Requests> topic in the /Amazon Simple Storage Service Developer Guide/ . For information about the import manifest referenced by this API action, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
didImportManifestURL :: Lens' DiskImageDetail Text
didImportManifestURL = lens _didImportManifestURL (\ s a -> s{_didImportManifestURL = a})

instance NFData DiskImageDetail where

instance ToQuery DiskImageDetail where
        toQuery DiskImageDetail'{..}
          = mconcat
              ["Bytes" =: _didBytes, "Format" =: _didFormat,
               "ImportManifestUrl" =: _didImportManifestURL]

-- | Describes a disk image volume.
--
--
--
-- /See:/ 'diskImageVolumeDescription' smart constructor.
data DiskImageVolumeDescription = DiskImageVolumeDescription'
  { _divdSize :: !(Maybe Integer)
  , _divdId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiskImageVolumeDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'divdSize' - The size of the volume, in GiB.
--
-- * 'divdId' - The volume identifier.
diskImageVolumeDescription
    :: DiskImageVolumeDescription
diskImageVolumeDescription =
  DiskImageVolumeDescription' {_divdSize = Nothing, _divdId = Nothing}


-- | The size of the volume, in GiB.
divdSize :: Lens' DiskImageVolumeDescription (Maybe Integer)
divdSize = lens _divdSize (\ s a -> s{_divdSize = a})

-- | The volume identifier.
divdId :: Lens' DiskImageVolumeDescription (Maybe Text)
divdId = lens _divdId (\ s a -> s{_divdId = a})

instance FromXML DiskImageVolumeDescription where
        parseXML x
          = DiskImageVolumeDescription' <$>
              (x .@? "size") <*> (x .@? "id")

instance NFData DiskImageVolumeDescription where

-- | Describes a block device for an EBS volume.
--
--
--
-- /See:/ 'ebsBlockDevice' smart constructor.
data EBSBlockDevice = EBSBlockDevice'
  { _ebdDeleteOnTermination :: !(Maybe Bool)
  , _ebdVolumeSize          :: !(Maybe Int)
  , _ebdIOPS                :: !(Maybe Int)
  , _ebdEncrypted           :: !(Maybe Bool)
  , _ebdKMSKeyId            :: !(Maybe Text)
  , _ebdVolumeType          :: !(Maybe VolumeType)
  , _ebdSnapshotId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSBlockDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebdDeleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- * 'ebdVolumeSize' - The size of the volume, in GiB. Constraints: 1-16384 for General Purpose SSD (@gp2@ ), 4-16384 for Provisioned IOPS SSD (@io1@ ), 500-16384 for Throughput Optimized HDD (@st1@ ), 500-16384 for Cold HDD (@sc1@ ), and 1-1024 for Magnetic (@standard@ ) volumes. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'ebdIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For @io1@ , this represents the number of IOPS that are provisioned for the volume. For @gp2@ , this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about General Purpose SSD baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraint: Range is 100-20000 IOPS for @io1@ volumes and 100-10000 IOPS for @gp2@ volumes. Condition: This parameter is required for requests to create @io1@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- * 'ebdEncrypted' - Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value. This is because only blank volumes can be encrypted on creation.
--
-- * 'ebdKMSKeyId' - Identifier (key ID, key alias, ID ARN, or alias ARN) for a user-managed CMK under which the EBS volume is encrypted. Note: This parameter is only supported on @BlockDeviceMapping@ objects called by <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> , <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet> , and <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances> .
--
-- * 'ebdVolumeType' - The volume type: @gp2@ , @io1@ , @st1@ , @sc1@ , or @standard@ . Default: @standard@
--
-- * 'ebdSnapshotId' - The ID of the snapshot.
ebsBlockDevice
    :: EBSBlockDevice
ebsBlockDevice =
  EBSBlockDevice'
    { _ebdDeleteOnTermination = Nothing
    , _ebdVolumeSize = Nothing
    , _ebdIOPS = Nothing
    , _ebdEncrypted = Nothing
    , _ebdKMSKeyId = Nothing
    , _ebdVolumeType = Nothing
    , _ebdSnapshotId = Nothing
    }


-- | Indicates whether the EBS volume is deleted on instance termination.
ebdDeleteOnTermination :: Lens' EBSBlockDevice (Maybe Bool)
ebdDeleteOnTermination = lens _ebdDeleteOnTermination (\ s a -> s{_ebdDeleteOnTermination = a})

-- | The size of the volume, in GiB. Constraints: 1-16384 for General Purpose SSD (@gp2@ ), 4-16384 for Provisioned IOPS SSD (@io1@ ), 500-16384 for Throughput Optimized HDD (@st1@ ), 500-16384 for Cold HDD (@sc1@ ), and 1-1024 for Magnetic (@standard@ ) volumes. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
ebdVolumeSize :: Lens' EBSBlockDevice (Maybe Int)
ebdVolumeSize = lens _ebdVolumeSize (\ s a -> s{_ebdVolumeSize = a})

-- | The number of I/O operations per second (IOPS) that the volume supports. For @io1@ , this represents the number of IOPS that are provisioned for the volume. For @gp2@ , this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about General Purpose SSD baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraint: Range is 100-20000 IOPS for @io1@ volumes and 100-10000 IOPS for @gp2@ volumes. Condition: This parameter is required for requests to create @io1@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
ebdIOPS :: Lens' EBSBlockDevice (Maybe Int)
ebdIOPS = lens _ebdIOPS (\ s a -> s{_ebdIOPS = a})

-- | Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value. This is because only blank volumes can be encrypted on creation.
ebdEncrypted :: Lens' EBSBlockDevice (Maybe Bool)
ebdEncrypted = lens _ebdEncrypted (\ s a -> s{_ebdEncrypted = a})

-- | Identifier (key ID, key alias, ID ARN, or alias ARN) for a user-managed CMK under which the EBS volume is encrypted. Note: This parameter is only supported on @BlockDeviceMapping@ objects called by <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> , <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet> , and <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances> .
ebdKMSKeyId :: Lens' EBSBlockDevice (Maybe Text)
ebdKMSKeyId = lens _ebdKMSKeyId (\ s a -> s{_ebdKMSKeyId = a})

-- | The volume type: @gp2@ , @io1@ , @st1@ , @sc1@ , or @standard@ . Default: @standard@
ebdVolumeType :: Lens' EBSBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\ s a -> s{_ebdVolumeType = a})

-- | The ID of the snapshot.
ebdSnapshotId :: Lens' EBSBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\ s a -> s{_ebdSnapshotId = a})

instance FromXML EBSBlockDevice where
        parseXML x
          = EBSBlockDevice' <$>
              (x .@? "deleteOnTermination") <*>
                (x .@? "volumeSize")
                <*> (x .@? "iops")
                <*> (x .@? "encrypted")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "volumeType")
                <*> (x .@? "snapshotId")

instance NFData EBSBlockDevice where

instance ToQuery EBSBlockDevice where
        toQuery EBSBlockDevice'{..}
          = mconcat
              ["DeleteOnTermination" =: _ebdDeleteOnTermination,
               "VolumeSize" =: _ebdVolumeSize, "Iops" =: _ebdIOPS,
               "Encrypted" =: _ebdEncrypted,
               "KmsKeyId" =: _ebdKMSKeyId,
               "VolumeType" =: _ebdVolumeType,
               "SnapshotId" =: _ebdSnapshotId]

-- | Describes a parameter used to set up an EBS volume in a block device mapping.
--
--
--
-- /See:/ 'ebsInstanceBlockDevice' smart constructor.
data EBSInstanceBlockDevice = EBSInstanceBlockDevice'
  { _eibdStatus              :: !(Maybe AttachmentStatus)
  , _eibdDeleteOnTermination :: !(Maybe Bool)
  , _eibdVolumeId            :: !(Maybe Text)
  , _eibdAttachTime          :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSInstanceBlockDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eibdStatus' - The attachment state.
--
-- * 'eibdDeleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- * 'eibdVolumeId' - The ID of the EBS volume.
--
-- * 'eibdAttachTime' - The time stamp when the attachment initiated.
ebsInstanceBlockDevice
    :: EBSInstanceBlockDevice
ebsInstanceBlockDevice =
  EBSInstanceBlockDevice'
    { _eibdStatus = Nothing
    , _eibdDeleteOnTermination = Nothing
    , _eibdVolumeId = Nothing
    , _eibdAttachTime = Nothing
    }


-- | The attachment state.
eibdStatus :: Lens' EBSInstanceBlockDevice (Maybe AttachmentStatus)
eibdStatus = lens _eibdStatus (\ s a -> s{_eibdStatus = a})

-- | Indicates whether the volume is deleted on instance termination.
eibdDeleteOnTermination :: Lens' EBSInstanceBlockDevice (Maybe Bool)
eibdDeleteOnTermination = lens _eibdDeleteOnTermination (\ s a -> s{_eibdDeleteOnTermination = a})

-- | The ID of the EBS volume.
eibdVolumeId :: Lens' EBSInstanceBlockDevice (Maybe Text)
eibdVolumeId = lens _eibdVolumeId (\ s a -> s{_eibdVolumeId = a})

-- | The time stamp when the attachment initiated.
eibdAttachTime :: Lens' EBSInstanceBlockDevice (Maybe UTCTime)
eibdAttachTime = lens _eibdAttachTime (\ s a -> s{_eibdAttachTime = a}) . mapping _Time

instance FromXML EBSInstanceBlockDevice where
        parseXML x
          = EBSInstanceBlockDevice' <$>
              (x .@? "status") <*> (x .@? "deleteOnTermination")
                <*> (x .@? "volumeId")
                <*> (x .@? "attachTime")

instance NFData EBSInstanceBlockDevice where

-- | Describes information used to set up an EBS volume specified in a block device mapping.
--
--
--
-- /See:/ 'ebsInstanceBlockDeviceSpecification' smart constructor.
data EBSInstanceBlockDeviceSpecification = EBSInstanceBlockDeviceSpecification'
  { _eibdsDeleteOnTermination :: !(Maybe Bool)
  , _eibdsVolumeId            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSInstanceBlockDeviceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eibdsDeleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- * 'eibdsVolumeId' - The ID of the EBS volume.
ebsInstanceBlockDeviceSpecification
    :: EBSInstanceBlockDeviceSpecification
ebsInstanceBlockDeviceSpecification =
  EBSInstanceBlockDeviceSpecification'
    {_eibdsDeleteOnTermination = Nothing, _eibdsVolumeId = Nothing}


-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination :: Lens' EBSInstanceBlockDeviceSpecification (Maybe Bool)
eibdsDeleteOnTermination = lens _eibdsDeleteOnTermination (\ s a -> s{_eibdsDeleteOnTermination = a})

-- | The ID of the EBS volume.
eibdsVolumeId :: Lens' EBSInstanceBlockDeviceSpecification (Maybe Text)
eibdsVolumeId = lens _eibdsVolumeId (\ s a -> s{_eibdsVolumeId = a})

instance NFData EBSInstanceBlockDeviceSpecification
         where

instance ToQuery EBSInstanceBlockDeviceSpecification
         where
        toQuery EBSInstanceBlockDeviceSpecification'{..}
          = mconcat
              ["DeleteOnTermination" =: _eibdsDeleteOnTermination,
               "VolumeId" =: _eibdsVolumeId]

-- | Describes an egress-only Internet gateway.
--
--
--
-- /See:/ 'egressOnlyInternetGateway' smart constructor.
data EgressOnlyInternetGateway = EgressOnlyInternetGateway'
  { _eoigEgressOnlyInternetGatewayId :: !(Maybe Text)
  , _eoigAttachments                 :: !(Maybe [InternetGatewayAttachment])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoigEgressOnlyInternetGatewayId' - The ID of the egress-only Internet gateway.
--
-- * 'eoigAttachments' - Information about the attachment of the egress-only Internet gateway.
egressOnlyInternetGateway
    :: EgressOnlyInternetGateway
egressOnlyInternetGateway =
  EgressOnlyInternetGateway'
    {_eoigEgressOnlyInternetGatewayId = Nothing, _eoigAttachments = Nothing}


-- | The ID of the egress-only Internet gateway.
eoigEgressOnlyInternetGatewayId :: Lens' EgressOnlyInternetGateway (Maybe Text)
eoigEgressOnlyInternetGatewayId = lens _eoigEgressOnlyInternetGatewayId (\ s a -> s{_eoigEgressOnlyInternetGatewayId = a})

-- | Information about the attachment of the egress-only Internet gateway.
eoigAttachments :: Lens' EgressOnlyInternetGateway [InternetGatewayAttachment]
eoigAttachments = lens _eoigAttachments (\ s a -> s{_eoigAttachments = a}) . _Default . _Coerce

instance FromXML EgressOnlyInternetGateway where
        parseXML x
          = EgressOnlyInternetGateway' <$>
              (x .@? "egressOnlyInternetGatewayId") <*>
                (x .@? "attachmentSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData EgressOnlyInternetGateway where

-- | Describes the association between an instance and an Elastic GPU.
--
--
--
-- /See:/ 'elasticGpuAssociation' smart constructor.
data ElasticGpuAssociation = ElasticGpuAssociation'
  { _egaElasticGpuId               :: !(Maybe Text)
  , _egaElasticGpuAssociationId    :: !(Maybe Text)
  , _egaElasticGpuAssociationTime  :: !(Maybe Text)
  , _egaElasticGpuAssociationState :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticGpuAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'egaElasticGpuId' - The ID of the Elastic GPU.
--
-- * 'egaElasticGpuAssociationId' - The ID of the association.
--
-- * 'egaElasticGpuAssociationTime' - The time the Elastic GPU was associated with the instance.
--
-- * 'egaElasticGpuAssociationState' - The state of the association between the instance and the Elastic GPU.
elasticGpuAssociation
    :: ElasticGpuAssociation
elasticGpuAssociation =
  ElasticGpuAssociation'
    { _egaElasticGpuId = Nothing
    , _egaElasticGpuAssociationId = Nothing
    , _egaElasticGpuAssociationTime = Nothing
    , _egaElasticGpuAssociationState = Nothing
    }


-- | The ID of the Elastic GPU.
egaElasticGpuId :: Lens' ElasticGpuAssociation (Maybe Text)
egaElasticGpuId = lens _egaElasticGpuId (\ s a -> s{_egaElasticGpuId = a})

-- | The ID of the association.
egaElasticGpuAssociationId :: Lens' ElasticGpuAssociation (Maybe Text)
egaElasticGpuAssociationId = lens _egaElasticGpuAssociationId (\ s a -> s{_egaElasticGpuAssociationId = a})

-- | The time the Elastic GPU was associated with the instance.
egaElasticGpuAssociationTime :: Lens' ElasticGpuAssociation (Maybe Text)
egaElasticGpuAssociationTime = lens _egaElasticGpuAssociationTime (\ s a -> s{_egaElasticGpuAssociationTime = a})

-- | The state of the association between the instance and the Elastic GPU.
egaElasticGpuAssociationState :: Lens' ElasticGpuAssociation (Maybe Text)
egaElasticGpuAssociationState = lens _egaElasticGpuAssociationState (\ s a -> s{_egaElasticGpuAssociationState = a})

instance FromXML ElasticGpuAssociation where
        parseXML x
          = ElasticGpuAssociation' <$>
              (x .@? "elasticGpuId") <*>
                (x .@? "elasticGpuAssociationId")
                <*> (x .@? "elasticGpuAssociationTime")
                <*> (x .@? "elasticGpuAssociationState")

instance NFData ElasticGpuAssociation where

-- | Describes the status of an Elastic GPU.
--
--
--
-- /See:/ 'elasticGpuHealth' smart constructor.
newtype ElasticGpuHealth = ElasticGpuHealth'
  { _eghStatus :: Maybe ElasticGpuStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticGpuHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eghStatus' - The health status.
elasticGpuHealth
    :: ElasticGpuHealth
elasticGpuHealth = ElasticGpuHealth' {_eghStatus = Nothing}


-- | The health status.
eghStatus :: Lens' ElasticGpuHealth (Maybe ElasticGpuStatus)
eghStatus = lens _eghStatus (\ s a -> s{_eghStatus = a})

instance FromXML ElasticGpuHealth where
        parseXML x = ElasticGpuHealth' <$> (x .@? "status")

instance NFData ElasticGpuHealth where

-- | A specification for an Elastic GPU.
--
--
--
-- /See:/ 'elasticGpuSpecification' smart constructor.
newtype ElasticGpuSpecification = ElasticGpuSpecification'
  { _egsType :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticGpuSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'egsType' - The type of Elastic GPU.
elasticGpuSpecification
    :: Text -- ^ 'egsType'
    -> ElasticGpuSpecification
elasticGpuSpecification pType_ = ElasticGpuSpecification' {_egsType = pType_}


-- | The type of Elastic GPU.
egsType :: Lens' ElasticGpuSpecification Text
egsType = lens _egsType (\ s a -> s{_egsType = a})

instance NFData ElasticGpuSpecification where

instance ToQuery ElasticGpuSpecification where
        toQuery ElasticGpuSpecification'{..}
          = mconcat ["Type" =: _egsType]

-- | Describes an elastic GPU.
--
--
--
-- /See:/ 'elasticGpuSpecificationResponse' smart constructor.
newtype ElasticGpuSpecificationResponse = ElasticGpuSpecificationResponse'
  { _eType :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticGpuSpecificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eType' - The elastic GPU type.
elasticGpuSpecificationResponse
    :: ElasticGpuSpecificationResponse
elasticGpuSpecificationResponse =
  ElasticGpuSpecificationResponse' {_eType = Nothing}


-- | The elastic GPU type.
eType :: Lens' ElasticGpuSpecificationResponse (Maybe Text)
eType = lens _eType (\ s a -> s{_eType = a})

instance FromXML ElasticGpuSpecificationResponse
         where
        parseXML x
          = ElasticGpuSpecificationResponse' <$> (x .@? "type")

instance NFData ElasticGpuSpecificationResponse where

-- | Describes an Elastic GPU.
--
--
--
-- /See:/ 'elasticGpus' smart constructor.
data ElasticGpus = ElasticGpus'
  { _egInstanceId       :: !(Maybe Text)
  , _egElasticGpuType   :: !(Maybe Text)
  , _egElasticGpuId     :: !(Maybe Text)
  , _egElasticGpuState  :: !(Maybe ElasticGpuState)
  , _egElasticGpuHealth :: !(Maybe ElasticGpuHealth)
  , _egAvailabilityZone :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticGpus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'egInstanceId' - The ID of the instance to which the Elastic GPU is attached.
--
-- * 'egElasticGpuType' - The type of Elastic GPU.
--
-- * 'egElasticGpuId' - The ID of the Elastic GPU.
--
-- * 'egElasticGpuState' - The state of the Elastic GPU.
--
-- * 'egElasticGpuHealth' - The status of the Elastic GPU.
--
-- * 'egAvailabilityZone' - The Availability Zone in the which the Elastic GPU resides.
elasticGpus
    :: ElasticGpus
elasticGpus =
  ElasticGpus'
    { _egInstanceId = Nothing
    , _egElasticGpuType = Nothing
    , _egElasticGpuId = Nothing
    , _egElasticGpuState = Nothing
    , _egElasticGpuHealth = Nothing
    , _egAvailabilityZone = Nothing
    }


-- | The ID of the instance to which the Elastic GPU is attached.
egInstanceId :: Lens' ElasticGpus (Maybe Text)
egInstanceId = lens _egInstanceId (\ s a -> s{_egInstanceId = a})

-- | The type of Elastic GPU.
egElasticGpuType :: Lens' ElasticGpus (Maybe Text)
egElasticGpuType = lens _egElasticGpuType (\ s a -> s{_egElasticGpuType = a})

-- | The ID of the Elastic GPU.
egElasticGpuId :: Lens' ElasticGpus (Maybe Text)
egElasticGpuId = lens _egElasticGpuId (\ s a -> s{_egElasticGpuId = a})

-- | The state of the Elastic GPU.
egElasticGpuState :: Lens' ElasticGpus (Maybe ElasticGpuState)
egElasticGpuState = lens _egElasticGpuState (\ s a -> s{_egElasticGpuState = a})

-- | The status of the Elastic GPU.
egElasticGpuHealth :: Lens' ElasticGpus (Maybe ElasticGpuHealth)
egElasticGpuHealth = lens _egElasticGpuHealth (\ s a -> s{_egElasticGpuHealth = a})

-- | The Availability Zone in the which the Elastic GPU resides.
egAvailabilityZone :: Lens' ElasticGpus (Maybe Text)
egAvailabilityZone = lens _egAvailabilityZone (\ s a -> s{_egAvailabilityZone = a})

instance FromXML ElasticGpus where
        parseXML x
          = ElasticGpus' <$>
              (x .@? "instanceId") <*> (x .@? "elasticGpuType") <*>
                (x .@? "elasticGpuId")
                <*> (x .@? "elasticGpuState")
                <*> (x .@? "elasticGpuHealth")
                <*> (x .@? "availabilityZone")

instance NFData ElasticGpus where

-- | Describes a Spot Fleet event.
--
--
--
-- /See:/ 'eventInformation' smart constructor.
data EventInformation = EventInformation'
  { _eiInstanceId       :: !(Maybe Text)
  , _eiEventDescription :: !(Maybe Text)
  , _eiEventSubType     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiInstanceId' - The ID of the instance. This information is available only for @instanceChange@ events.
--
-- * 'eiEventDescription' - The description of the event.
--
-- * 'eiEventSubType' - The event. The following are the @error@ events:     * @iamFleetRoleInvalid@ - The Spot Fleet did not have the required permissions either to launch or terminate an instance.     * @launchSpecTemporarilyBlacklisted@ - The configuration is not valid and several attempts to launch instances have failed. For more information, see the description of the event.     * @spotFleetRequestConfigurationInvalid@ - The configuration is not valid. For more information, see the description of the event.     * @spotInstanceCountLimitExceeded@ - You've reached the limit on the number of Spot Instances that you can launch. The following are the @fleetRequestChange@ events:     * @active@ - The Spot Fleet has been validated and Amazon EC2 is attempting to maintain the target number of running Spot Instances.     * @cancelled@ - The Spot Fleet is canceled and has no running Spot Instances. The Spot Fleet will be deleted two days after its instances were terminated.     * @cancelled_running@ - The Spot Fleet is canceled and does not launch additional Spot Instances. Existing Spot Instances continue to run until they are interrupted or terminated.     * @cancelled_terminating@ - The Spot Fleet is canceled and its Spot Instances are terminating.     * @expired@ - The Spot Fleet request has expired. A subsequent event indicates that the instances were terminated, if the request was created with @TerminateInstancesWithExpiration@ set.     * @modify_in_progress@ - A request to modify the Spot Fleet request was accepted and is in progress.     * @modify_successful@ - The Spot Fleet request was modified.     * @price_update@ - The price for a launch configuration was adjusted because it was too high. This change is permanent.     * @submitted@ - The Spot Fleet request is being evaluated and Amazon EC2 is preparing to launch the target number of Spot Instances. The following are the @instanceChange@ events:     * @launched@ - A request was fulfilled and a new instance was launched.     * @terminated@ - An instance was terminated by the user. The following are the @Information@ events:     * @launchSpecUnusable@ - The price in a launch specification is not valid because it is below the Spot price or the Spot price is above the On-Demand price.     * @fleetProgressHalted@ - The price in every launch specification is not valid. A launch specification might become valid if the Spot price changes.
eventInformation
    :: EventInformation
eventInformation =
  EventInformation'
    { _eiInstanceId = Nothing
    , _eiEventDescription = Nothing
    , _eiEventSubType = Nothing
    }


-- | The ID of the instance. This information is available only for @instanceChange@ events.
eiInstanceId :: Lens' EventInformation (Maybe Text)
eiInstanceId = lens _eiInstanceId (\ s a -> s{_eiInstanceId = a})

-- | The description of the event.
eiEventDescription :: Lens' EventInformation (Maybe Text)
eiEventDescription = lens _eiEventDescription (\ s a -> s{_eiEventDescription = a})

-- | The event. The following are the @error@ events:     * @iamFleetRoleInvalid@ - The Spot Fleet did not have the required permissions either to launch or terminate an instance.     * @launchSpecTemporarilyBlacklisted@ - The configuration is not valid and several attempts to launch instances have failed. For more information, see the description of the event.     * @spotFleetRequestConfigurationInvalid@ - The configuration is not valid. For more information, see the description of the event.     * @spotInstanceCountLimitExceeded@ - You've reached the limit on the number of Spot Instances that you can launch. The following are the @fleetRequestChange@ events:     * @active@ - The Spot Fleet has been validated and Amazon EC2 is attempting to maintain the target number of running Spot Instances.     * @cancelled@ - The Spot Fleet is canceled and has no running Spot Instances. The Spot Fleet will be deleted two days after its instances were terminated.     * @cancelled_running@ - The Spot Fleet is canceled and does not launch additional Spot Instances. Existing Spot Instances continue to run until they are interrupted or terminated.     * @cancelled_terminating@ - The Spot Fleet is canceled and its Spot Instances are terminating.     * @expired@ - The Spot Fleet request has expired. A subsequent event indicates that the instances were terminated, if the request was created with @TerminateInstancesWithExpiration@ set.     * @modify_in_progress@ - A request to modify the Spot Fleet request was accepted and is in progress.     * @modify_successful@ - The Spot Fleet request was modified.     * @price_update@ - The price for a launch configuration was adjusted because it was too high. This change is permanent.     * @submitted@ - The Spot Fleet request is being evaluated and Amazon EC2 is preparing to launch the target number of Spot Instances. The following are the @instanceChange@ events:     * @launched@ - A request was fulfilled and a new instance was launched.     * @terminated@ - An instance was terminated by the user. The following are the @Information@ events:     * @launchSpecUnusable@ - The price in a launch specification is not valid because it is below the Spot price or the Spot price is above the On-Demand price.     * @fleetProgressHalted@ - The price in every launch specification is not valid. A launch specification might become valid if the Spot price changes.
eiEventSubType :: Lens' EventInformation (Maybe Text)
eiEventSubType = lens _eiEventSubType (\ s a -> s{_eiEventSubType = a})

instance FromXML EventInformation where
        parseXML x
          = EventInformation' <$>
              (x .@? "instanceId") <*> (x .@? "eventDescription")
                <*> (x .@? "eventSubType")

instance NFData EventInformation where

-- | Describes an instance export task.
--
--
--
-- /See:/ 'exportTask' smart constructor.
data ExportTask = ExportTask'
  { _etDescription           :: !Text
  , _etExportTaskId          :: !Text
  , _etExportToS3Task        :: !ExportToS3Task
  , _etInstanceExportDetails :: !InstanceExportDetails
  , _etState                 :: !ExportTaskState
  , _etStatusMessage         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etDescription' - A description of the resource being exported.
--
-- * 'etExportTaskId' - The ID of the export task.
--
-- * 'etExportToS3Task' - Information about the export task.
--
-- * 'etInstanceExportDetails' - Information about the instance to export.
--
-- * 'etState' - The state of the export task.
--
-- * 'etStatusMessage' - The status message related to the export task.
exportTask
    :: Text -- ^ 'etDescription'
    -> Text -- ^ 'etExportTaskId'
    -> ExportToS3Task -- ^ 'etExportToS3Task'
    -> InstanceExportDetails -- ^ 'etInstanceExportDetails'
    -> ExportTaskState -- ^ 'etState'
    -> Text -- ^ 'etStatusMessage'
    -> ExportTask
exportTask pDescription_ pExportTaskId_ pExportToS3Task_ pInstanceExportDetails_ pState_ pStatusMessage_ =
  ExportTask'
    { _etDescription = pDescription_
    , _etExportTaskId = pExportTaskId_
    , _etExportToS3Task = pExportToS3Task_
    , _etInstanceExportDetails = pInstanceExportDetails_
    , _etState = pState_
    , _etStatusMessage = pStatusMessage_
    }


-- | A description of the resource being exported.
etDescription :: Lens' ExportTask Text
etDescription = lens _etDescription (\ s a -> s{_etDescription = a})

-- | The ID of the export task.
etExportTaskId :: Lens' ExportTask Text
etExportTaskId = lens _etExportTaskId (\ s a -> s{_etExportTaskId = a})

-- | Information about the export task.
etExportToS3Task :: Lens' ExportTask ExportToS3Task
etExportToS3Task = lens _etExportToS3Task (\ s a -> s{_etExportToS3Task = a})

-- | Information about the instance to export.
etInstanceExportDetails :: Lens' ExportTask InstanceExportDetails
etInstanceExportDetails = lens _etInstanceExportDetails (\ s a -> s{_etInstanceExportDetails = a})

-- | The state of the export task.
etState :: Lens' ExportTask ExportTaskState
etState = lens _etState (\ s a -> s{_etState = a})

-- | The status message related to the export task.
etStatusMessage :: Lens' ExportTask Text
etStatusMessage = lens _etStatusMessage (\ s a -> s{_etStatusMessage = a})

instance FromXML ExportTask where
        parseXML x
          = ExportTask' <$>
              (x .@ "description") <*> (x .@ "exportTaskId") <*>
                (x .@ "exportToS3")
                <*> (x .@ "instanceExport")
                <*> (x .@ "state")
                <*> (x .@ "statusMessage")

instance NFData ExportTask where

-- | Describes the format and location for an instance export task.
--
--
--
-- /See:/ 'exportToS3Task' smart constructor.
data ExportToS3Task = ExportToS3Task'
  { _etstS3Key           :: !(Maybe Text)
  , _etstContainerFormat :: !(Maybe ContainerFormat)
  , _etstS3Bucket        :: !(Maybe Text)
  , _etstDiskImageFormat :: !(Maybe DiskImageFormat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportToS3Task' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etstS3Key' - The encryption key for your S3 bucket.
--
-- * 'etstContainerFormat' - The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
--
-- * 'etstS3Bucket' - The S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
--
-- * 'etstDiskImageFormat' - The format for the exported image.
exportToS3Task
    :: ExportToS3Task
exportToS3Task =
  ExportToS3Task'
    { _etstS3Key = Nothing
    , _etstContainerFormat = Nothing
    , _etstS3Bucket = Nothing
    , _etstDiskImageFormat = Nothing
    }


-- | The encryption key for your S3 bucket.
etstS3Key :: Lens' ExportToS3Task (Maybe Text)
etstS3Key = lens _etstS3Key (\ s a -> s{_etstS3Key = a})

-- | The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
etstContainerFormat :: Lens' ExportToS3Task (Maybe ContainerFormat)
etstContainerFormat = lens _etstContainerFormat (\ s a -> s{_etstContainerFormat = a})

-- | The S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
etstS3Bucket :: Lens' ExportToS3Task (Maybe Text)
etstS3Bucket = lens _etstS3Bucket (\ s a -> s{_etstS3Bucket = a})

-- | The format for the exported image.
etstDiskImageFormat :: Lens' ExportToS3Task (Maybe DiskImageFormat)
etstDiskImageFormat = lens _etstDiskImageFormat (\ s a -> s{_etstDiskImageFormat = a})

instance FromXML ExportToS3Task where
        parseXML x
          = ExportToS3Task' <$>
              (x .@? "s3Key") <*> (x .@? "containerFormat") <*>
                (x .@? "s3Bucket")
                <*> (x .@? "diskImageFormat")

instance NFData ExportToS3Task where

-- | Describes an instance export task.
--
--
--
-- /See:/ 'exportToS3TaskSpecification' smart constructor.
data ExportToS3TaskSpecification = ExportToS3TaskSpecification'
  { _etstsContainerFormat :: !(Maybe ContainerFormat)
  , _etstsS3Prefix        :: !(Maybe Text)
  , _etstsS3Bucket        :: !(Maybe Text)
  , _etstsDiskImageFormat :: !(Maybe DiskImageFormat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportToS3TaskSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etstsContainerFormat' - The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
--
-- * 'etstsS3Prefix' - The image is written to a single object in the S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
--
-- * 'etstsS3Bucket' - The S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
--
-- * 'etstsDiskImageFormat' - The format for the exported image.
exportToS3TaskSpecification
    :: ExportToS3TaskSpecification
exportToS3TaskSpecification =
  ExportToS3TaskSpecification'
    { _etstsContainerFormat = Nothing
    , _etstsS3Prefix = Nothing
    , _etstsS3Bucket = Nothing
    , _etstsDiskImageFormat = Nothing
    }


-- | The container format used to combine disk images with metadata (such as OVF). If absent, only the disk image is exported.
etstsContainerFormat :: Lens' ExportToS3TaskSpecification (Maybe ContainerFormat)
etstsContainerFormat = lens _etstsContainerFormat (\ s a -> s{_etstsContainerFormat = a})

-- | The image is written to a single object in the S3 bucket at the S3 key s3prefix + exportTaskId + '.' + diskImageFormat.
etstsS3Prefix :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Prefix = lens _etstsS3Prefix (\ s a -> s{_etstsS3Prefix = a})

-- | The S3 bucket for the destination image. The destination bucket must exist and grant WRITE and READ_ACP permissions to the AWS account @vm-import-export@amazon.com@ .
etstsS3Bucket :: Lens' ExportToS3TaskSpecification (Maybe Text)
etstsS3Bucket = lens _etstsS3Bucket (\ s a -> s{_etstsS3Bucket = a})

-- | The format for the exported image.
etstsDiskImageFormat :: Lens' ExportToS3TaskSpecification (Maybe DiskImageFormat)
etstsDiskImageFormat = lens _etstsDiskImageFormat (\ s a -> s{_etstsDiskImageFormat = a})

instance NFData ExportToS3TaskSpecification where

instance ToQuery ExportToS3TaskSpecification where
        toQuery ExportToS3TaskSpecification'{..}
          = mconcat
              ["ContainerFormat" =: _etstsContainerFormat,
               "S3Prefix" =: _etstsS3Prefix,
               "S3Bucket" =: _etstsS3Bucket,
               "DiskImageFormat" =: _etstsDiskImageFormat]

-- | A filter name and value pair that is used to return a more specific list of results from a describe operation. Filters can be used to match a set of resources by specific criteria, such as tags, attributes, or IDs. The filters supported by a describe operation are documented with the describe operation. For example:
--
--
--     * 'DescribeAvailabilityZones'
--
--     * 'DescribeImages'
--
--     * 'DescribeInstances'
--
--     * 'DescribeKeyPairs'
--
--     * 'DescribeSecurityGroups'
--
--     * 'DescribeSnapshots'
--
--     * 'DescribeSubnets'
--
--     * 'DescribeTags'
--
--     * 'DescribeVolumes'
--
--     * 'DescribeVpcs'
--
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fValues :: !(Maybe [Text])
  , _fName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fValues' - One or more filter values. Filter values are case-sensitive.
--
-- * 'fName' - The name of the filter. Filter names are case-sensitive.
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_ = Filter' {_fValues = Nothing, _fName = pName_}


-- | One or more filter values. Filter values are case-sensitive.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Default . _Coerce

-- | The name of the filter. Filter names are case-sensitive.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a})

instance NFData Filter where

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              [toQuery (toQueryList "Value" <$> _fValues),
               "Name" =: _fName]

-- | Describes an EC2 Fleet.
--
--
--
-- /See:/ 'fleetData' smart constructor.
data FleetData = FleetData'
  { _fdClientToken :: !(Maybe Text)
  , _fdTargetCapacitySpecification :: !(Maybe TargetCapacitySpecification)
  , _fdSpotOptions :: !(Maybe SpotOptions)
  , _fdExcessCapacityTerminationPolicy :: !(Maybe FleetExcessCapacityTerminationPolicy)
  , _fdFleetState :: !(Maybe FleetStateCode)
  , _fdLaunchTemplateConfigs :: !(Maybe [FleetLaunchTemplateConfig])
  , _fdValidUntil :: !(Maybe ISO8601)
  , _fdTerminateInstancesWithExpiration :: !(Maybe Bool)
  , _fdFulfilledCapacity :: !(Maybe Double)
  , _fdType :: !(Maybe FleetType)
  , _fdValidFrom :: !(Maybe ISO8601)
  , _fdReplaceUnhealthyInstances :: !(Maybe Bool)
  , _fdFulfilledOnDemandCapacity :: !(Maybe Double)
  , _fdFleetId :: !(Maybe Text)
  , _fdCreateTime :: !(Maybe ISO8601)
  , _fdTags :: !(Maybe [Tag])
  , _fdActivityStatus :: !(Maybe FleetActivityStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Maximum 64 ASCII characters
--
-- * 'fdTargetCapacitySpecification' - The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- * 'fdSpotOptions' - The configuration of Spot Instances in an EC2 Fleet.
--
-- * 'fdExcessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- * 'fdFleetState' - The state of the EC2 Fleet.
--
-- * 'fdLaunchTemplateConfigs' - The launch template and overrides.
--
-- * 'fdValidUntil' - The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
--
-- * 'fdTerminateInstancesWithExpiration' - Indicates whether running instances should be terminated when the EC2 Fleet expires.
--
-- * 'fdFulfilledCapacity' - The number of units fulfilled by this request compared to the set target capacity.
--
-- * 'fdType' - The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests; it does not attempt to replenish instances if capacity is diminished, and does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
--
-- * 'fdValidFrom' - The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
--
-- * 'fdReplaceUnhealthyInstances' - Indicates whether EC2 Fleet should replace unhealthy instances.
--
-- * 'fdFulfilledOnDemandCapacity' - The number of units fulfilled by this request compared to the set target On-Demand capacity.
--
-- * 'fdFleetId' - The ID of the EC2 Fleet.
--
-- * 'fdCreateTime' - The creation date and time of the EC2 Fleet.
--
-- * 'fdTags' - The tags for an EC2 Fleet resource.
--
-- * 'fdActivityStatus' - The progress of the EC2 Fleet. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the EC2 Fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the EC2 Fleet is decreased, the status is @pending_termination@ while instances are terminating.
fleetData
    :: FleetData
fleetData =
  FleetData'
    { _fdClientToken = Nothing
    , _fdTargetCapacitySpecification = Nothing
    , _fdSpotOptions = Nothing
    , _fdExcessCapacityTerminationPolicy = Nothing
    , _fdFleetState = Nothing
    , _fdLaunchTemplateConfigs = Nothing
    , _fdValidUntil = Nothing
    , _fdTerminateInstancesWithExpiration = Nothing
    , _fdFulfilledCapacity = Nothing
    , _fdType = Nothing
    , _fdValidFrom = Nothing
    , _fdReplaceUnhealthyInstances = Nothing
    , _fdFulfilledOnDemandCapacity = Nothing
    , _fdFleetId = Nothing
    , _fdCreateTime = Nothing
    , _fdTags = Nothing
    , _fdActivityStatus = Nothing
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Maximum 64 ASCII characters
fdClientToken :: Lens' FleetData (Maybe Text)
fdClientToken = lens _fdClientToken (\ s a -> s{_fdClientToken = a})

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
fdTargetCapacitySpecification :: Lens' FleetData (Maybe TargetCapacitySpecification)
fdTargetCapacitySpecification = lens _fdTargetCapacitySpecification (\ s a -> s{_fdTargetCapacitySpecification = a})

-- | The configuration of Spot Instances in an EC2 Fleet.
fdSpotOptions :: Lens' FleetData (Maybe SpotOptions)
fdSpotOptions = lens _fdSpotOptions (\ s a -> s{_fdSpotOptions = a})

-- | Indicates whether running instances should be terminated if the target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
fdExcessCapacityTerminationPolicy :: Lens' FleetData (Maybe FleetExcessCapacityTerminationPolicy)
fdExcessCapacityTerminationPolicy = lens _fdExcessCapacityTerminationPolicy (\ s a -> s{_fdExcessCapacityTerminationPolicy = a})

-- | The state of the EC2 Fleet.
fdFleetState :: Lens' FleetData (Maybe FleetStateCode)
fdFleetState = lens _fdFleetState (\ s a -> s{_fdFleetState = a})

-- | The launch template and overrides.
fdLaunchTemplateConfigs :: Lens' FleetData [FleetLaunchTemplateConfig]
fdLaunchTemplateConfigs = lens _fdLaunchTemplateConfigs (\ s a -> s{_fdLaunchTemplateConfigs = a}) . _Default . _Coerce

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
fdValidUntil :: Lens' FleetData (Maybe UTCTime)
fdValidUntil = lens _fdValidUntil (\ s a -> s{_fdValidUntil = a}) . mapping _Time

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
fdTerminateInstancesWithExpiration :: Lens' FleetData (Maybe Bool)
fdTerminateInstancesWithExpiration = lens _fdTerminateInstancesWithExpiration (\ s a -> s{_fdTerminateInstancesWithExpiration = a})

-- | The number of units fulfilled by this request compared to the set target capacity.
fdFulfilledCapacity :: Lens' FleetData (Maybe Double)
fdFulfilledCapacity = lens _fdFulfilledCapacity (\ s a -> s{_fdFulfilledCapacity = a})

-- | The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests; it does not attempt to replenish instances if capacity is diminished, and does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
fdType :: Lens' FleetData (Maybe FleetType)
fdType = lens _fdType (\ s a -> s{_fdType = a})

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
fdValidFrom :: Lens' FleetData (Maybe UTCTime)
fdValidFrom = lens _fdValidFrom (\ s a -> s{_fdValidFrom = a}) . mapping _Time

-- | Indicates whether EC2 Fleet should replace unhealthy instances.
fdReplaceUnhealthyInstances :: Lens' FleetData (Maybe Bool)
fdReplaceUnhealthyInstances = lens _fdReplaceUnhealthyInstances (\ s a -> s{_fdReplaceUnhealthyInstances = a})

-- | The number of units fulfilled by this request compared to the set target On-Demand capacity.
fdFulfilledOnDemandCapacity :: Lens' FleetData (Maybe Double)
fdFulfilledOnDemandCapacity = lens _fdFulfilledOnDemandCapacity (\ s a -> s{_fdFulfilledOnDemandCapacity = a})

-- | The ID of the EC2 Fleet.
fdFleetId :: Lens' FleetData (Maybe Text)
fdFleetId = lens _fdFleetId (\ s a -> s{_fdFleetId = a})

-- | The creation date and time of the EC2 Fleet.
fdCreateTime :: Lens' FleetData (Maybe UTCTime)
fdCreateTime = lens _fdCreateTime (\ s a -> s{_fdCreateTime = a}) . mapping _Time

-- | The tags for an EC2 Fleet resource.
fdTags :: Lens' FleetData [Tag]
fdTags = lens _fdTags (\ s a -> s{_fdTags = a}) . _Default . _Coerce

-- | The progress of the EC2 Fleet. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the EC2 Fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the EC2 Fleet is decreased, the status is @pending_termination@ while instances are terminating.
fdActivityStatus :: Lens' FleetData (Maybe FleetActivityStatus)
fdActivityStatus = lens _fdActivityStatus (\ s a -> s{_fdActivityStatus = a})

instance FromXML FleetData where
        parseXML x
          = FleetData' <$>
              (x .@? "clientToken") <*>
                (x .@? "targetCapacitySpecification")
                <*> (x .@? "spotOptions")
                <*> (x .@? "excessCapacityTerminationPolicy")
                <*> (x .@? "fleetState")
                <*>
                (x .@? "launchTemplateConfigs" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "validUntil")
                <*> (x .@? "terminateInstancesWithExpiration")
                <*> (x .@? "fulfilledCapacity")
                <*> (x .@? "type")
                <*> (x .@? "validFrom")
                <*> (x .@? "replaceUnhealthyInstances")
                <*> (x .@? "fulfilledOnDemandCapacity")
                <*> (x .@? "fleetId")
                <*> (x .@? "createTime")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "activityStatus")

instance NFData FleetData where

-- | Describes a launch template and overrides.
--
--
--
-- /See:/ 'fleetLaunchTemplateConfig' smart constructor.
data FleetLaunchTemplateConfig = FleetLaunchTemplateConfig'
  { _fltcOverrides :: !(Maybe [FleetLaunchTemplateOverrides])
  , _fltcLaunchTemplateSpecification :: !(Maybe FleetLaunchTemplateSpecification)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetLaunchTemplateConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltcOverrides' - Any parameters that you specify override the same parameters in the launch template.
--
-- * 'fltcLaunchTemplateSpecification' - The launch template.
fleetLaunchTemplateConfig
    :: FleetLaunchTemplateConfig
fleetLaunchTemplateConfig =
  FleetLaunchTemplateConfig'
    {_fltcOverrides = Nothing, _fltcLaunchTemplateSpecification = Nothing}


-- | Any parameters that you specify override the same parameters in the launch template.
fltcOverrides :: Lens' FleetLaunchTemplateConfig [FleetLaunchTemplateOverrides]
fltcOverrides = lens _fltcOverrides (\ s a -> s{_fltcOverrides = a}) . _Default . _Coerce

-- | The launch template.
fltcLaunchTemplateSpecification :: Lens' FleetLaunchTemplateConfig (Maybe FleetLaunchTemplateSpecification)
fltcLaunchTemplateSpecification = lens _fltcLaunchTemplateSpecification (\ s a -> s{_fltcLaunchTemplateSpecification = a})

instance FromXML FleetLaunchTemplateConfig where
        parseXML x
          = FleetLaunchTemplateConfig' <$>
              (x .@? "overrides" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "launchTemplateSpecification")

instance NFData FleetLaunchTemplateConfig where

-- | Describes a launch template and overrides.
--
--
--
-- /See:/ 'fleetLaunchTemplateConfigRequest' smart constructor.
data FleetLaunchTemplateConfigRequest = FleetLaunchTemplateConfigRequest'
  { _fltcrOverrides :: !(Maybe [FleetLaunchTemplateOverridesRequest])
  , _fltcrLaunchTemplateSpecification :: !(Maybe FleetLaunchTemplateSpecificationRequest)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetLaunchTemplateConfigRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltcrOverrides' - Any parameters that you specify override the same parameters in the launch template.
--
-- * 'fltcrLaunchTemplateSpecification' - The launch template to use. You must specify either the launch template ID or launch template name in the request.
fleetLaunchTemplateConfigRequest
    :: FleetLaunchTemplateConfigRequest
fleetLaunchTemplateConfigRequest =
  FleetLaunchTemplateConfigRequest'
    {_fltcrOverrides = Nothing, _fltcrLaunchTemplateSpecification = Nothing}


-- | Any parameters that you specify override the same parameters in the launch template.
fltcrOverrides :: Lens' FleetLaunchTemplateConfigRequest [FleetLaunchTemplateOverridesRequest]
fltcrOverrides = lens _fltcrOverrides (\ s a -> s{_fltcrOverrides = a}) . _Default . _Coerce

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request.
fltcrLaunchTemplateSpecification :: Lens' FleetLaunchTemplateConfigRequest (Maybe FleetLaunchTemplateSpecificationRequest)
fltcrLaunchTemplateSpecification = lens _fltcrLaunchTemplateSpecification (\ s a -> s{_fltcrLaunchTemplateSpecification = a})

instance NFData FleetLaunchTemplateConfigRequest
         where

instance ToQuery FleetLaunchTemplateConfigRequest
         where
        toQuery FleetLaunchTemplateConfigRequest'{..}
          = mconcat
              [toQuery
                 (toQueryList "Overrides" <$> _fltcrOverrides),
               "LaunchTemplateSpecification" =:
                 _fltcrLaunchTemplateSpecification]

-- | Describes overrides for a launch template.
--
--
--
-- /See:/ 'fleetLaunchTemplateOverrides' smart constructor.
data FleetLaunchTemplateOverrides = FleetLaunchTemplateOverrides'
  { _fltoWeightedCapacity :: !(Maybe Double)
  , _fltoSubnetId         :: !(Maybe Text)
  , _fltoInstanceType     :: !(Maybe InstanceType)
  , _fltoAvailabilityZone :: !(Maybe Text)
  , _fltoMaxPrice         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetLaunchTemplateOverrides' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltoWeightedCapacity' - The number of units provided by the specified instance type.
--
-- * 'fltoSubnetId' - The ID of the subnet in which to launch the instances.
--
-- * 'fltoInstanceType' - The instance type.
--
-- * 'fltoAvailabilityZone' - The Availability Zone in which to launch the instances.
--
-- * 'fltoMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance.
fleetLaunchTemplateOverrides
    :: FleetLaunchTemplateOverrides
fleetLaunchTemplateOverrides =
  FleetLaunchTemplateOverrides'
    { _fltoWeightedCapacity = Nothing
    , _fltoSubnetId = Nothing
    , _fltoInstanceType = Nothing
    , _fltoAvailabilityZone = Nothing
    , _fltoMaxPrice = Nothing
    }


-- | The number of units provided by the specified instance type.
fltoWeightedCapacity :: Lens' FleetLaunchTemplateOverrides (Maybe Double)
fltoWeightedCapacity = lens _fltoWeightedCapacity (\ s a -> s{_fltoWeightedCapacity = a})

-- | The ID of the subnet in which to launch the instances.
fltoSubnetId :: Lens' FleetLaunchTemplateOverrides (Maybe Text)
fltoSubnetId = lens _fltoSubnetId (\ s a -> s{_fltoSubnetId = a})

-- | The instance type.
fltoInstanceType :: Lens' FleetLaunchTemplateOverrides (Maybe InstanceType)
fltoInstanceType = lens _fltoInstanceType (\ s a -> s{_fltoInstanceType = a})

-- | The Availability Zone in which to launch the instances.
fltoAvailabilityZone :: Lens' FleetLaunchTemplateOverrides (Maybe Text)
fltoAvailabilityZone = lens _fltoAvailabilityZone (\ s a -> s{_fltoAvailabilityZone = a})

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
fltoMaxPrice :: Lens' FleetLaunchTemplateOverrides (Maybe Text)
fltoMaxPrice = lens _fltoMaxPrice (\ s a -> s{_fltoMaxPrice = a})

instance FromXML FleetLaunchTemplateOverrides where
        parseXML x
          = FleetLaunchTemplateOverrides' <$>
              (x .@? "weightedCapacity") <*> (x .@? "subnetId") <*>
                (x .@? "instanceType")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "maxPrice")

instance NFData FleetLaunchTemplateOverrides where

-- | Describes overrides for a launch template.
--
--
--
-- /See:/ 'fleetLaunchTemplateOverridesRequest' smart constructor.
data FleetLaunchTemplateOverridesRequest = FleetLaunchTemplateOverridesRequest'
  { _fltorWeightedCapacity :: !(Maybe Double)
  , _fltorSubnetId         :: !(Maybe Text)
  , _fltorInstanceType     :: !(Maybe InstanceType)
  , _fltorAvailabilityZone :: !(Maybe Text)
  , _fltorMaxPrice         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetLaunchTemplateOverridesRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltorWeightedCapacity' - The number of units provided by the specified instance type.
--
-- * 'fltorSubnetId' - The ID of the subnet in which to launch the instances.
--
-- * 'fltorInstanceType' - The instance type.
--
-- * 'fltorAvailabilityZone' - The Availability Zone in which to launch the instances.
--
-- * 'fltorMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance.
fleetLaunchTemplateOverridesRequest
    :: FleetLaunchTemplateOverridesRequest
fleetLaunchTemplateOverridesRequest =
  FleetLaunchTemplateOverridesRequest'
    { _fltorWeightedCapacity = Nothing
    , _fltorSubnetId = Nothing
    , _fltorInstanceType = Nothing
    , _fltorAvailabilityZone = Nothing
    , _fltorMaxPrice = Nothing
    }


-- | The number of units provided by the specified instance type.
fltorWeightedCapacity :: Lens' FleetLaunchTemplateOverridesRequest (Maybe Double)
fltorWeightedCapacity = lens _fltorWeightedCapacity (\ s a -> s{_fltorWeightedCapacity = a})

-- | The ID of the subnet in which to launch the instances.
fltorSubnetId :: Lens' FleetLaunchTemplateOverridesRequest (Maybe Text)
fltorSubnetId = lens _fltorSubnetId (\ s a -> s{_fltorSubnetId = a})

-- | The instance type.
fltorInstanceType :: Lens' FleetLaunchTemplateOverridesRequest (Maybe InstanceType)
fltorInstanceType = lens _fltorInstanceType (\ s a -> s{_fltorInstanceType = a})

-- | The Availability Zone in which to launch the instances.
fltorAvailabilityZone :: Lens' FleetLaunchTemplateOverridesRequest (Maybe Text)
fltorAvailabilityZone = lens _fltorAvailabilityZone (\ s a -> s{_fltorAvailabilityZone = a})

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
fltorMaxPrice :: Lens' FleetLaunchTemplateOverridesRequest (Maybe Text)
fltorMaxPrice = lens _fltorMaxPrice (\ s a -> s{_fltorMaxPrice = a})

instance NFData FleetLaunchTemplateOverridesRequest
         where

instance ToQuery FleetLaunchTemplateOverridesRequest
         where
        toQuery FleetLaunchTemplateOverridesRequest'{..}
          = mconcat
              ["WeightedCapacity" =: _fltorWeightedCapacity,
               "SubnetId" =: _fltorSubnetId,
               "InstanceType" =: _fltorInstanceType,
               "AvailabilityZone" =: _fltorAvailabilityZone,
               "MaxPrice" =: _fltorMaxPrice]

-- | Describes a launch template.
--
--
--
-- /See:/ 'fleetLaunchTemplateSpecification' smart constructor.
data FleetLaunchTemplateSpecification = FleetLaunchTemplateSpecification'
  { _fltsLaunchTemplateName :: !(Maybe Text)
  , _fltsLaunchTemplateId   :: !(Maybe Text)
  , _fltsVersion            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetLaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltsLaunchTemplateName' - The name of the launch template. You must specify either a template name or a template ID.
--
-- * 'fltsLaunchTemplateId' - The ID of the launch template. You must specify either a template ID or a template name.
--
-- * 'fltsVersion' - The version number. By default, the default version of the launch template is used.
fleetLaunchTemplateSpecification
    :: FleetLaunchTemplateSpecification
fleetLaunchTemplateSpecification =
  FleetLaunchTemplateSpecification'
    { _fltsLaunchTemplateName = Nothing
    , _fltsLaunchTemplateId = Nothing
    , _fltsVersion = Nothing
    }


-- | The name of the launch template. You must specify either a template name or a template ID.
fltsLaunchTemplateName :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsLaunchTemplateName = lens _fltsLaunchTemplateName (\ s a -> s{_fltsLaunchTemplateName = a})

-- | The ID of the launch template. You must specify either a template ID or a template name.
fltsLaunchTemplateId :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsLaunchTemplateId = lens _fltsLaunchTemplateId (\ s a -> s{_fltsLaunchTemplateId = a})

-- | The version number. By default, the default version of the launch template is used.
fltsVersion :: Lens' FleetLaunchTemplateSpecification (Maybe Text)
fltsVersion = lens _fltsVersion (\ s a -> s{_fltsVersion = a})

instance FromXML FleetLaunchTemplateSpecification
         where
        parseXML x
          = FleetLaunchTemplateSpecification' <$>
              (x .@? "launchTemplateName") <*>
                (x .@? "launchTemplateId")
                <*> (x .@? "version")

instance NFData FleetLaunchTemplateSpecification
         where

instance ToQuery FleetLaunchTemplateSpecification
         where
        toQuery FleetLaunchTemplateSpecification'{..}
          = mconcat
              ["LaunchTemplateName" =: _fltsLaunchTemplateName,
               "LaunchTemplateId" =: _fltsLaunchTemplateId,
               "Version" =: _fltsVersion]

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request.
--
--
--
-- /See:/ 'fleetLaunchTemplateSpecificationRequest' smart constructor.
data FleetLaunchTemplateSpecificationRequest = FleetLaunchTemplateSpecificationRequest'
  { _fltsrLaunchTemplateName :: !(Maybe Text)
  , _fltsrLaunchTemplateId   :: !(Maybe Text)
  , _fltsrVersion            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FleetLaunchTemplateSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fltsrLaunchTemplateName' - The name of the launch template.
--
-- * 'fltsrLaunchTemplateId' - The ID of the launch template.
--
-- * 'fltsrVersion' - The version number of the launch template.
fleetLaunchTemplateSpecificationRequest
    :: FleetLaunchTemplateSpecificationRequest
fleetLaunchTemplateSpecificationRequest =
  FleetLaunchTemplateSpecificationRequest'
    { _fltsrLaunchTemplateName = Nothing
    , _fltsrLaunchTemplateId = Nothing
    , _fltsrVersion = Nothing
    }


-- | The name of the launch template.
fltsrLaunchTemplateName :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrLaunchTemplateName = lens _fltsrLaunchTemplateName (\ s a -> s{_fltsrLaunchTemplateName = a})

-- | The ID of the launch template.
fltsrLaunchTemplateId :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrLaunchTemplateId = lens _fltsrLaunchTemplateId (\ s a -> s{_fltsrLaunchTemplateId = a})

-- | The version number of the launch template.
fltsrVersion :: Lens' FleetLaunchTemplateSpecificationRequest (Maybe Text)
fltsrVersion = lens _fltsrVersion (\ s a -> s{_fltsrVersion = a})

instance NFData
           FleetLaunchTemplateSpecificationRequest
         where

instance ToQuery
           FleetLaunchTemplateSpecificationRequest
         where
        toQuery FleetLaunchTemplateSpecificationRequest'{..}
          = mconcat
              ["LaunchTemplateName" =: _fltsrLaunchTemplateName,
               "LaunchTemplateId" =: _fltsrLaunchTemplateId,
               "Version" =: _fltsrVersion]

-- | Describes a flow log.
--
--
--
-- /See:/ 'flowLog' smart constructor.
data FlowLog = FlowLog'
  { _flCreationTime             :: !(Maybe ISO8601)
  , _flResourceId               :: !(Maybe Text)
  , _flFlowLogStatus            :: !(Maybe Text)
  , _flTrafficType              :: !(Maybe TrafficType)
  , _flDeliverLogsStatus        :: !(Maybe Text)
  , _flDeliverLogsErrorMessage  :: !(Maybe Text)
  , _flLogGroupName             :: !(Maybe Text)
  , _flDeliverLogsPermissionARN :: !(Maybe Text)
  , _flFlowLogId                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FlowLog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flCreationTime' - The date and time the flow log was created.
--
-- * 'flResourceId' - The ID of the resource on which the flow log was created.
--
-- * 'flFlowLogStatus' - The status of the flow log (@ACTIVE@ ).
--
-- * 'flTrafficType' - The type of traffic captured for the flow log.
--
-- * 'flDeliverLogsStatus' - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
--
-- * 'flDeliverLogsErrorMessage' - Information about the error that occurred. @Rate limited@ indicates that CloudWatch logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of CloudWatch Logs log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
--
-- * 'flLogGroupName' - The name of the flow log group.
--
-- * 'flDeliverLogsPermissionARN' - The ARN of the IAM role that posts logs to CloudWatch Logs.
--
-- * 'flFlowLogId' - The flow log ID.
flowLog
    :: FlowLog
flowLog =
  FlowLog'
    { _flCreationTime = Nothing
    , _flResourceId = Nothing
    , _flFlowLogStatus = Nothing
    , _flTrafficType = Nothing
    , _flDeliverLogsStatus = Nothing
    , _flDeliverLogsErrorMessage = Nothing
    , _flLogGroupName = Nothing
    , _flDeliverLogsPermissionARN = Nothing
    , _flFlowLogId = Nothing
    }


-- | The date and time the flow log was created.
flCreationTime :: Lens' FlowLog (Maybe UTCTime)
flCreationTime = lens _flCreationTime (\ s a -> s{_flCreationTime = a}) . mapping _Time

-- | The ID of the resource on which the flow log was created.
flResourceId :: Lens' FlowLog (Maybe Text)
flResourceId = lens _flResourceId (\ s a -> s{_flResourceId = a})

-- | The status of the flow log (@ACTIVE@ ).
flFlowLogStatus :: Lens' FlowLog (Maybe Text)
flFlowLogStatus = lens _flFlowLogStatus (\ s a -> s{_flFlowLogStatus = a})

-- | The type of traffic captured for the flow log.
flTrafficType :: Lens' FlowLog (Maybe TrafficType)
flTrafficType = lens _flTrafficType (\ s a -> s{_flTrafficType = a})

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
flDeliverLogsStatus :: Lens' FlowLog (Maybe Text)
flDeliverLogsStatus = lens _flDeliverLogsStatus (\ s a -> s{_flDeliverLogsStatus = a})

-- | Information about the error that occurred. @Rate limited@ indicates that CloudWatch logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of CloudWatch Logs log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
flDeliverLogsErrorMessage :: Lens' FlowLog (Maybe Text)
flDeliverLogsErrorMessage = lens _flDeliverLogsErrorMessage (\ s a -> s{_flDeliverLogsErrorMessage = a})

-- | The name of the flow log group.
flLogGroupName :: Lens' FlowLog (Maybe Text)
flLogGroupName = lens _flLogGroupName (\ s a -> s{_flLogGroupName = a})

-- | The ARN of the IAM role that posts logs to CloudWatch Logs.
flDeliverLogsPermissionARN :: Lens' FlowLog (Maybe Text)
flDeliverLogsPermissionARN = lens _flDeliverLogsPermissionARN (\ s a -> s{_flDeliverLogsPermissionARN = a})

-- | The flow log ID.
flFlowLogId :: Lens' FlowLog (Maybe Text)
flFlowLogId = lens _flFlowLogId (\ s a -> s{_flFlowLogId = a})

instance FromXML FlowLog where
        parseXML x
          = FlowLog' <$>
              (x .@? "creationTime") <*> (x .@? "resourceId") <*>
                (x .@? "flowLogStatus")
                <*> (x .@? "trafficType")
                <*> (x .@? "deliverLogsStatus")
                <*> (x .@? "deliverLogsErrorMessage")
                <*> (x .@? "logGroupName")
                <*> (x .@? "deliverLogsPermissionArn")
                <*> (x .@? "flowLogId")

instance NFData FlowLog where

-- | Describes an Amazon FPGA image (AFI).
--
--
--
-- /See:/ 'fpgaImage' smart constructor.
data FpgaImage = FpgaImage'
  { _fiShellVersion      :: !(Maybe Text)
  , _fiPciId             :: !(Maybe PciId)
  , _fiState             :: !(Maybe FpgaImageState)
  , _fiOwnerAlias        :: !(Maybe Text)
  , _fiFpgaImageId       :: !(Maybe Text)
  , _fiOwnerId           :: !(Maybe Text)
  , _fiUpdateTime        :: !(Maybe ISO8601)
  , _fiName              :: !(Maybe Text)
  , _fiProductCodes      :: !(Maybe [ProductCode])
  , _fiDescription       :: !(Maybe Text)
  , _fiCreateTime        :: !(Maybe ISO8601)
  , _fiTags              :: !(Maybe [Tag])
  , _fiPublic            :: !(Maybe Bool)
  , _fiFpgaImageGlobalId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FpgaImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fiShellVersion' - The version of the AWS Shell that was used to create the bitstream.
--
-- * 'fiPciId' - Information about the PCI bus.
--
-- * 'fiState' - Information about the state of the AFI.
--
-- * 'fiOwnerAlias' - The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
--
-- * 'fiFpgaImageId' - The FPGA image identifier (AFI ID).
--
-- * 'fiOwnerId' - The AWS account ID of the AFI owner.
--
-- * 'fiUpdateTime' - The time of the most recent update to the AFI.
--
-- * 'fiName' - The name of the AFI.
--
-- * 'fiProductCodes' - The product codes for the AFI.
--
-- * 'fiDescription' - The description of the AFI.
--
-- * 'fiCreateTime' - The date and time the AFI was created.
--
-- * 'fiTags' - Any tags assigned to the AFI.
--
-- * 'fiPublic' - Indicates whether the AFI is public.
--
-- * 'fiFpgaImageGlobalId' - The global FPGA image identifier (AGFI ID).
fpgaImage
    :: FpgaImage
fpgaImage =
  FpgaImage'
    { _fiShellVersion = Nothing
    , _fiPciId = Nothing
    , _fiState = Nothing
    , _fiOwnerAlias = Nothing
    , _fiFpgaImageId = Nothing
    , _fiOwnerId = Nothing
    , _fiUpdateTime = Nothing
    , _fiName = Nothing
    , _fiProductCodes = Nothing
    , _fiDescription = Nothing
    , _fiCreateTime = Nothing
    , _fiTags = Nothing
    , _fiPublic = Nothing
    , _fiFpgaImageGlobalId = Nothing
    }


-- | The version of the AWS Shell that was used to create the bitstream.
fiShellVersion :: Lens' FpgaImage (Maybe Text)
fiShellVersion = lens _fiShellVersion (\ s a -> s{_fiShellVersion = a})

-- | Information about the PCI bus.
fiPciId :: Lens' FpgaImage (Maybe PciId)
fiPciId = lens _fiPciId (\ s a -> s{_fiPciId = a})

-- | Information about the state of the AFI.
fiState :: Lens' FpgaImage (Maybe FpgaImageState)
fiState = lens _fiState (\ s a -> s{_fiState = a})

-- | The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
fiOwnerAlias :: Lens' FpgaImage (Maybe Text)
fiOwnerAlias = lens _fiOwnerAlias (\ s a -> s{_fiOwnerAlias = a})

-- | The FPGA image identifier (AFI ID).
fiFpgaImageId :: Lens' FpgaImage (Maybe Text)
fiFpgaImageId = lens _fiFpgaImageId (\ s a -> s{_fiFpgaImageId = a})

-- | The AWS account ID of the AFI owner.
fiOwnerId :: Lens' FpgaImage (Maybe Text)
fiOwnerId = lens _fiOwnerId (\ s a -> s{_fiOwnerId = a})

-- | The time of the most recent update to the AFI.
fiUpdateTime :: Lens' FpgaImage (Maybe UTCTime)
fiUpdateTime = lens _fiUpdateTime (\ s a -> s{_fiUpdateTime = a}) . mapping _Time

-- | The name of the AFI.
fiName :: Lens' FpgaImage (Maybe Text)
fiName = lens _fiName (\ s a -> s{_fiName = a})

-- | The product codes for the AFI.
fiProductCodes :: Lens' FpgaImage [ProductCode]
fiProductCodes = lens _fiProductCodes (\ s a -> s{_fiProductCodes = a}) . _Default . _Coerce

-- | The description of the AFI.
fiDescription :: Lens' FpgaImage (Maybe Text)
fiDescription = lens _fiDescription (\ s a -> s{_fiDescription = a})

-- | The date and time the AFI was created.
fiCreateTime :: Lens' FpgaImage (Maybe UTCTime)
fiCreateTime = lens _fiCreateTime (\ s a -> s{_fiCreateTime = a}) . mapping _Time

-- | Any tags assigned to the AFI.
fiTags :: Lens' FpgaImage [Tag]
fiTags = lens _fiTags (\ s a -> s{_fiTags = a}) . _Default . _Coerce

-- | Indicates whether the AFI is public.
fiPublic :: Lens' FpgaImage (Maybe Bool)
fiPublic = lens _fiPublic (\ s a -> s{_fiPublic = a})

-- | The global FPGA image identifier (AGFI ID).
fiFpgaImageGlobalId :: Lens' FpgaImage (Maybe Text)
fiFpgaImageGlobalId = lens _fiFpgaImageGlobalId (\ s a -> s{_fiFpgaImageGlobalId = a})

instance FromXML FpgaImage where
        parseXML x
          = FpgaImage' <$>
              (x .@? "shellVersion") <*> (x .@? "pciId") <*>
                (x .@? "state")
                <*> (x .@? "ownerAlias")
                <*> (x .@? "fpgaImageId")
                <*> (x .@? "ownerId")
                <*> (x .@? "updateTime")
                <*> (x .@? "name")
                <*>
                (x .@? "productCodes" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "description")
                <*> (x .@? "createTime")
                <*>
                (x .@? "tags" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "public")
                <*> (x .@? "fpgaImageGlobalId")

instance NFData FpgaImage where

-- | Describes an Amazon FPGA image (AFI) attribute.
--
--
--
-- /See:/ 'fpgaImageAttribute' smart constructor.
data FpgaImageAttribute = FpgaImageAttribute'
  { _fiaFpgaImageId     :: !(Maybe Text)
  , _fiaName            :: !(Maybe Text)
  , _fiaProductCodes    :: !(Maybe [ProductCode])
  , _fiaDescription     :: !(Maybe Text)
  , _fiaLoadPermissions :: !(Maybe [LoadPermission])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FpgaImageAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fiaFpgaImageId' - The ID of the AFI.
--
-- * 'fiaName' - The name of the AFI.
--
-- * 'fiaProductCodes' - One or more product codes.
--
-- * 'fiaDescription' - The description of the AFI.
--
-- * 'fiaLoadPermissions' - One or more load permissions.
fpgaImageAttribute
    :: FpgaImageAttribute
fpgaImageAttribute =
  FpgaImageAttribute'
    { _fiaFpgaImageId = Nothing
    , _fiaName = Nothing
    , _fiaProductCodes = Nothing
    , _fiaDescription = Nothing
    , _fiaLoadPermissions = Nothing
    }


-- | The ID of the AFI.
fiaFpgaImageId :: Lens' FpgaImageAttribute (Maybe Text)
fiaFpgaImageId = lens _fiaFpgaImageId (\ s a -> s{_fiaFpgaImageId = a})

-- | The name of the AFI.
fiaName :: Lens' FpgaImageAttribute (Maybe Text)
fiaName = lens _fiaName (\ s a -> s{_fiaName = a})

-- | One or more product codes.
fiaProductCodes :: Lens' FpgaImageAttribute [ProductCode]
fiaProductCodes = lens _fiaProductCodes (\ s a -> s{_fiaProductCodes = a}) . _Default . _Coerce

-- | The description of the AFI.
fiaDescription :: Lens' FpgaImageAttribute (Maybe Text)
fiaDescription = lens _fiaDescription (\ s a -> s{_fiaDescription = a})

-- | One or more load permissions.
fiaLoadPermissions :: Lens' FpgaImageAttribute [LoadPermission]
fiaLoadPermissions = lens _fiaLoadPermissions (\ s a -> s{_fiaLoadPermissions = a}) . _Default . _Coerce

instance FromXML FpgaImageAttribute where
        parseXML x
          = FpgaImageAttribute' <$>
              (x .@? "fpgaImageId") <*> (x .@? "name") <*>
                (x .@? "productCodes" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "description")
                <*>
                (x .@? "loadPermissions" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData FpgaImageAttribute where

-- | Describes the state of the bitstream generation process for an Amazon FPGA image (AFI).
--
--
--
-- /See:/ 'fpgaImageState' smart constructor.
data FpgaImageState = FpgaImageState'
  { _fisCode    :: !(Maybe FpgaImageStateCode)
  , _fisMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FpgaImageState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fisCode' - The state. The following are the possible values:     * @pending@ - AFI bitstream generation is in progress.     * @available@ - The AFI is available for use.     * @failed@ - AFI bitstream generation failed.     * @unavailable@ - The AFI is no longer available for use.
--
-- * 'fisMessage' - If the state is @failed@ , this is the error message.
fpgaImageState
    :: FpgaImageState
fpgaImageState = FpgaImageState' {_fisCode = Nothing, _fisMessage = Nothing}


-- | The state. The following are the possible values:     * @pending@ - AFI bitstream generation is in progress.     * @available@ - The AFI is available for use.     * @failed@ - AFI bitstream generation failed.     * @unavailable@ - The AFI is no longer available for use.
fisCode :: Lens' FpgaImageState (Maybe FpgaImageStateCode)
fisCode = lens _fisCode (\ s a -> s{_fisCode = a})

-- | If the state is @failed@ , this is the error message.
fisMessage :: Lens' FpgaImageState (Maybe Text)
fisMessage = lens _fisMessage (\ s a -> s{_fisMessage = a})

instance FromXML FpgaImageState where
        parseXML x
          = FpgaImageState' <$>
              (x .@? "code") <*> (x .@? "message")

instance NFData FpgaImageState where

-- | Describes a security group.
--
--
--
-- /See:/ 'groupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { _giGroupId   :: !(Maybe Text)
  , _giGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giGroupId' - The ID of the security group.
--
-- * 'giGroupName' - The name of the security group.
groupIdentifier
    :: GroupIdentifier
groupIdentifier =
  GroupIdentifier' {_giGroupId = Nothing, _giGroupName = Nothing}


-- | The ID of the security group.
giGroupId :: Lens' GroupIdentifier (Maybe Text)
giGroupId = lens _giGroupId (\ s a -> s{_giGroupId = a})

-- | The name of the security group.
giGroupName :: Lens' GroupIdentifier (Maybe Text)
giGroupName = lens _giGroupName (\ s a -> s{_giGroupName = a})

instance FromXML GroupIdentifier where
        parseXML x
          = GroupIdentifier' <$>
              (x .@? "groupId") <*> (x .@? "groupName")

instance NFData GroupIdentifier where

instance ToQuery GroupIdentifier where
        toQuery GroupIdentifier'{..}
          = mconcat
              ["GroupId" =: _giGroupId,
               "GroupName" =: _giGroupName]

-- | Describes an event in the history of the Spot Fleet request.
--
--
--
-- /See:/ 'historyRecord' smart constructor.
data HistoryRecord = HistoryRecord'
  { _hrEventInformation :: !EventInformation
  , _hrEventType        :: !EventType
  , _hrTimestamp        :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HistoryRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hrEventInformation' - Information about the event.
--
-- * 'hrEventType' - The event type.     * @error@ - An error with the Spot Fleet request.     * @fleetRequestChange@ - A change in the status or configuration of the Spot Fleet request.     * @instanceChange@ - An instance was launched or terminated.     * @Information@ - An informational event.
--
-- * 'hrTimestamp' - The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
historyRecord
    :: EventInformation -- ^ 'hrEventInformation'
    -> EventType -- ^ 'hrEventType'
    -> UTCTime -- ^ 'hrTimestamp'
    -> HistoryRecord
historyRecord pEventInformation_ pEventType_ pTimestamp_ =
  HistoryRecord'
    { _hrEventInformation = pEventInformation_
    , _hrEventType = pEventType_
    , _hrTimestamp = _Time # pTimestamp_
    }


-- | Information about the event.
hrEventInformation :: Lens' HistoryRecord EventInformation
hrEventInformation = lens _hrEventInformation (\ s a -> s{_hrEventInformation = a})

-- | The event type.     * @error@ - An error with the Spot Fleet request.     * @fleetRequestChange@ - A change in the status or configuration of the Spot Fleet request.     * @instanceChange@ - An instance was launched or terminated.     * @Information@ - An informational event.
hrEventType :: Lens' HistoryRecord EventType
hrEventType = lens _hrEventType (\ s a -> s{_hrEventType = a})

-- | The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
hrTimestamp :: Lens' HistoryRecord UTCTime
hrTimestamp = lens _hrTimestamp (\ s a -> s{_hrTimestamp = a}) . _Time

instance FromXML HistoryRecord where
        parseXML x
          = HistoryRecord' <$>
              (x .@ "eventInformation") <*> (x .@ "eventType") <*>
                (x .@ "timestamp")

instance NFData HistoryRecord where

-- | Describes an event in the history of the EC2 Fleet.
--
--
--
-- /See:/ 'historyRecordEntry' smart constructor.
data HistoryRecordEntry = HistoryRecordEntry'
  { _hreEventType        :: !(Maybe FleetEventType)
  , _hreEventInformation :: !(Maybe EventInformation)
  , _hreTimestamp        :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HistoryRecordEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hreEventType' - The event type.
--
-- * 'hreEventInformation' - Information about the event.
--
-- * 'hreTimestamp' - The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
historyRecordEntry
    :: HistoryRecordEntry
historyRecordEntry =
  HistoryRecordEntry'
    { _hreEventType = Nothing
    , _hreEventInformation = Nothing
    , _hreTimestamp = Nothing
    }


-- | The event type.
hreEventType :: Lens' HistoryRecordEntry (Maybe FleetEventType)
hreEventType = lens _hreEventType (\ s a -> s{_hreEventType = a})

-- | Information about the event.
hreEventInformation :: Lens' HistoryRecordEntry (Maybe EventInformation)
hreEventInformation = lens _hreEventInformation (\ s a -> s{_hreEventInformation = a})

-- | The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
hreTimestamp :: Lens' HistoryRecordEntry (Maybe UTCTime)
hreTimestamp = lens _hreTimestamp (\ s a -> s{_hreTimestamp = a}) . mapping _Time

instance FromXML HistoryRecordEntry where
        parseXML x
          = HistoryRecordEntry' <$>
              (x .@? "eventType") <*> (x .@? "eventInformation")
                <*> (x .@? "timestamp")

instance NFData HistoryRecordEntry where

-- | Describes the properties of the Dedicated Host.
--
--
--
-- /See:/ 'host' smart constructor.
data Host = Host'
  { _hReleaseTime       :: !(Maybe ISO8601)
  , _hState             :: !(Maybe AllocationState)
  , _hClientToken       :: !(Maybe Text)
  , _hHostId            :: !(Maybe Text)
  , _hAvailableCapacity :: !(Maybe AvailableCapacity)
  , _hHostReservationId :: !(Maybe Text)
  , _hHostProperties    :: !(Maybe HostProperties)
  , _hAvailabilityZone  :: !(Maybe Text)
  , _hInstances         :: !(Maybe [HostInstance])
  , _hAllocationTime    :: !(Maybe ISO8601)
  , _hAutoPlacement     :: !(Maybe AutoPlacement)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Host' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hReleaseTime' - The time that the Dedicated Host was released.
--
-- * 'hState' - The Dedicated Host's state.
--
-- * 'hClientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'hHostId' - The ID of the Dedicated Host.
--
-- * 'hAvailableCapacity' - The number of new instances that can be launched onto the Dedicated Host.
--
-- * 'hHostReservationId' - The reservation ID of the Dedicated Host. This returns a @null@ response if the Dedicated Host doesn't have an associated reservation.
--
-- * 'hHostProperties' - The hardware specifications of the Dedicated Host.
--
-- * 'hAvailabilityZone' - The Availability Zone of the Dedicated Host.
--
-- * 'hInstances' - The IDs and instance type that are currently running on the Dedicated Host.
--
-- * 'hAllocationTime' - The time that the Dedicated Host was allocated.
--
-- * 'hAutoPlacement' - Whether auto-placement is on or off.
host
    :: Host
host =
  Host'
    { _hReleaseTime = Nothing
    , _hState = Nothing
    , _hClientToken = Nothing
    , _hHostId = Nothing
    , _hAvailableCapacity = Nothing
    , _hHostReservationId = Nothing
    , _hHostProperties = Nothing
    , _hAvailabilityZone = Nothing
    , _hInstances = Nothing
    , _hAllocationTime = Nothing
    , _hAutoPlacement = Nothing
    }


-- | The time that the Dedicated Host was released.
hReleaseTime :: Lens' Host (Maybe UTCTime)
hReleaseTime = lens _hReleaseTime (\ s a -> s{_hReleaseTime = a}) . mapping _Time

-- | The Dedicated Host's state.
hState :: Lens' Host (Maybe AllocationState)
hState = lens _hState (\ s a -> s{_hState = a})

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
hClientToken :: Lens' Host (Maybe Text)
hClientToken = lens _hClientToken (\ s a -> s{_hClientToken = a})

-- | The ID of the Dedicated Host.
hHostId :: Lens' Host (Maybe Text)
hHostId = lens _hHostId (\ s a -> s{_hHostId = a})

-- | The number of new instances that can be launched onto the Dedicated Host.
hAvailableCapacity :: Lens' Host (Maybe AvailableCapacity)
hAvailableCapacity = lens _hAvailableCapacity (\ s a -> s{_hAvailableCapacity = a})

-- | The reservation ID of the Dedicated Host. This returns a @null@ response if the Dedicated Host doesn't have an associated reservation.
hHostReservationId :: Lens' Host (Maybe Text)
hHostReservationId = lens _hHostReservationId (\ s a -> s{_hHostReservationId = a})

-- | The hardware specifications of the Dedicated Host.
hHostProperties :: Lens' Host (Maybe HostProperties)
hHostProperties = lens _hHostProperties (\ s a -> s{_hHostProperties = a})

-- | The Availability Zone of the Dedicated Host.
hAvailabilityZone :: Lens' Host (Maybe Text)
hAvailabilityZone = lens _hAvailabilityZone (\ s a -> s{_hAvailabilityZone = a})

-- | The IDs and instance type that are currently running on the Dedicated Host.
hInstances :: Lens' Host [HostInstance]
hInstances = lens _hInstances (\ s a -> s{_hInstances = a}) . _Default . _Coerce

-- | The time that the Dedicated Host was allocated.
hAllocationTime :: Lens' Host (Maybe UTCTime)
hAllocationTime = lens _hAllocationTime (\ s a -> s{_hAllocationTime = a}) . mapping _Time

-- | Whether auto-placement is on or off.
hAutoPlacement :: Lens' Host (Maybe AutoPlacement)
hAutoPlacement = lens _hAutoPlacement (\ s a -> s{_hAutoPlacement = a})

instance FromXML Host where
        parseXML x
          = Host' <$>
              (x .@? "releaseTime") <*> (x .@? "state") <*>
                (x .@? "clientToken")
                <*> (x .@? "hostId")
                <*> (x .@? "availableCapacity")
                <*> (x .@? "hostReservationId")
                <*> (x .@? "hostProperties")
                <*> (x .@? "availabilityZone")
                <*>
                (x .@? "instances" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "allocationTime")
                <*> (x .@? "autoPlacement")

instance NFData Host where

-- | Describes an instance running on a Dedicated Host.
--
--
--
-- /See:/ 'hostInstance' smart constructor.
data HostInstance = HostInstance'
  { _hiInstanceId   :: !(Maybe Text)
  , _hiInstanceType :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hiInstanceId' - the IDs of instances that are running on the Dedicated Host.
--
-- * 'hiInstanceType' - The instance type size (for example, @m3.medium@ ) of the running instance.
hostInstance
    :: HostInstance
hostInstance =
  HostInstance' {_hiInstanceId = Nothing, _hiInstanceType = Nothing}


-- | the IDs of instances that are running on the Dedicated Host.
hiInstanceId :: Lens' HostInstance (Maybe Text)
hiInstanceId = lens _hiInstanceId (\ s a -> s{_hiInstanceId = a})

-- | The instance type size (for example, @m3.medium@ ) of the running instance.
hiInstanceType :: Lens' HostInstance (Maybe Text)
hiInstanceType = lens _hiInstanceType (\ s a -> s{_hiInstanceType = a})

instance FromXML HostInstance where
        parseXML x
          = HostInstance' <$>
              (x .@? "instanceId") <*> (x .@? "instanceType")

instance NFData HostInstance where

-- | Details about the Dedicated Host Reservation offering.
--
--
--
-- /See:/ 'hostOffering' smart constructor.
data HostOffering = HostOffering'
  { _hoInstanceFamily :: !(Maybe Text)
  , _hoCurrencyCode   :: !(Maybe CurrencyCodeValues)
  , _hoHourlyPrice    :: !(Maybe Text)
  , _hoUpfrontPrice   :: !(Maybe Text)
  , _hoOfferingId     :: !(Maybe Text)
  , _hoDuration       :: !(Maybe Int)
  , _hoPaymentOption  :: !(Maybe PaymentOption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hoInstanceFamily' - The instance family of the offering.
--
-- * 'hoCurrencyCode' - The currency of the offering.
--
-- * 'hoHourlyPrice' - The hourly price of the offering.
--
-- * 'hoUpfrontPrice' - The upfront price of the offering. Does not apply to No Upfront offerings.
--
-- * 'hoOfferingId' - The ID of the offering.
--
-- * 'hoDuration' - The duration of the offering (in seconds).
--
-- * 'hoPaymentOption' - The available payment option.
hostOffering
    :: HostOffering
hostOffering =
  HostOffering'
    { _hoInstanceFamily = Nothing
    , _hoCurrencyCode = Nothing
    , _hoHourlyPrice = Nothing
    , _hoUpfrontPrice = Nothing
    , _hoOfferingId = Nothing
    , _hoDuration = Nothing
    , _hoPaymentOption = Nothing
    }


-- | The instance family of the offering.
hoInstanceFamily :: Lens' HostOffering (Maybe Text)
hoInstanceFamily = lens _hoInstanceFamily (\ s a -> s{_hoInstanceFamily = a})

-- | The currency of the offering.
hoCurrencyCode :: Lens' HostOffering (Maybe CurrencyCodeValues)
hoCurrencyCode = lens _hoCurrencyCode (\ s a -> s{_hoCurrencyCode = a})

-- | The hourly price of the offering.
hoHourlyPrice :: Lens' HostOffering (Maybe Text)
hoHourlyPrice = lens _hoHourlyPrice (\ s a -> s{_hoHourlyPrice = a})

-- | The upfront price of the offering. Does not apply to No Upfront offerings.
hoUpfrontPrice :: Lens' HostOffering (Maybe Text)
hoUpfrontPrice = lens _hoUpfrontPrice (\ s a -> s{_hoUpfrontPrice = a})

-- | The ID of the offering.
hoOfferingId :: Lens' HostOffering (Maybe Text)
hoOfferingId = lens _hoOfferingId (\ s a -> s{_hoOfferingId = a})

-- | The duration of the offering (in seconds).
hoDuration :: Lens' HostOffering (Maybe Int)
hoDuration = lens _hoDuration (\ s a -> s{_hoDuration = a})

-- | The available payment option.
hoPaymentOption :: Lens' HostOffering (Maybe PaymentOption)
hoPaymentOption = lens _hoPaymentOption (\ s a -> s{_hoPaymentOption = a})

instance FromXML HostOffering where
        parseXML x
          = HostOffering' <$>
              (x .@? "instanceFamily") <*> (x .@? "currencyCode")
                <*> (x .@? "hourlyPrice")
                <*> (x .@? "upfrontPrice")
                <*> (x .@? "offeringId")
                <*> (x .@? "duration")
                <*> (x .@? "paymentOption")

instance NFData HostOffering where

-- | Describes properties of a Dedicated Host.
--
--
--
-- /See:/ 'hostProperties' smart constructor.
data HostProperties = HostProperties'
  { _hpInstanceType :: !(Maybe Text)
  , _hpTotalVCPUs   :: !(Maybe Int)
  , _hpCores        :: !(Maybe Int)
  , _hpSockets      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpInstanceType' - The instance type size that the Dedicated Host supports (for example, @m3.medium@ ).
--
-- * 'hpTotalVCPUs' - The number of vCPUs on the Dedicated Host.
--
-- * 'hpCores' - The number of cores on the Dedicated Host.
--
-- * 'hpSockets' - The number of sockets on the Dedicated Host.
hostProperties
    :: HostProperties
hostProperties =
  HostProperties'
    { _hpInstanceType = Nothing
    , _hpTotalVCPUs = Nothing
    , _hpCores = Nothing
    , _hpSockets = Nothing
    }


-- | The instance type size that the Dedicated Host supports (for example, @m3.medium@ ).
hpInstanceType :: Lens' HostProperties (Maybe Text)
hpInstanceType = lens _hpInstanceType (\ s a -> s{_hpInstanceType = a})

-- | The number of vCPUs on the Dedicated Host.
hpTotalVCPUs :: Lens' HostProperties (Maybe Int)
hpTotalVCPUs = lens _hpTotalVCPUs (\ s a -> s{_hpTotalVCPUs = a})

-- | The number of cores on the Dedicated Host.
hpCores :: Lens' HostProperties (Maybe Int)
hpCores = lens _hpCores (\ s a -> s{_hpCores = a})

-- | The number of sockets on the Dedicated Host.
hpSockets :: Lens' HostProperties (Maybe Int)
hpSockets = lens _hpSockets (\ s a -> s{_hpSockets = a})

instance FromXML HostProperties where
        parseXML x
          = HostProperties' <$>
              (x .@? "instanceType") <*> (x .@? "totalVCpus") <*>
                (x .@? "cores")
                <*> (x .@? "sockets")

instance NFData HostProperties where

-- | Details about the Dedicated Host Reservation and associated Dedicated Hosts.
--
--
--
-- /See:/ 'hostReservation' smart constructor.
data HostReservation = HostReservation'
  { _hrState             :: !(Maybe ReservationState)
  , _hrInstanceFamily    :: !(Maybe Text)
  , _hrCurrencyCode      :: !(Maybe CurrencyCodeValues)
  , _hrHostReservationId :: !(Maybe Text)
  , _hrStart             :: !(Maybe ISO8601)
  , _hrHourlyPrice       :: !(Maybe Text)
  , _hrCount             :: !(Maybe Int)
  , _hrUpfrontPrice      :: !(Maybe Text)
  , _hrEnd               :: !(Maybe ISO8601)
  , _hrHostIdSet         :: !(Maybe [Text])
  , _hrOfferingId        :: !(Maybe Text)
  , _hrDuration          :: !(Maybe Int)
  , _hrPaymentOption     :: !(Maybe PaymentOption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hrState' - The state of the reservation.
--
-- * 'hrInstanceFamily' - The instance family of the Dedicated Host Reservation. The instance family on the Dedicated Host must be the same in order for it to benefit from the reservation.
--
-- * 'hrCurrencyCode' - The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- * 'hrHostReservationId' - The ID of the reservation that specifies the associated Dedicated Hosts.
--
-- * 'hrStart' - The date and time that the reservation started.
--
-- * 'hrHourlyPrice' - The hourly price of the reservation.
--
-- * 'hrCount' - The number of Dedicated Hosts the reservation is associated with.
--
-- * 'hrUpfrontPrice' - The upfront price of the reservation.
--
-- * 'hrEnd' - The date and time that the reservation ends.
--
-- * 'hrHostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
--
-- * 'hrOfferingId' - The ID of the reservation. This remains the same regardless of which Dedicated Hosts are associated with it.
--
-- * 'hrDuration' - The length of the reservation's term, specified in seconds. Can be @31536000 (1 year)@ | @94608000 (3 years)@ .
--
-- * 'hrPaymentOption' - The payment option selected for this reservation.
hostReservation
    :: HostReservation
hostReservation =
  HostReservation'
    { _hrState = Nothing
    , _hrInstanceFamily = Nothing
    , _hrCurrencyCode = Nothing
    , _hrHostReservationId = Nothing
    , _hrStart = Nothing
    , _hrHourlyPrice = Nothing
    , _hrCount = Nothing
    , _hrUpfrontPrice = Nothing
    , _hrEnd = Nothing
    , _hrHostIdSet = Nothing
    , _hrOfferingId = Nothing
    , _hrDuration = Nothing
    , _hrPaymentOption = Nothing
    }


-- | The state of the reservation.
hrState :: Lens' HostReservation (Maybe ReservationState)
hrState = lens _hrState (\ s a -> s{_hrState = a})

-- | The instance family of the Dedicated Host Reservation. The instance family on the Dedicated Host must be the same in order for it to benefit from the reservation.
hrInstanceFamily :: Lens' HostReservation (Maybe Text)
hrInstanceFamily = lens _hrInstanceFamily (\ s a -> s{_hrInstanceFamily = a})

-- | The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
hrCurrencyCode :: Lens' HostReservation (Maybe CurrencyCodeValues)
hrCurrencyCode = lens _hrCurrencyCode (\ s a -> s{_hrCurrencyCode = a})

-- | The ID of the reservation that specifies the associated Dedicated Hosts.
hrHostReservationId :: Lens' HostReservation (Maybe Text)
hrHostReservationId = lens _hrHostReservationId (\ s a -> s{_hrHostReservationId = a})

-- | The date and time that the reservation started.
hrStart :: Lens' HostReservation (Maybe UTCTime)
hrStart = lens _hrStart (\ s a -> s{_hrStart = a}) . mapping _Time

-- | The hourly price of the reservation.
hrHourlyPrice :: Lens' HostReservation (Maybe Text)
hrHourlyPrice = lens _hrHourlyPrice (\ s a -> s{_hrHourlyPrice = a})

-- | The number of Dedicated Hosts the reservation is associated with.
hrCount :: Lens' HostReservation (Maybe Int)
hrCount = lens _hrCount (\ s a -> s{_hrCount = a})

-- | The upfront price of the reservation.
hrUpfrontPrice :: Lens' HostReservation (Maybe Text)
hrUpfrontPrice = lens _hrUpfrontPrice (\ s a -> s{_hrUpfrontPrice = a})

-- | The date and time that the reservation ends.
hrEnd :: Lens' HostReservation (Maybe UTCTime)
hrEnd = lens _hrEnd (\ s a -> s{_hrEnd = a}) . mapping _Time

-- | The IDs of the Dedicated Hosts associated with the reservation.
hrHostIdSet :: Lens' HostReservation [Text]
hrHostIdSet = lens _hrHostIdSet (\ s a -> s{_hrHostIdSet = a}) . _Default . _Coerce

-- | The ID of the reservation. This remains the same regardless of which Dedicated Hosts are associated with it.
hrOfferingId :: Lens' HostReservation (Maybe Text)
hrOfferingId = lens _hrOfferingId (\ s a -> s{_hrOfferingId = a})

-- | The length of the reservation's term, specified in seconds. Can be @31536000 (1 year)@ | @94608000 (3 years)@ .
hrDuration :: Lens' HostReservation (Maybe Int)
hrDuration = lens _hrDuration (\ s a -> s{_hrDuration = a})

-- | The payment option selected for this reservation.
hrPaymentOption :: Lens' HostReservation (Maybe PaymentOption)
hrPaymentOption = lens _hrPaymentOption (\ s a -> s{_hrPaymentOption = a})

instance FromXML HostReservation where
        parseXML x
          = HostReservation' <$>
              (x .@? "state") <*> (x .@? "instanceFamily") <*>
                (x .@? "currencyCode")
                <*> (x .@? "hostReservationId")
                <*> (x .@? "start")
                <*> (x .@? "hourlyPrice")
                <*> (x .@? "count")
                <*> (x .@? "upfrontPrice")
                <*> (x .@? "end")
                <*>
                (x .@? "hostIdSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "offeringId")
                <*> (x .@? "duration")
                <*> (x .@? "paymentOption")

instance NFData HostReservation where

-- | Describes an IAM instance profile.
--
--
--
-- /See:/ 'iamInstanceProfile' smart constructor.
data IAMInstanceProfile = IAMInstanceProfile'
  { _iapARN :: !(Maybe Text)
  , _iapId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IAMInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iapARN' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'iapId' - The ID of the instance profile.
iamInstanceProfile
    :: IAMInstanceProfile
iamInstanceProfile = IAMInstanceProfile' {_iapARN = Nothing, _iapId = Nothing}


-- | The Amazon Resource Name (ARN) of the instance profile.
iapARN :: Lens' IAMInstanceProfile (Maybe Text)
iapARN = lens _iapARN (\ s a -> s{_iapARN = a})

-- | The ID of the instance profile.
iapId :: Lens' IAMInstanceProfile (Maybe Text)
iapId = lens _iapId (\ s a -> s{_iapId = a})

instance FromXML IAMInstanceProfile where
        parseXML x
          = IAMInstanceProfile' <$>
              (x .@? "arn") <*> (x .@? "id")

instance NFData IAMInstanceProfile where

-- | Describes an association between an IAM instance profile and an instance.
--
--
--
-- /See:/ 'iamInstanceProfileAssociation' smart constructor.
data IAMInstanceProfileAssociation = IAMInstanceProfileAssociation'
  { _iapaAssociationId      :: !(Maybe Text)
  , _iapaInstanceId         :: !(Maybe Text)
  , _iapaState              :: !(Maybe IAMInstanceProfileAssociationState)
  , _iapaIAMInstanceProfile :: !(Maybe IAMInstanceProfile)
  , _iapaTimestamp          :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IAMInstanceProfileAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iapaAssociationId' - The ID of the association.
--
-- * 'iapaInstanceId' - The ID of the instance.
--
-- * 'iapaState' - The state of the association.
--
-- * 'iapaIAMInstanceProfile' - The IAM instance profile.
--
-- * 'iapaTimestamp' - The time the IAM instance profile was associated with the instance.
iamInstanceProfileAssociation
    :: IAMInstanceProfileAssociation
iamInstanceProfileAssociation =
  IAMInstanceProfileAssociation'
    { _iapaAssociationId = Nothing
    , _iapaInstanceId = Nothing
    , _iapaState = Nothing
    , _iapaIAMInstanceProfile = Nothing
    , _iapaTimestamp = Nothing
    }


-- | The ID of the association.
iapaAssociationId :: Lens' IAMInstanceProfileAssociation (Maybe Text)
iapaAssociationId = lens _iapaAssociationId (\ s a -> s{_iapaAssociationId = a})

-- | The ID of the instance.
iapaInstanceId :: Lens' IAMInstanceProfileAssociation (Maybe Text)
iapaInstanceId = lens _iapaInstanceId (\ s a -> s{_iapaInstanceId = a})

-- | The state of the association.
iapaState :: Lens' IAMInstanceProfileAssociation (Maybe IAMInstanceProfileAssociationState)
iapaState = lens _iapaState (\ s a -> s{_iapaState = a})

-- | The IAM instance profile.
iapaIAMInstanceProfile :: Lens' IAMInstanceProfileAssociation (Maybe IAMInstanceProfile)
iapaIAMInstanceProfile = lens _iapaIAMInstanceProfile (\ s a -> s{_iapaIAMInstanceProfile = a})

-- | The time the IAM instance profile was associated with the instance.
iapaTimestamp :: Lens' IAMInstanceProfileAssociation (Maybe UTCTime)
iapaTimestamp = lens _iapaTimestamp (\ s a -> s{_iapaTimestamp = a}) . mapping _Time

instance FromXML IAMInstanceProfileAssociation where
        parseXML x
          = IAMInstanceProfileAssociation' <$>
              (x .@? "associationId") <*> (x .@? "instanceId") <*>
                (x .@? "state")
                <*> (x .@? "iamInstanceProfile")
                <*> (x .@? "timestamp")

instance NFData IAMInstanceProfileAssociation where

-- | Describes an IAM instance profile.
--
--
--
-- /See:/ 'iamInstanceProfileSpecification' smart constructor.
data IAMInstanceProfileSpecification = IAMInstanceProfileSpecification'
  { _iapsARN  :: !(Maybe Text)
  , _iapsName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IAMInstanceProfileSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iapsARN' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'iapsName' - The name of the instance profile.
iamInstanceProfileSpecification
    :: IAMInstanceProfileSpecification
iamInstanceProfileSpecification =
  IAMInstanceProfileSpecification' {_iapsARN = Nothing, _iapsName = Nothing}


-- | The Amazon Resource Name (ARN) of the instance profile.
iapsARN :: Lens' IAMInstanceProfileSpecification (Maybe Text)
iapsARN = lens _iapsARN (\ s a -> s{_iapsARN = a})

-- | The name of the instance profile.
iapsName :: Lens' IAMInstanceProfileSpecification (Maybe Text)
iapsName = lens _iapsName (\ s a -> s{_iapsName = a})

instance FromXML IAMInstanceProfileSpecification
         where
        parseXML x
          = IAMInstanceProfileSpecification' <$>
              (x .@? "arn") <*> (x .@? "name")

instance NFData IAMInstanceProfileSpecification where

instance ToQuery IAMInstanceProfileSpecification
         where
        toQuery IAMInstanceProfileSpecification'{..}
          = mconcat ["Arn" =: _iapsARN, "Name" =: _iapsName]

-- | Describes the ICMP type and code.
--
--
--
-- /See:/ 'icmpTypeCode' smart constructor.
data ICMPTypeCode = ICMPTypeCode'
  { _itcCode :: !(Maybe Int)
  , _itcType :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ICMPTypeCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itcCode' - The ICMP code. A value of -1 means all codes for the specified ICMP type.
--
-- * 'itcType' - The ICMP type. A value of -1 means all types.
icmpTypeCode
    :: ICMPTypeCode
icmpTypeCode = ICMPTypeCode' {_itcCode = Nothing, _itcType = Nothing}


-- | The ICMP code. A value of -1 means all codes for the specified ICMP type.
itcCode :: Lens' ICMPTypeCode (Maybe Int)
itcCode = lens _itcCode (\ s a -> s{_itcCode = a})

-- | The ICMP type. A value of -1 means all types.
itcType :: Lens' ICMPTypeCode (Maybe Int)
itcType = lens _itcType (\ s a -> s{_itcType = a})

instance FromXML ICMPTypeCode where
        parseXML x
          = ICMPTypeCode' <$> (x .@? "code") <*> (x .@? "type")

instance NFData ICMPTypeCode where

instance ToQuery ICMPTypeCode where
        toQuery ICMPTypeCode'{..}
          = mconcat ["Code" =: _itcCode, "Type" =: _itcType]

-- | Describes a set of permissions for a security group rule.
--
--
--
-- /See:/ 'ipPermission' smart constructor.
data IPPermission = IPPermission'
  { _ipFromPort         :: !(Maybe Int)
  , _ipUserIdGroupPairs :: !(Maybe [UserIdGroupPair])
  , _ipPrefixListIds    :: !(Maybe [PrefixListId])
  , _ipToPort           :: !(Maybe Int)
  , _ipIPv6Ranges       :: !(Maybe [IPv6Range])
  , _ipIPRanges         :: !(Maybe [IPRange])
  , _ipIPProtocol       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipFromPort' - The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
--
-- * 'ipUserIdGroupPairs' - One or more security group and AWS account ID pairs.
--
-- * 'ipPrefixListIds' - (EC2-VPC only; valid for 'AuthorizeSecurityGroupEgress' , 'RevokeSecurityGroupEgress' and 'DescribeSecurityGroups' only) One or more prefix list IDs for an AWS service. In an 'AuthorizeSecurityGroupEgress' request, this is the AWS service that you want to access through a VPC endpoint from instances associated with the security group.
--
-- * 'ipToPort' - The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes for the specified ICMP type. If you specify all ICMP/ICMPv6 types, you must specify all codes.
--
-- * 'ipIPv6Ranges' - [EC2-VPC only] One or more IPv6 ranges.
--
-- * 'ipIPRanges' - One or more IPv4 ranges.
--
-- * 'ipIPProtocol' - The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ).  [EC2-VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @58@ (ICMPv6) allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @58@ (ICMPv6), you can optionally specify a port range; if you don't, traffic for all types and codes is allowed when authorizing rules.
ipPermission
    :: Text -- ^ 'ipIPProtocol'
    -> IPPermission
ipPermission pIPProtocol_ =
  IPPermission'
    { _ipFromPort = Nothing
    , _ipUserIdGroupPairs = Nothing
    , _ipPrefixListIds = Nothing
    , _ipToPort = Nothing
    , _ipIPv6Ranges = Nothing
    , _ipIPRanges = Nothing
    , _ipIPProtocol = pIPProtocol_
    }


-- | The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
ipFromPort :: Lens' IPPermission (Maybe Int)
ipFromPort = lens _ipFromPort (\ s a -> s{_ipFromPort = a})

-- | One or more security group and AWS account ID pairs.
ipUserIdGroupPairs :: Lens' IPPermission [UserIdGroupPair]
ipUserIdGroupPairs = lens _ipUserIdGroupPairs (\ s a -> s{_ipUserIdGroupPairs = a}) . _Default . _Coerce

-- | (EC2-VPC only; valid for 'AuthorizeSecurityGroupEgress' , 'RevokeSecurityGroupEgress' and 'DescribeSecurityGroups' only) One or more prefix list IDs for an AWS service. In an 'AuthorizeSecurityGroupEgress' request, this is the AWS service that you want to access through a VPC endpoint from instances associated with the security group.
ipPrefixListIds :: Lens' IPPermission [PrefixListId]
ipPrefixListIds = lens _ipPrefixListIds (\ s a -> s{_ipPrefixListIds = a}) . _Default . _Coerce

-- | The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes for the specified ICMP type. If you specify all ICMP/ICMPv6 types, you must specify all codes.
ipToPort :: Lens' IPPermission (Maybe Int)
ipToPort = lens _ipToPort (\ s a -> s{_ipToPort = a})

-- | [EC2-VPC only] One or more IPv6 ranges.
ipIPv6Ranges :: Lens' IPPermission [IPv6Range]
ipIPv6Ranges = lens _ipIPv6Ranges (\ s a -> s{_ipIPv6Ranges = a}) . _Default . _Coerce

-- | One or more IPv4 ranges.
ipIPRanges :: Lens' IPPermission [IPRange]
ipIPRanges = lens _ipIPRanges (\ s a -> s{_ipIPRanges = a}) . _Default . _Coerce

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ).  [EC2-VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @58@ (ICMPv6) allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @58@ (ICMPv6), you can optionally specify a port range; if you don't, traffic for all types and codes is allowed when authorizing rules.
ipIPProtocol :: Lens' IPPermission Text
ipIPProtocol = lens _ipIPProtocol (\ s a -> s{_ipIPProtocol = a})

instance FromXML IPPermission where
        parseXML x
          = IPPermission' <$>
              (x .@? "fromPort") <*>
                (x .@? "groups" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "prefixListIds" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "toPort")
                <*>
                (x .@? "ipv6Ranges" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "ipRanges" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "ipProtocol")

instance NFData IPPermission where

instance ToQuery IPPermission where
        toQuery IPPermission'{..}
          = mconcat
              ["FromPort" =: _ipFromPort,
               toQuery
                 (toQueryList "Groups" <$> _ipUserIdGroupPairs),
               toQuery
                 (toQueryList "PrefixListIds" <$> _ipPrefixListIds),
               "ToPort" =: _ipToPort,
               toQuery (toQueryList "Ipv6Ranges" <$> _ipIPv6Ranges),
               toQuery (toQueryList "IpRanges" <$> _ipIPRanges),
               "IpProtocol" =: _ipIPProtocol]

-- | Describes an IPv4 range.
--
--
--
-- /See:/ 'ipRange' smart constructor.
data IPRange = IPRange'
  { _iprDescription :: !(Maybe Text)
  , _iprCidrIP      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iprDescription' - A description for the security group rule that references this IPv4 address range. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
--
-- * 'iprCidrIP' - The IPv4 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv4 address, use the /32 prefix length.
ipRange
    :: Text -- ^ 'iprCidrIP'
    -> IPRange
ipRange pCidrIP_ = IPRange' {_iprDescription = Nothing, _iprCidrIP = pCidrIP_}


-- | A description for the security group rule that references this IPv4 address range. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
iprDescription :: Lens' IPRange (Maybe Text)
iprDescription = lens _iprDescription (\ s a -> s{_iprDescription = a})

-- | The IPv4 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv4 address, use the /32 prefix length.
iprCidrIP :: Lens' IPRange Text
iprCidrIP = lens _iprCidrIP (\ s a -> s{_iprCidrIP = a})

instance FromXML IPRange where
        parseXML x
          = IPRange' <$>
              (x .@? "description") <*> (x .@ "cidrIp")

instance NFData IPRange where

instance ToQuery IPRange where
        toQuery IPRange'{..}
          = mconcat
              ["Description" =: _iprDescription,
               "CidrIp" =: _iprCidrIP]

-- | Describes an IPv6 CIDR block.
--
--
--
-- /See:/ 'ipv6CidrBlock' smart constructor.
newtype IPv6CidrBlock = IPv6CidrBlock'
  { _icbIPv6CidrBlock :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPv6CidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icbIPv6CidrBlock' - The IPv6 CIDR block.
ipv6CidrBlock
    :: IPv6CidrBlock
ipv6CidrBlock = IPv6CidrBlock' {_icbIPv6CidrBlock = Nothing}


-- | The IPv6 CIDR block.
icbIPv6CidrBlock :: Lens' IPv6CidrBlock (Maybe Text)
icbIPv6CidrBlock = lens _icbIPv6CidrBlock (\ s a -> s{_icbIPv6CidrBlock = a})

instance FromXML IPv6CidrBlock where
        parseXML x
          = IPv6CidrBlock' <$> (x .@? "ipv6CidrBlock")

instance NFData IPv6CidrBlock where

-- | [EC2-VPC only] Describes an IPv6 range.
--
--
--
-- /See:/ 'ipv6Range' smart constructor.
data IPv6Range = IPv6Range'
  { _irCidrIPv6    :: !(Maybe Text)
  , _irDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPv6Range' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irCidrIPv6' - The IPv6 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv6 address, use the /128 prefix length.
--
-- * 'irDescription' - A description for the security group rule that references this IPv6 address range. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
ipv6Range
    :: IPv6Range
ipv6Range = IPv6Range' {_irCidrIPv6 = Nothing, _irDescription = Nothing}


-- | The IPv6 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv6 address, use the /128 prefix length.
irCidrIPv6 :: Lens' IPv6Range (Maybe Text)
irCidrIPv6 = lens _irCidrIPv6 (\ s a -> s{_irCidrIPv6 = a})

-- | A description for the security group rule that references this IPv6 address range. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
irDescription :: Lens' IPv6Range (Maybe Text)
irDescription = lens _irDescription (\ s a -> s{_irDescription = a})

instance FromXML IPv6Range where
        parseXML x
          = IPv6Range' <$>
              (x .@? "cidrIpv6") <*> (x .@? "description")

instance NFData IPv6Range where

instance ToQuery IPv6Range where
        toQuery IPv6Range'{..}
          = mconcat
              ["CidrIpv6" =: _irCidrIPv6,
               "Description" =: _irDescription]

-- | Describes the ID format for a resource.
--
--
--
-- /See:/ 'idFormat' smart constructor.
data IdFormat = IdFormat'
  { _ifUseLongIds :: !(Maybe Bool)
  , _ifDeadline   :: !(Maybe ISO8601)
  , _ifResource   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifUseLongIds' - Indicates whether longer IDs (17-character IDs) are enabled for the resource.
--
-- * 'ifDeadline' - The date in UTC at which you are permanently switched over to using longer IDs. If a deadline is not yet available for this resource type, this field is not returned.
--
-- * 'ifResource' - The type of resource.
idFormat
    :: IdFormat
idFormat =
  IdFormat'
    {_ifUseLongIds = Nothing, _ifDeadline = Nothing, _ifResource = Nothing}


-- | Indicates whether longer IDs (17-character IDs) are enabled for the resource.
ifUseLongIds :: Lens' IdFormat (Maybe Bool)
ifUseLongIds = lens _ifUseLongIds (\ s a -> s{_ifUseLongIds = a})

-- | The date in UTC at which you are permanently switched over to using longer IDs. If a deadline is not yet available for this resource type, this field is not returned.
ifDeadline :: Lens' IdFormat (Maybe UTCTime)
ifDeadline = lens _ifDeadline (\ s a -> s{_ifDeadline = a}) . mapping _Time

-- | The type of resource.
ifResource :: Lens' IdFormat (Maybe Text)
ifResource = lens _ifResource (\ s a -> s{_ifResource = a})

instance FromXML IdFormat where
        parseXML x
          = IdFormat' <$>
              (x .@? "useLongIds") <*> (x .@? "deadline") <*>
                (x .@? "resource")

instance NFData IdFormat where

-- | Describes an image.
--
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
  { _iPlatform            :: !(Maybe PlatformValues)
  , _iEnaSupport          :: !(Maybe Bool)
  , _iImageOwnerAlias     :: !(Maybe Text)
  , _iRAMDiskId           :: !(Maybe Text)
  , _iKernelId            :: !(Maybe Text)
  , _iRootDeviceName      :: !(Maybe Text)
  , _iSRIOVNetSupport     :: !(Maybe Text)
  , _iName                :: !(Maybe Text)
  , _iCreationDate        :: !(Maybe Text)
  , _iProductCodes        :: !(Maybe [ProductCode])
  , _iStateReason         :: !(Maybe StateReason)
  , _iDescription         :: !(Maybe Text)
  , _iBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  , _iTags                :: !(Maybe [Tag])
  , _iImageId             :: !Text
  , _iImageLocation       :: !Text
  , _iState               :: !ImageState
  , _iOwnerId             :: !Text
  , _iPublic              :: !Bool
  , _iArchitecture        :: !ArchitectureValues
  , _iImageType           :: !ImageTypeValues
  , _iRootDeviceType      :: !DeviceType
  , _iVirtualizationType  :: !VirtualizationType
  , _iHypervisor          :: !HypervisorType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iPlatform' - The value is @Windows@ for Windows AMIs; otherwise blank.
--
-- * 'iEnaSupport' - Specifies whether enhanced networking with ENA is enabled.
--
-- * 'iImageOwnerAlias' - The AWS account alias (for example, @amazon@ , @self@ ) or the AWS account ID of the AMI owner.
--
-- * 'iRAMDiskId' - The RAM disk associated with the image, if any. Only applicable for machine images.
--
-- * 'iKernelId' - The kernel associated with the image, if any. Only applicable for machine images.
--
-- * 'iRootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- * 'iSRIOVNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- * 'iName' - The name of the AMI that was provided during image creation.
--
-- * 'iCreationDate' - The date and time the image was created.
--
-- * 'iProductCodes' - Any product codes associated with the AMI.
--
-- * 'iStateReason' - The reason for the state change.
--
-- * 'iDescription' - The description of the AMI that was provided during image creation.
--
-- * 'iBlockDeviceMappings' - Any block device mapping entries.
--
-- * 'iTags' - Any tags assigned to the image.
--
-- * 'iImageId' - The ID of the AMI.
--
-- * 'iImageLocation' - The location of the AMI.
--
-- * 'iState' - The current state of the AMI. If the state is @available@ , the image is successfully registered and can be used to launch an instance.
--
-- * 'iOwnerId' - The AWS account ID of the image owner.
--
-- * 'iPublic' - Indicates whether the image has public launch permissions. The value is @true@ if this image has public launch permissions or @false@ if it has only implicit and explicit launch permissions.
--
-- * 'iArchitecture' - The architecture of the image.
--
-- * 'iImageType' - The type of image.
--
-- * 'iRootDeviceType' - The type of root device used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- * 'iVirtualizationType' - The type of virtualization of the AMI.
--
-- * 'iHypervisor' - The hypervisor type of the image.
image
    :: Text -- ^ 'iImageId'
    -> Text -- ^ 'iImageLocation'
    -> ImageState -- ^ 'iState'
    -> Text -- ^ 'iOwnerId'
    -> Bool -- ^ 'iPublic'
    -> ArchitectureValues -- ^ 'iArchitecture'
    -> ImageTypeValues -- ^ 'iImageType'
    -> DeviceType -- ^ 'iRootDeviceType'
    -> VirtualizationType -- ^ 'iVirtualizationType'
    -> HypervisorType -- ^ 'iHypervisor'
    -> Image
image pImageId_ pImageLocation_ pState_ pOwnerId_ pPublic_ pArchitecture_ pImageType_ pRootDeviceType_ pVirtualizationType_ pHypervisor_ =
  Image'
    { _iPlatform = Nothing
    , _iEnaSupport = Nothing
    , _iImageOwnerAlias = Nothing
    , _iRAMDiskId = Nothing
    , _iKernelId = Nothing
    , _iRootDeviceName = Nothing
    , _iSRIOVNetSupport = Nothing
    , _iName = Nothing
    , _iCreationDate = Nothing
    , _iProductCodes = Nothing
    , _iStateReason = Nothing
    , _iDescription = Nothing
    , _iBlockDeviceMappings = Nothing
    , _iTags = Nothing
    , _iImageId = pImageId_
    , _iImageLocation = pImageLocation_
    , _iState = pState_
    , _iOwnerId = pOwnerId_
    , _iPublic = pPublic_
    , _iArchitecture = pArchitecture_
    , _iImageType = pImageType_
    , _iRootDeviceType = pRootDeviceType_
    , _iVirtualizationType = pVirtualizationType_
    , _iHypervisor = pHypervisor_
    }


-- | The value is @Windows@ for Windows AMIs; otherwise blank.
iPlatform :: Lens' Image (Maybe PlatformValues)
iPlatform = lens _iPlatform (\ s a -> s{_iPlatform = a})

-- | Specifies whether enhanced networking with ENA is enabled.
iEnaSupport :: Lens' Image (Maybe Bool)
iEnaSupport = lens _iEnaSupport (\ s a -> s{_iEnaSupport = a})

-- | The AWS account alias (for example, @amazon@ , @self@ ) or the AWS account ID of the AMI owner.
iImageOwnerAlias :: Lens' Image (Maybe Text)
iImageOwnerAlias = lens _iImageOwnerAlias (\ s a -> s{_iImageOwnerAlias = a})

-- | The RAM disk associated with the image, if any. Only applicable for machine images.
iRAMDiskId :: Lens' Image (Maybe Text)
iRAMDiskId = lens _iRAMDiskId (\ s a -> s{_iRAMDiskId = a})

-- | The kernel associated with the image, if any. Only applicable for machine images.
iKernelId :: Lens' Image (Maybe Text)
iKernelId = lens _iKernelId (\ s a -> s{_iKernelId = a})

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
iRootDeviceName :: Lens' Image (Maybe Text)
iRootDeviceName = lens _iRootDeviceName (\ s a -> s{_iRootDeviceName = a})

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
iSRIOVNetSupport :: Lens' Image (Maybe Text)
iSRIOVNetSupport = lens _iSRIOVNetSupport (\ s a -> s{_iSRIOVNetSupport = a})

-- | The name of the AMI that was provided during image creation.
iName :: Lens' Image (Maybe Text)
iName = lens _iName (\ s a -> s{_iName = a})

-- | The date and time the image was created.
iCreationDate :: Lens' Image (Maybe Text)
iCreationDate = lens _iCreationDate (\ s a -> s{_iCreationDate = a})

-- | Any product codes associated with the AMI.
iProductCodes :: Lens' Image [ProductCode]
iProductCodes = lens _iProductCodes (\ s a -> s{_iProductCodes = a}) . _Default . _Coerce

-- | The reason for the state change.
iStateReason :: Lens' Image (Maybe StateReason)
iStateReason = lens _iStateReason (\ s a -> s{_iStateReason = a})

-- | The description of the AMI that was provided during image creation.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\ s a -> s{_iDescription = a})

-- | Any block device mapping entries.
iBlockDeviceMappings :: Lens' Image [BlockDeviceMapping]
iBlockDeviceMappings = lens _iBlockDeviceMappings (\ s a -> s{_iBlockDeviceMappings = a}) . _Default . _Coerce

-- | Any tags assigned to the image.
iTags :: Lens' Image [Tag]
iTags = lens _iTags (\ s a -> s{_iTags = a}) . _Default . _Coerce

-- | The ID of the AMI.
iImageId :: Lens' Image Text
iImageId = lens _iImageId (\ s a -> s{_iImageId = a})

-- | The location of the AMI.
iImageLocation :: Lens' Image Text
iImageLocation = lens _iImageLocation (\ s a -> s{_iImageLocation = a})

-- | The current state of the AMI. If the state is @available@ , the image is successfully registered and can be used to launch an instance.
iState :: Lens' Image ImageState
iState = lens _iState (\ s a -> s{_iState = a})

-- | The AWS account ID of the image owner.
iOwnerId :: Lens' Image Text
iOwnerId = lens _iOwnerId (\ s a -> s{_iOwnerId = a})

-- | Indicates whether the image has public launch permissions. The value is @true@ if this image has public launch permissions or @false@ if it has only implicit and explicit launch permissions.
iPublic :: Lens' Image Bool
iPublic = lens _iPublic (\ s a -> s{_iPublic = a})

-- | The architecture of the image.
iArchitecture :: Lens' Image ArchitectureValues
iArchitecture = lens _iArchitecture (\ s a -> s{_iArchitecture = a})

-- | The type of image.
iImageType :: Lens' Image ImageTypeValues
iImageType = lens _iImageType (\ s a -> s{_iImageType = a})

-- | The type of root device used by the AMI. The AMI can use an EBS volume or an instance store volume.
iRootDeviceType :: Lens' Image DeviceType
iRootDeviceType = lens _iRootDeviceType (\ s a -> s{_iRootDeviceType = a})

-- | The type of virtualization of the AMI.
iVirtualizationType :: Lens' Image VirtualizationType
iVirtualizationType = lens _iVirtualizationType (\ s a -> s{_iVirtualizationType = a})

-- | The hypervisor type of the image.
iHypervisor :: Lens' Image HypervisorType
iHypervisor = lens _iHypervisor (\ s a -> s{_iHypervisor = a})

instance FromXML Image where
        parseXML x
          = Image' <$>
              (x .@? "platform") <*> (x .@? "enaSupport") <*>
                (x .@? "imageOwnerAlias")
                <*> (x .@? "ramdiskId")
                <*> (x .@? "kernelId")
                <*> (x .@? "rootDeviceName")
                <*> (x .@? "sriovNetSupport")
                <*> (x .@? "name")
                <*> (x .@? "creationDate")
                <*>
                (x .@? "productCodes" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "stateReason")
                <*> (x .@? "description")
                <*>
                (x .@? "blockDeviceMapping" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "imageId")
                <*> (x .@ "imageLocation")
                <*> (x .@ "imageState")
                <*> (x .@ "imageOwnerId")
                <*> (x .@ "isPublic")
                <*> (x .@ "architecture")
                <*> (x .@ "imageType")
                <*> (x .@ "rootDeviceType")
                <*> (x .@ "virtualizationType")
                <*> (x .@ "hypervisor")

instance NFData Image where

-- | Describes the disk container object for an import image task.
--
--
--
-- /See:/ 'imageDiskContainer' smart constructor.
data ImageDiskContainer = ImageDiskContainer'
  { _idcFormat      :: !(Maybe Text)
  , _idcURL         :: !(Maybe Text)
  , _idcDeviceName  :: !(Maybe Text)
  , _idcUserBucket  :: !(Maybe UserBucket)
  , _idcDescription :: !(Maybe Text)
  , _idcSnapshotId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageDiskContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcFormat' - The format of the disk image being imported. Valid values: @VHD@ | @VMDK@ | @OVA@
--
-- * 'idcURL' - The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
--
-- * 'idcDeviceName' - The block device mapping for the disk.
--
-- * 'idcUserBucket' - The S3 bucket for the disk image.
--
-- * 'idcDescription' - The description of the disk image.
--
-- * 'idcSnapshotId' - The ID of the EBS snapshot to be used for importing the snapshot.
imageDiskContainer
    :: ImageDiskContainer
imageDiskContainer =
  ImageDiskContainer'
    { _idcFormat = Nothing
    , _idcURL = Nothing
    , _idcDeviceName = Nothing
    , _idcUserBucket = Nothing
    , _idcDescription = Nothing
    , _idcSnapshotId = Nothing
    }


-- | The format of the disk image being imported. Valid values: @VHD@ | @VMDK@ | @OVA@
idcFormat :: Lens' ImageDiskContainer (Maybe Text)
idcFormat = lens _idcFormat (\ s a -> s{_idcFormat = a})

-- | The URL to the Amazon S3-based disk image being imported. The URL can either be a https URL (https://..) or an Amazon S3 URL (s3://..)
idcURL :: Lens' ImageDiskContainer (Maybe Text)
idcURL = lens _idcURL (\ s a -> s{_idcURL = a})

-- | The block device mapping for the disk.
idcDeviceName :: Lens' ImageDiskContainer (Maybe Text)
idcDeviceName = lens _idcDeviceName (\ s a -> s{_idcDeviceName = a})

-- | The S3 bucket for the disk image.
idcUserBucket :: Lens' ImageDiskContainer (Maybe UserBucket)
idcUserBucket = lens _idcUserBucket (\ s a -> s{_idcUserBucket = a})

-- | The description of the disk image.
idcDescription :: Lens' ImageDiskContainer (Maybe Text)
idcDescription = lens _idcDescription (\ s a -> s{_idcDescription = a})

-- | The ID of the EBS snapshot to be used for importing the snapshot.
idcSnapshotId :: Lens' ImageDiskContainer (Maybe Text)
idcSnapshotId = lens _idcSnapshotId (\ s a -> s{_idcSnapshotId = a})

instance NFData ImageDiskContainer where

instance ToQuery ImageDiskContainer where
        toQuery ImageDiskContainer'{..}
          = mconcat
              ["Format" =: _idcFormat, "Url" =: _idcURL,
               "DeviceName" =: _idcDeviceName,
               "UserBucket" =: _idcUserBucket,
               "Description" =: _idcDescription,
               "SnapshotId" =: _idcSnapshotId]

-- | Describes an import image task.
--
--
--
-- /See:/ 'importImageTask' smart constructor.
data ImportImageTask = ImportImageTask'
  { _iitStatus          :: !(Maybe Text)
  , _iitHypervisor      :: !(Maybe Text)
  , _iitPlatform        :: !(Maybe Text)
  , _iitProgress        :: !(Maybe Text)
  , _iitLicenseType     :: !(Maybe Text)
  , _iitSnapshotDetails :: !(Maybe [SnapshotDetail])
  , _iitStatusMessage   :: !(Maybe Text)
  , _iitImageId         :: !(Maybe Text)
  , _iitImportTaskId    :: !(Maybe Text)
  , _iitArchitecture    :: !(Maybe Text)
  , _iitDescription     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportImageTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iitStatus' - A brief status for the import image task.
--
-- * 'iitHypervisor' - The target hypervisor for the import task. Valid values: @xen@
--
-- * 'iitPlatform' - The description string for the import image task.
--
-- * 'iitProgress' - The percentage of progress of the import image task.
--
-- * 'iitLicenseType' - The license type of the virtual machine.
--
-- * 'iitSnapshotDetails' - Information about the snapshots.
--
-- * 'iitStatusMessage' - A descriptive status message for the import image task.
--
-- * 'iitImageId' - The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
--
-- * 'iitImportTaskId' - The ID of the import image task.
--
-- * 'iitArchitecture' - The architecture of the virtual machine. Valid values: @i386@ | @x86_64@
--
-- * 'iitDescription' - A description of the import task.
importImageTask
    :: ImportImageTask
importImageTask =
  ImportImageTask'
    { _iitStatus = Nothing
    , _iitHypervisor = Nothing
    , _iitPlatform = Nothing
    , _iitProgress = Nothing
    , _iitLicenseType = Nothing
    , _iitSnapshotDetails = Nothing
    , _iitStatusMessage = Nothing
    , _iitImageId = Nothing
    , _iitImportTaskId = Nothing
    , _iitArchitecture = Nothing
    , _iitDescription = Nothing
    }


-- | A brief status for the import image task.
iitStatus :: Lens' ImportImageTask (Maybe Text)
iitStatus = lens _iitStatus (\ s a -> s{_iitStatus = a})

-- | The target hypervisor for the import task. Valid values: @xen@
iitHypervisor :: Lens' ImportImageTask (Maybe Text)
iitHypervisor = lens _iitHypervisor (\ s a -> s{_iitHypervisor = a})

-- | The description string for the import image task.
iitPlatform :: Lens' ImportImageTask (Maybe Text)
iitPlatform = lens _iitPlatform (\ s a -> s{_iitPlatform = a})

-- | The percentage of progress of the import image task.
iitProgress :: Lens' ImportImageTask (Maybe Text)
iitProgress = lens _iitProgress (\ s a -> s{_iitProgress = a})

-- | The license type of the virtual machine.
iitLicenseType :: Lens' ImportImageTask (Maybe Text)
iitLicenseType = lens _iitLicenseType (\ s a -> s{_iitLicenseType = a})

-- | Information about the snapshots.
iitSnapshotDetails :: Lens' ImportImageTask [SnapshotDetail]
iitSnapshotDetails = lens _iitSnapshotDetails (\ s a -> s{_iitSnapshotDetails = a}) . _Default . _Coerce

-- | A descriptive status message for the import image task.
iitStatusMessage :: Lens' ImportImageTask (Maybe Text)
iitStatusMessage = lens _iitStatusMessage (\ s a -> s{_iitStatusMessage = a})

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
iitImageId :: Lens' ImportImageTask (Maybe Text)
iitImageId = lens _iitImageId (\ s a -> s{_iitImageId = a})

-- | The ID of the import image task.
iitImportTaskId :: Lens' ImportImageTask (Maybe Text)
iitImportTaskId = lens _iitImportTaskId (\ s a -> s{_iitImportTaskId = a})

-- | The architecture of the virtual machine. Valid values: @i386@ | @x86_64@
iitArchitecture :: Lens' ImportImageTask (Maybe Text)
iitArchitecture = lens _iitArchitecture (\ s a -> s{_iitArchitecture = a})

-- | A description of the import task.
iitDescription :: Lens' ImportImageTask (Maybe Text)
iitDescription = lens _iitDescription (\ s a -> s{_iitDescription = a})

instance FromXML ImportImageTask where
        parseXML x
          = ImportImageTask' <$>
              (x .@? "status") <*> (x .@? "hypervisor") <*>
                (x .@? "platform")
                <*> (x .@? "progress")
                <*> (x .@? "licenseType")
                <*>
                (x .@? "snapshotDetailSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "statusMessage")
                <*> (x .@? "imageId")
                <*> (x .@? "importTaskId")
                <*> (x .@? "architecture")
                <*> (x .@? "description")

instance NFData ImportImageTask where

-- | Describes the launch specification for VM import.
--
--
--
-- /See:/ 'importInstanceLaunchSpecification' smart constructor.
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification'
  { _iilsAdditionalInfo                    :: !(Maybe Text)
  , _iilsGroupNames                        :: !(Maybe [Text])
  , _iilsSubnetId                          :: !(Maybe Text)
  , _iilsInstanceType                      :: !(Maybe InstanceType)
  , _iilsGroupIds                          :: !(Maybe [Text])
  , _iilsUserData                          :: !(Maybe UserData)
  , _iilsMonitoring                        :: !(Maybe Bool)
  , _iilsPrivateIPAddress                  :: !(Maybe Text)
  , _iilsInstanceInitiatedShutdownBehavior :: !(Maybe ShutdownBehavior)
  , _iilsArchitecture                      :: !(Maybe ArchitectureValues)
  , _iilsPlacement                         :: !(Maybe Placement)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportInstanceLaunchSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iilsAdditionalInfo' - Reserved.
--
-- * 'iilsGroupNames' - One or more security group names.
--
-- * 'iilsSubnetId' - [EC2-VPC] The ID of the subnet in which to launch the instance.
--
-- * 'iilsInstanceType' - The instance type. For more information about the instance types that you can import, see <http://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types> in the VM Import/Export User Guide.
--
-- * 'iilsGroupIds' - One or more security group IDs.
--
-- * 'iilsUserData' - The Base64-encoded user data to make available to the instance.
--
-- * 'iilsMonitoring' - Indicates whether monitoring is enabled.
--
-- * 'iilsPrivateIPAddress' - [EC2-VPC] An available IP address from the IP address range of the subnet.
--
-- * 'iilsInstanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- * 'iilsArchitecture' - The architecture of the instance.
--
-- * 'iilsPlacement' - The placement information for the instance.
importInstanceLaunchSpecification
    :: ImportInstanceLaunchSpecification
importInstanceLaunchSpecification =
  ImportInstanceLaunchSpecification'
    { _iilsAdditionalInfo = Nothing
    , _iilsGroupNames = Nothing
    , _iilsSubnetId = Nothing
    , _iilsInstanceType = Nothing
    , _iilsGroupIds = Nothing
    , _iilsUserData = Nothing
    , _iilsMonitoring = Nothing
    , _iilsPrivateIPAddress = Nothing
    , _iilsInstanceInitiatedShutdownBehavior = Nothing
    , _iilsArchitecture = Nothing
    , _iilsPlacement = Nothing
    }


-- | Reserved.
iilsAdditionalInfo :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsAdditionalInfo = lens _iilsAdditionalInfo (\ s a -> s{_iilsAdditionalInfo = a})

-- | One or more security group names.
iilsGroupNames :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupNames = lens _iilsGroupNames (\ s a -> s{_iilsGroupNames = a}) . _Default . _Coerce

-- | [EC2-VPC] The ID of the subnet in which to launch the instance.
iilsSubnetId :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsSubnetId = lens _iilsSubnetId (\ s a -> s{_iilsSubnetId = a})

-- | The instance type. For more information about the instance types that you can import, see <http://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types> in the VM Import/Export User Guide.
iilsInstanceType :: Lens' ImportInstanceLaunchSpecification (Maybe InstanceType)
iilsInstanceType = lens _iilsInstanceType (\ s a -> s{_iilsInstanceType = a})

-- | One or more security group IDs.
iilsGroupIds :: Lens' ImportInstanceLaunchSpecification [Text]
iilsGroupIds = lens _iilsGroupIds (\ s a -> s{_iilsGroupIds = a}) . _Default . _Coerce

-- | The Base64-encoded user data to make available to the instance.
iilsUserData :: Lens' ImportInstanceLaunchSpecification (Maybe UserData)
iilsUserData = lens _iilsUserData (\ s a -> s{_iilsUserData = a})

-- | Indicates whether monitoring is enabled.
iilsMonitoring :: Lens' ImportInstanceLaunchSpecification (Maybe Bool)
iilsMonitoring = lens _iilsMonitoring (\ s a -> s{_iilsMonitoring = a})

-- | [EC2-VPC] An available IP address from the IP address range of the subnet.
iilsPrivateIPAddress :: Lens' ImportInstanceLaunchSpecification (Maybe Text)
iilsPrivateIPAddress = lens _iilsPrivateIPAddress (\ s a -> s{_iilsPrivateIPAddress = a})

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
iilsInstanceInitiatedShutdownBehavior :: Lens' ImportInstanceLaunchSpecification (Maybe ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior = lens _iilsInstanceInitiatedShutdownBehavior (\ s a -> s{_iilsInstanceInitiatedShutdownBehavior = a})

-- | The architecture of the instance.
iilsArchitecture :: Lens' ImportInstanceLaunchSpecification (Maybe ArchitectureValues)
iilsArchitecture = lens _iilsArchitecture (\ s a -> s{_iilsArchitecture = a})

-- | The placement information for the instance.
iilsPlacement :: Lens' ImportInstanceLaunchSpecification (Maybe Placement)
iilsPlacement = lens _iilsPlacement (\ s a -> s{_iilsPlacement = a})

instance NFData ImportInstanceLaunchSpecification
         where

instance ToQuery ImportInstanceLaunchSpecification
         where
        toQuery ImportInstanceLaunchSpecification'{..}
          = mconcat
              ["AdditionalInfo" =: _iilsAdditionalInfo,
               toQuery
                 (toQueryList "GroupName" <$> _iilsGroupNames),
               "SubnetId" =: _iilsSubnetId,
               "InstanceType" =: _iilsInstanceType,
               toQuery (toQueryList "GroupId" <$> _iilsGroupIds),
               "UserData" =: _iilsUserData,
               "Monitoring" =: _iilsMonitoring,
               "PrivateIpAddress" =: _iilsPrivateIPAddress,
               "InstanceInitiatedShutdownBehavior" =:
                 _iilsInstanceInitiatedShutdownBehavior,
               "Architecture" =: _iilsArchitecture,
               "Placement" =: _iilsPlacement]

-- | Describes an import instance task.
--
--
--
-- /See:/ 'importInstanceTaskDetails' smart constructor.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails'
  { _iitdInstanceId  :: !(Maybe Text)
  , _iitdPlatform    :: !(Maybe PlatformValues)
  , _iitdVolumes     :: !(Maybe [ImportInstanceVolumeDetailItem])
  , _iitdDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportInstanceTaskDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iitdInstanceId' - The ID of the instance.
--
-- * 'iitdPlatform' - The instance operating system.
--
-- * 'iitdVolumes' - One or more volumes.
--
-- * 'iitdDescription' - A description of the task.
importInstanceTaskDetails
    :: ImportInstanceTaskDetails
importInstanceTaskDetails =
  ImportInstanceTaskDetails'
    { _iitdInstanceId = Nothing
    , _iitdPlatform = Nothing
    , _iitdVolumes = Nothing
    , _iitdDescription = Nothing
    }


-- | The ID of the instance.
iitdInstanceId :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdInstanceId = lens _iitdInstanceId (\ s a -> s{_iitdInstanceId = a})

-- | The instance operating system.
iitdPlatform :: Lens' ImportInstanceTaskDetails (Maybe PlatformValues)
iitdPlatform = lens _iitdPlatform (\ s a -> s{_iitdPlatform = a})

-- | One or more volumes.
iitdVolumes :: Lens' ImportInstanceTaskDetails [ImportInstanceVolumeDetailItem]
iitdVolumes = lens _iitdVolumes (\ s a -> s{_iitdVolumes = a}) . _Default . _Coerce

-- | A description of the task.
iitdDescription :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdDescription = lens _iitdDescription (\ s a -> s{_iitdDescription = a})

instance FromXML ImportInstanceTaskDetails where
        parseXML x
          = ImportInstanceTaskDetails' <$>
              (x .@? "instanceId") <*> (x .@? "platform") <*>
                (x .@? "volumes" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "description")

instance NFData ImportInstanceTaskDetails where

-- | Describes an import volume task.
--
--
--
-- /See:/ 'importInstanceVolumeDetailItem' smart constructor.
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem'
  { _iivdiStatusMessage    :: !(Maybe Text)
  , _iivdiDescription      :: !(Maybe Text)
  , _iivdiAvailabilityZone :: !Text
  , _iivdiBytesConverted   :: !Integer
  , _iivdiImage            :: !DiskImageDescription
  , _iivdiStatus           :: !Text
  , _iivdiVolume           :: !DiskImageVolumeDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportInstanceVolumeDetailItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iivdiStatusMessage' - The status information or errors related to the disk image.
--
-- * 'iivdiDescription' - A description of the task.
--
-- * 'iivdiAvailabilityZone' - The Availability Zone where the resulting instance will reside.
--
-- * 'iivdiBytesConverted' - The number of bytes converted so far.
--
-- * 'iivdiImage' - The image.
--
-- * 'iivdiStatus' - The status of the import of this particular disk image.
--
-- * 'iivdiVolume' - The volume.
importInstanceVolumeDetailItem
    :: Text -- ^ 'iivdiAvailabilityZone'
    -> Integer -- ^ 'iivdiBytesConverted'
    -> DiskImageDescription -- ^ 'iivdiImage'
    -> Text -- ^ 'iivdiStatus'
    -> DiskImageVolumeDescription -- ^ 'iivdiVolume'
    -> ImportInstanceVolumeDetailItem
importInstanceVolumeDetailItem pAvailabilityZone_ pBytesConverted_ pImage_ pStatus_ pVolume_ =
  ImportInstanceVolumeDetailItem'
    { _iivdiStatusMessage = Nothing
    , _iivdiDescription = Nothing
    , _iivdiAvailabilityZone = pAvailabilityZone_
    , _iivdiBytesConverted = pBytesConverted_
    , _iivdiImage = pImage_
    , _iivdiStatus = pStatus_
    , _iivdiVolume = pVolume_
    }


-- | The status information or errors related to the disk image.
iivdiStatusMessage :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiStatusMessage = lens _iivdiStatusMessage (\ s a -> s{_iivdiStatusMessage = a})

-- | A description of the task.
iivdiDescription :: Lens' ImportInstanceVolumeDetailItem (Maybe Text)
iivdiDescription = lens _iivdiDescription (\ s a -> s{_iivdiDescription = a})

-- | The Availability Zone where the resulting instance will reside.
iivdiAvailabilityZone :: Lens' ImportInstanceVolumeDetailItem Text
iivdiAvailabilityZone = lens _iivdiAvailabilityZone (\ s a -> s{_iivdiAvailabilityZone = a})

-- | The number of bytes converted so far.
iivdiBytesConverted :: Lens' ImportInstanceVolumeDetailItem Integer
iivdiBytesConverted = lens _iivdiBytesConverted (\ s a -> s{_iivdiBytesConverted = a})

-- | The image.
iivdiImage :: Lens' ImportInstanceVolumeDetailItem DiskImageDescription
iivdiImage = lens _iivdiImage (\ s a -> s{_iivdiImage = a})

-- | The status of the import of this particular disk image.
iivdiStatus :: Lens' ImportInstanceVolumeDetailItem Text
iivdiStatus = lens _iivdiStatus (\ s a -> s{_iivdiStatus = a})

-- | The volume.
iivdiVolume :: Lens' ImportInstanceVolumeDetailItem DiskImageVolumeDescription
iivdiVolume = lens _iivdiVolume (\ s a -> s{_iivdiVolume = a})

instance FromXML ImportInstanceVolumeDetailItem where
        parseXML x
          = ImportInstanceVolumeDetailItem' <$>
              (x .@? "statusMessage") <*> (x .@? "description") <*>
                (x .@ "availabilityZone")
                <*> (x .@ "bytesConverted")
                <*> (x .@ "image")
                <*> (x .@ "status")
                <*> (x .@ "volume")

instance NFData ImportInstanceVolumeDetailItem where

-- | Describes an import snapshot task.
--
--
--
-- /See:/ 'importSnapshotTask' smart constructor.
data ImportSnapshotTask = ImportSnapshotTask'
  { _istSnapshotTaskDetail :: !(Maybe SnapshotTaskDetail)
  , _istImportTaskId       :: !(Maybe Text)
  , _istDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportSnapshotTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'istSnapshotTaskDetail' - Describes an import snapshot task.
--
-- * 'istImportTaskId' - The ID of the import snapshot task.
--
-- * 'istDescription' - A description of the import snapshot task.
importSnapshotTask
    :: ImportSnapshotTask
importSnapshotTask =
  ImportSnapshotTask'
    { _istSnapshotTaskDetail = Nothing
    , _istImportTaskId = Nothing
    , _istDescription = Nothing
    }


-- | Describes an import snapshot task.
istSnapshotTaskDetail :: Lens' ImportSnapshotTask (Maybe SnapshotTaskDetail)
istSnapshotTaskDetail = lens _istSnapshotTaskDetail (\ s a -> s{_istSnapshotTaskDetail = a})

-- | The ID of the import snapshot task.
istImportTaskId :: Lens' ImportSnapshotTask (Maybe Text)
istImportTaskId = lens _istImportTaskId (\ s a -> s{_istImportTaskId = a})

-- | A description of the import snapshot task.
istDescription :: Lens' ImportSnapshotTask (Maybe Text)
istDescription = lens _istDescription (\ s a -> s{_istDescription = a})

instance FromXML ImportSnapshotTask where
        parseXML x
          = ImportSnapshotTask' <$>
              (x .@? "snapshotTaskDetail") <*>
                (x .@? "importTaskId")
                <*> (x .@? "description")

instance NFData ImportSnapshotTask where

-- | Describes an import volume task.
--
--
--
-- /See:/ 'importVolumeTaskDetails' smart constructor.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails'
  { _ivtdBytesConverted   :: !(Maybe Integer)
  , _ivtdImage            :: !(Maybe DiskImageDescription)
  , _ivtdVolume           :: !(Maybe DiskImageVolumeDescription)
  , _ivtdAvailabilityZone :: !(Maybe Text)
  , _ivtdDescription      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportVolumeTaskDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivtdBytesConverted' - The number of bytes converted so far.
--
-- * 'ivtdImage' - The image.
--
-- * 'ivtdVolume' - The volume.
--
-- * 'ivtdAvailabilityZone' - The Availability Zone where the resulting volume will reside.
--
-- * 'ivtdDescription' - The description you provided when starting the import volume task.
importVolumeTaskDetails
    :: ImportVolumeTaskDetails
importVolumeTaskDetails =
  ImportVolumeTaskDetails'
    { _ivtdBytesConverted = Nothing
    , _ivtdImage = Nothing
    , _ivtdVolume = Nothing
    , _ivtdAvailabilityZone = Nothing
    , _ivtdDescription = Nothing
    }


-- | The number of bytes converted so far.
ivtdBytesConverted :: Lens' ImportVolumeTaskDetails (Maybe Integer)
ivtdBytesConverted = lens _ivtdBytesConverted (\ s a -> s{_ivtdBytesConverted = a})

-- | The image.
ivtdImage :: Lens' ImportVolumeTaskDetails (Maybe DiskImageDescription)
ivtdImage = lens _ivtdImage (\ s a -> s{_ivtdImage = a})

-- | The volume.
ivtdVolume :: Lens' ImportVolumeTaskDetails (Maybe DiskImageVolumeDescription)
ivtdVolume = lens _ivtdVolume (\ s a -> s{_ivtdVolume = a})

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdAvailabilityZone = lens _ivtdAvailabilityZone (\ s a -> s{_ivtdAvailabilityZone = a})

-- | The description you provided when starting the import volume task.
ivtdDescription :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdDescription = lens _ivtdDescription (\ s a -> s{_ivtdDescription = a})

instance FromXML ImportVolumeTaskDetails where
        parseXML x
          = ImportVolumeTaskDetails' <$>
              (x .@? "bytesConverted") <*> (x .@? "image") <*>
                (x .@? "volume")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "description")

instance NFData ImportVolumeTaskDetails where

-- | Describes an instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _insPublicDNSName          :: !(Maybe Text)
  , _insPlatform               :: !(Maybe PlatformValues)
  , _insSecurityGroups         :: !(Maybe [GroupIdentifier])
  , _insClientToken            :: !(Maybe Text)
  , _insEnaSupport             :: !(Maybe Bool)
  , _insSourceDestCheck        :: !(Maybe Bool)
  , _insElasticGpuAssociations :: !(Maybe [ElasticGpuAssociation])
  , _insVPCId                  :: !(Maybe Text)
  , _insKeyName                :: !(Maybe Text)
  , _insNetworkInterfaces      :: !(Maybe [InstanceNetworkInterface])
  , _insRAMDiskId              :: !(Maybe Text)
  , _insCPUOptions             :: !(Maybe CPUOptions)
  , _insSubnetId               :: !(Maybe Text)
  , _insKernelId               :: !(Maybe Text)
  , _insRootDeviceName         :: !(Maybe Text)
  , _insSRIOVNetSupport        :: !(Maybe Text)
  , _insEBSOptimized           :: !(Maybe Bool)
  , _insStateTransitionReason  :: !(Maybe Text)
  , _insInstanceLifecycle      :: !(Maybe InstanceLifecycleType)
  , _insIAMInstanceProfile     :: !(Maybe IAMInstanceProfile)
  , _insPrivateIPAddress       :: !(Maybe Text)
  , _insProductCodes           :: !(Maybe [ProductCode])
  , _insSpotInstanceRequestId  :: !(Maybe Text)
  , _insPrivateDNSName         :: !(Maybe Text)
  , _insStateReason            :: !(Maybe StateReason)
  , _insBlockDeviceMappings    :: !(Maybe [InstanceBlockDeviceMapping])
  , _insPublicIPAddress        :: !(Maybe Text)
  , _insTags                   :: !(Maybe [Tag])
  , _insInstanceId             :: !Text
  , _insImageId                :: !Text
  , _insAMILaunchIndex         :: !Int
  , _insInstanceType           :: !InstanceType
  , _insLaunchTime             :: !ISO8601
  , _insPlacement              :: !Placement
  , _insMonitoring             :: !Monitoring
  , _insArchitecture           :: !ArchitectureValues
  , _insRootDeviceType         :: !DeviceType
  , _insVirtualizationType     :: !VirtualizationType
  , _insHypervisor             :: !HypervisorType
  , _insState                  :: !InstanceState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'insPublicDNSName' - (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
--
-- * 'insPlatform' - The value is @Windows@ for Windows instances; otherwise blank.
--
-- * 'insSecurityGroups' - One or more security groups for the instance.
--
-- * 'insClientToken' - The idempotency token you provided when you launched the instance, if applicable.
--
-- * 'insEnaSupport' - Specifies whether enhanced networking with ENA is enabled.
--
-- * 'insSourceDestCheck' - Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- * 'insElasticGpuAssociations' - The Elastic GPU associated with the instance.
--
-- * 'insVPCId' - [EC2-VPC] The ID of the VPC in which the instance is running.
--
-- * 'insKeyName' - The name of the key pair, if this instance was launched with an associated key pair.
--
-- * 'insNetworkInterfaces' - [EC2-VPC] One or more network interfaces for the instance.
--
-- * 'insRAMDiskId' - The RAM disk associated with this instance, if applicable.
--
-- * 'insCPUOptions' - The CPU options for the instance.
--
-- * 'insSubnetId' - [EC2-VPC] The ID of the subnet in which the instance is running.
--
-- * 'insKernelId' - The kernel associated with this instance, if applicable.
--
-- * 'insRootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- * 'insSRIOVNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- * 'insEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- * 'insStateTransitionReason' - The reason for the most recent state transition. This might be an empty string.
--
-- * 'insInstanceLifecycle' - Indicates whether this is a Spot Instance or a Scheduled Instance.
--
-- * 'insIAMInstanceProfile' - The IAM instance profile associated with the instance, if applicable.
--
-- * 'insPrivateIPAddress' - The private IPv4 address assigned to the instance.
--
-- * 'insProductCodes' - The product codes attached to this instance, if applicable.
--
-- * 'insSpotInstanceRequestId' - If the request is a Spot Instance request, the ID of the request.
--
-- * 'insPrivateDNSName' - (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.  [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
--
-- * 'insStateReason' - The reason for the most recent state transition.
--
-- * 'insBlockDeviceMappings' - Any block device mapping entries for the instance.
--
-- * 'insPublicIPAddress' - The public IPv4 address assigned to the instance, if applicable.
--
-- * 'insTags' - Any tags assigned to the instance.
--
-- * 'insInstanceId' - The ID of the instance.
--
-- * 'insImageId' - The ID of the AMI used to launch the instance.
--
-- * 'insAMILaunchIndex' - The AMI launch index, which can be used to find this instance in the launch group.
--
-- * 'insInstanceType' - The instance type.
--
-- * 'insLaunchTime' - The time the instance was launched.
--
-- * 'insPlacement' - The location where the instance launched, if applicable.
--
-- * 'insMonitoring' - The monitoring for the instance.
--
-- * 'insArchitecture' - The architecture of the image.
--
-- * 'insRootDeviceType' - The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- * 'insVirtualizationType' - The virtualization type of the instance.
--
-- * 'insHypervisor' - The hypervisor type of the instance.
--
-- * 'insState' - The current state of the instance.
instance'
    :: Text -- ^ 'insInstanceId'
    -> Text -- ^ 'insImageId'
    -> Int -- ^ 'insAMILaunchIndex'
    -> InstanceType -- ^ 'insInstanceType'
    -> UTCTime -- ^ 'insLaunchTime'
    -> Placement -- ^ 'insPlacement'
    -> Monitoring -- ^ 'insMonitoring'
    -> ArchitectureValues -- ^ 'insArchitecture'
    -> DeviceType -- ^ 'insRootDeviceType'
    -> VirtualizationType -- ^ 'insVirtualizationType'
    -> HypervisorType -- ^ 'insHypervisor'
    -> InstanceState -- ^ 'insState'
    -> Instance
instance' pInstanceId_ pImageId_ pAMILaunchIndex_ pInstanceType_ pLaunchTime_ pPlacement_ pMonitoring_ pArchitecture_ pRootDeviceType_ pVirtualizationType_ pHypervisor_ pState_ =
  Instance'
    { _insPublicDNSName = Nothing
    , _insPlatform = Nothing
    , _insSecurityGroups = Nothing
    , _insClientToken = Nothing
    , _insEnaSupport = Nothing
    , _insSourceDestCheck = Nothing
    , _insElasticGpuAssociations = Nothing
    , _insVPCId = Nothing
    , _insKeyName = Nothing
    , _insNetworkInterfaces = Nothing
    , _insRAMDiskId = Nothing
    , _insCPUOptions = Nothing
    , _insSubnetId = Nothing
    , _insKernelId = Nothing
    , _insRootDeviceName = Nothing
    , _insSRIOVNetSupport = Nothing
    , _insEBSOptimized = Nothing
    , _insStateTransitionReason = Nothing
    , _insInstanceLifecycle = Nothing
    , _insIAMInstanceProfile = Nothing
    , _insPrivateIPAddress = Nothing
    , _insProductCodes = Nothing
    , _insSpotInstanceRequestId = Nothing
    , _insPrivateDNSName = Nothing
    , _insStateReason = Nothing
    , _insBlockDeviceMappings = Nothing
    , _insPublicIPAddress = Nothing
    , _insTags = Nothing
    , _insInstanceId = pInstanceId_
    , _insImageId = pImageId_
    , _insAMILaunchIndex = pAMILaunchIndex_
    , _insInstanceType = pInstanceType_
    , _insLaunchTime = _Time # pLaunchTime_
    , _insPlacement = pPlacement_
    , _insMonitoring = pMonitoring_
    , _insArchitecture = pArchitecture_
    , _insRootDeviceType = pRootDeviceType_
    , _insVirtualizationType = pVirtualizationType_
    , _insHypervisor = pHypervisor_
    , _insState = pState_
    }


-- | (IPv4 only) The public DNS name assigned to the instance. This name is not available until the instance enters the @running@ state. For EC2-VPC, this name is only available if you've enabled DNS hostnames for your VPC.
insPublicDNSName :: Lens' Instance (Maybe Text)
insPublicDNSName = lens _insPublicDNSName (\ s a -> s{_insPublicDNSName = a})

-- | The value is @Windows@ for Windows instances; otherwise blank.
insPlatform :: Lens' Instance (Maybe PlatformValues)
insPlatform = lens _insPlatform (\ s a -> s{_insPlatform = a})

-- | One or more security groups for the instance.
insSecurityGroups :: Lens' Instance [GroupIdentifier]
insSecurityGroups = lens _insSecurityGroups (\ s a -> s{_insSecurityGroups = a}) . _Default . _Coerce

-- | The idempotency token you provided when you launched the instance, if applicable.
insClientToken :: Lens' Instance (Maybe Text)
insClientToken = lens _insClientToken (\ s a -> s{_insClientToken = a})

-- | Specifies whether enhanced networking with ENA is enabled.
insEnaSupport :: Lens' Instance (Maybe Bool)
insEnaSupport = lens _insEnaSupport (\ s a -> s{_insEnaSupport = a})

-- | Specifies whether to enable an instance launched in a VPC to perform NAT. This controls whether source/destination checking is enabled on the instance. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. The value must be @false@ for the instance to perform NAT. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
insSourceDestCheck :: Lens' Instance (Maybe Bool)
insSourceDestCheck = lens _insSourceDestCheck (\ s a -> s{_insSourceDestCheck = a})

-- | The Elastic GPU associated with the instance.
insElasticGpuAssociations :: Lens' Instance [ElasticGpuAssociation]
insElasticGpuAssociations = lens _insElasticGpuAssociations (\ s a -> s{_insElasticGpuAssociations = a}) . _Default . _Coerce

-- | [EC2-VPC] The ID of the VPC in which the instance is running.
insVPCId :: Lens' Instance (Maybe Text)
insVPCId = lens _insVPCId (\ s a -> s{_insVPCId = a})

-- | The name of the key pair, if this instance was launched with an associated key pair.
insKeyName :: Lens' Instance (Maybe Text)
insKeyName = lens _insKeyName (\ s a -> s{_insKeyName = a})

-- | [EC2-VPC] One or more network interfaces for the instance.
insNetworkInterfaces :: Lens' Instance [InstanceNetworkInterface]
insNetworkInterfaces = lens _insNetworkInterfaces (\ s a -> s{_insNetworkInterfaces = a}) . _Default . _Coerce

-- | The RAM disk associated with this instance, if applicable.
insRAMDiskId :: Lens' Instance (Maybe Text)
insRAMDiskId = lens _insRAMDiskId (\ s a -> s{_insRAMDiskId = a})

-- | The CPU options for the instance.
insCPUOptions :: Lens' Instance (Maybe CPUOptions)
insCPUOptions = lens _insCPUOptions (\ s a -> s{_insCPUOptions = a})

-- | [EC2-VPC] The ID of the subnet in which the instance is running.
insSubnetId :: Lens' Instance (Maybe Text)
insSubnetId = lens _insSubnetId (\ s a -> s{_insSubnetId = a})

-- | The kernel associated with this instance, if applicable.
insKernelId :: Lens' Instance (Maybe Text)
insKernelId = lens _insKernelId (\ s a -> s{_insKernelId = a})

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
insRootDeviceName :: Lens' Instance (Maybe Text)
insRootDeviceName = lens _insRootDeviceName (\ s a -> s{_insRootDeviceName = a})

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
insSRIOVNetSupport :: Lens' Instance (Maybe Text)
insSRIOVNetSupport = lens _insSRIOVNetSupport (\ s a -> s{_insSRIOVNetSupport = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
insEBSOptimized :: Lens' Instance (Maybe Bool)
insEBSOptimized = lens _insEBSOptimized (\ s a -> s{_insEBSOptimized = a})

-- | The reason for the most recent state transition. This might be an empty string.
insStateTransitionReason :: Lens' Instance (Maybe Text)
insStateTransitionReason = lens _insStateTransitionReason (\ s a -> s{_insStateTransitionReason = a})

-- | Indicates whether this is a Spot Instance or a Scheduled Instance.
insInstanceLifecycle :: Lens' Instance (Maybe InstanceLifecycleType)
insInstanceLifecycle = lens _insInstanceLifecycle (\ s a -> s{_insInstanceLifecycle = a})

-- | The IAM instance profile associated with the instance, if applicable.
insIAMInstanceProfile :: Lens' Instance (Maybe IAMInstanceProfile)
insIAMInstanceProfile = lens _insIAMInstanceProfile (\ s a -> s{_insIAMInstanceProfile = a})

-- | The private IPv4 address assigned to the instance.
insPrivateIPAddress :: Lens' Instance (Maybe Text)
insPrivateIPAddress = lens _insPrivateIPAddress (\ s a -> s{_insPrivateIPAddress = a})

-- | The product codes attached to this instance, if applicable.
insProductCodes :: Lens' Instance [ProductCode]
insProductCodes = lens _insProductCodes (\ s a -> s{_insProductCodes = a}) . _Default . _Coerce

-- | If the request is a Spot Instance request, the ID of the request.
insSpotInstanceRequestId :: Lens' Instance (Maybe Text)
insSpotInstanceRequestId = lens _insSpotInstanceRequestId (\ s a -> s{_insSpotInstanceRequestId = a})

-- | (IPv4 only) The private DNS hostname name assigned to the instance. This DNS hostname can only be used inside the Amazon EC2 network. This name is not available until the instance enters the @running@ state.  [EC2-VPC] The Amazon-provided DNS server resolves Amazon-provided private DNS hostnames if you've enabled DNS resolution and DNS hostnames in your VPC. If you are not using the Amazon-provided DNS server in your VPC, your custom domain name servers must resolve the hostname as appropriate.
insPrivateDNSName :: Lens' Instance (Maybe Text)
insPrivateDNSName = lens _insPrivateDNSName (\ s a -> s{_insPrivateDNSName = a})

-- | The reason for the most recent state transition.
insStateReason :: Lens' Instance (Maybe StateReason)
insStateReason = lens _insStateReason (\ s a -> s{_insStateReason = a})

-- | Any block device mapping entries for the instance.
insBlockDeviceMappings :: Lens' Instance [InstanceBlockDeviceMapping]
insBlockDeviceMappings = lens _insBlockDeviceMappings (\ s a -> s{_insBlockDeviceMappings = a}) . _Default . _Coerce

-- | The public IPv4 address assigned to the instance, if applicable.
insPublicIPAddress :: Lens' Instance (Maybe Text)
insPublicIPAddress = lens _insPublicIPAddress (\ s a -> s{_insPublicIPAddress = a})

-- | Any tags assigned to the instance.
insTags :: Lens' Instance [Tag]
insTags = lens _insTags (\ s a -> s{_insTags = a}) . _Default . _Coerce

-- | The ID of the instance.
insInstanceId :: Lens' Instance Text
insInstanceId = lens _insInstanceId (\ s a -> s{_insInstanceId = a})

-- | The ID of the AMI used to launch the instance.
insImageId :: Lens' Instance Text
insImageId = lens _insImageId (\ s a -> s{_insImageId = a})

-- | The AMI launch index, which can be used to find this instance in the launch group.
insAMILaunchIndex :: Lens' Instance Int
insAMILaunchIndex = lens _insAMILaunchIndex (\ s a -> s{_insAMILaunchIndex = a})

-- | The instance type.
insInstanceType :: Lens' Instance InstanceType
insInstanceType = lens _insInstanceType (\ s a -> s{_insInstanceType = a})

-- | The time the instance was launched.
insLaunchTime :: Lens' Instance UTCTime
insLaunchTime = lens _insLaunchTime (\ s a -> s{_insLaunchTime = a}) . _Time

-- | The location where the instance launched, if applicable.
insPlacement :: Lens' Instance Placement
insPlacement = lens _insPlacement (\ s a -> s{_insPlacement = a})

-- | The monitoring for the instance.
insMonitoring :: Lens' Instance Monitoring
insMonitoring = lens _insMonitoring (\ s a -> s{_insMonitoring = a})

-- | The architecture of the image.
insArchitecture :: Lens' Instance ArchitectureValues
insArchitecture = lens _insArchitecture (\ s a -> s{_insArchitecture = a})

-- | The root device type used by the AMI. The AMI can use an EBS volume or an instance store volume.
insRootDeviceType :: Lens' Instance DeviceType
insRootDeviceType = lens _insRootDeviceType (\ s a -> s{_insRootDeviceType = a})

-- | The virtualization type of the instance.
insVirtualizationType :: Lens' Instance VirtualizationType
insVirtualizationType = lens _insVirtualizationType (\ s a -> s{_insVirtualizationType = a})

-- | The hypervisor type of the instance.
insHypervisor :: Lens' Instance HypervisorType
insHypervisor = lens _insHypervisor (\ s a -> s{_insHypervisor = a})

-- | The current state of the instance.
insState :: Lens' Instance InstanceState
insState = lens _insState (\ s a -> s{_insState = a})

instance FromXML Instance where
        parseXML x
          = Instance' <$>
              (x .@? "dnsName") <*> (x .@? "platform") <*>
                (x .@? "groupSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "clientToken")
                <*> (x .@? "enaSupport")
                <*> (x .@? "sourceDestCheck")
                <*>
                (x .@? "elasticGpuAssociationSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "vpcId")
                <*> (x .@? "keyName")
                <*>
                (x .@? "networkInterfaceSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "ramdiskId")
                <*> (x .@? "cpuOptions")
                <*> (x .@? "subnetId")
                <*> (x .@? "kernelId")
                <*> (x .@? "rootDeviceName")
                <*> (x .@? "sriovNetSupport")
                <*> (x .@? "ebsOptimized")
                <*> (x .@? "reason")
                <*> (x .@? "instanceLifecycle")
                <*> (x .@? "iamInstanceProfile")
                <*> (x .@? "privateIpAddress")
                <*>
                (x .@? "productCodes" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "spotInstanceRequestId")
                <*> (x .@? "privateDnsName")
                <*> (x .@? "stateReason")
                <*>
                (x .@? "blockDeviceMapping" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "ipAddress")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "instanceId")
                <*> (x .@ "imageId")
                <*> (x .@ "amiLaunchIndex")
                <*> (x .@ "instanceType")
                <*> (x .@ "launchTime")
                <*> (x .@ "placement")
                <*> (x .@ "monitoring")
                <*> (x .@ "architecture")
                <*> (x .@ "rootDeviceType")
                <*> (x .@ "virtualizationType")
                <*> (x .@ "hypervisor")
                <*> (x .@ "instanceState")

instance NFData Instance where

-- | Describes a block device mapping.
--
--
--
-- /See:/ 'instanceBlockDeviceMapping' smart constructor.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping'
  { _ibdmEBS        :: !(Maybe EBSInstanceBlockDevice)
  , _ibdmDeviceName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceBlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibdmEBS' - Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- * 'ibdmDeviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
instanceBlockDeviceMapping
    :: InstanceBlockDeviceMapping
instanceBlockDeviceMapping =
  InstanceBlockDeviceMapping' {_ibdmEBS = Nothing, _ibdmDeviceName = Nothing}


-- | Parameters used to automatically set up EBS volumes when the instance is launched.
ibdmEBS :: Lens' InstanceBlockDeviceMapping (Maybe EBSInstanceBlockDevice)
ibdmEBS = lens _ibdmEBS (\ s a -> s{_ibdmEBS = a})

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
ibdmDeviceName :: Lens' InstanceBlockDeviceMapping (Maybe Text)
ibdmDeviceName = lens _ibdmDeviceName (\ s a -> s{_ibdmDeviceName = a})

instance FromXML InstanceBlockDeviceMapping where
        parseXML x
          = InstanceBlockDeviceMapping' <$>
              (x .@? "ebs") <*> (x .@? "deviceName")

instance NFData InstanceBlockDeviceMapping where

-- | Describes a block device mapping entry.
--
--
--
-- /See:/ 'instanceBlockDeviceMappingSpecification' smart constructor.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification'
  { _ibdmsVirtualName :: !(Maybe Text)
  , _ibdmsNoDevice    :: !(Maybe Text)
  , _ibdmsEBS         :: !(Maybe EBSInstanceBlockDeviceSpecification)
  , _ibdmsDeviceName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceBlockDeviceMappingSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibdmsVirtualName' - The virtual device name.
--
-- * 'ibdmsNoDevice' - suppress the specified device included in the block device mapping.
--
-- * 'ibdmsEBS' - Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- * 'ibdmsDeviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
instanceBlockDeviceMappingSpecification
    :: InstanceBlockDeviceMappingSpecification
instanceBlockDeviceMappingSpecification =
  InstanceBlockDeviceMappingSpecification'
    { _ibdmsVirtualName = Nothing
    , _ibdmsNoDevice = Nothing
    , _ibdmsEBS = Nothing
    , _ibdmsDeviceName = Nothing
    }


-- | The virtual device name.
ibdmsVirtualName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsVirtualName = lens _ibdmsVirtualName (\ s a -> s{_ibdmsVirtualName = a})

-- | suppress the specified device included in the block device mapping.
ibdmsNoDevice :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsNoDevice = lens _ibdmsNoDevice (\ s a -> s{_ibdmsNoDevice = a})

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
ibdmsEBS :: Lens' InstanceBlockDeviceMappingSpecification (Maybe EBSInstanceBlockDeviceSpecification)
ibdmsEBS = lens _ibdmsEBS (\ s a -> s{_ibdmsEBS = a})

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
ibdmsDeviceName :: Lens' InstanceBlockDeviceMappingSpecification (Maybe Text)
ibdmsDeviceName = lens _ibdmsDeviceName (\ s a -> s{_ibdmsDeviceName = a})

instance NFData
           InstanceBlockDeviceMappingSpecification
         where

instance ToQuery
           InstanceBlockDeviceMappingSpecification
         where
        toQuery InstanceBlockDeviceMappingSpecification'{..}
          = mconcat
              ["VirtualName" =: _ibdmsVirtualName,
               "NoDevice" =: _ibdmsNoDevice, "Ebs" =: _ibdmsEBS,
               "DeviceName" =: _ibdmsDeviceName]

-- | Information about the instance type that the Dedicated Host supports.
--
--
--
-- /See:/ 'instanceCapacity' smart constructor.
data InstanceCapacity = InstanceCapacity'
  { _icAvailableCapacity :: !(Maybe Int)
  , _icInstanceType      :: !(Maybe Text)
  , _icTotalCapacity     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icAvailableCapacity' - The number of instances that can still be launched onto the Dedicated Host.
--
-- * 'icInstanceType' - The instance type size supported by the Dedicated Host.
--
-- * 'icTotalCapacity' - The total number of instances that can be launched onto the Dedicated Host.
instanceCapacity
    :: InstanceCapacity
instanceCapacity =
  InstanceCapacity'
    { _icAvailableCapacity = Nothing
    , _icInstanceType = Nothing
    , _icTotalCapacity = Nothing
    }


-- | The number of instances that can still be launched onto the Dedicated Host.
icAvailableCapacity :: Lens' InstanceCapacity (Maybe Int)
icAvailableCapacity = lens _icAvailableCapacity (\ s a -> s{_icAvailableCapacity = a})

-- | The instance type size supported by the Dedicated Host.
icInstanceType :: Lens' InstanceCapacity (Maybe Text)
icInstanceType = lens _icInstanceType (\ s a -> s{_icInstanceType = a})

-- | The total number of instances that can be launched onto the Dedicated Host.
icTotalCapacity :: Lens' InstanceCapacity (Maybe Int)
icTotalCapacity = lens _icTotalCapacity (\ s a -> s{_icTotalCapacity = a})

instance FromXML InstanceCapacity where
        parseXML x
          = InstanceCapacity' <$>
              (x .@? "availableCapacity") <*>
                (x .@? "instanceType")
                <*> (x .@? "totalCapacity")

instance NFData InstanceCapacity where

-- | Describes a Reserved Instance listing state.
--
--
--
-- /See:/ 'instanceCount' smart constructor.
data InstanceCount = InstanceCount'
  { _icState         :: !(Maybe ListingState)
  , _icInstanceCount :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icState' - The states of the listed Reserved Instances.
--
-- * 'icInstanceCount' - The number of listed Reserved Instances in the state specified by the @state@ .
instanceCount
    :: InstanceCount
instanceCount = InstanceCount' {_icState = Nothing, _icInstanceCount = Nothing}


-- | The states of the listed Reserved Instances.
icState :: Lens' InstanceCount (Maybe ListingState)
icState = lens _icState (\ s a -> s{_icState = a})

-- | The number of listed Reserved Instances in the state specified by the @state@ .
icInstanceCount :: Lens' InstanceCount (Maybe Int)
icInstanceCount = lens _icInstanceCount (\ s a -> s{_icInstanceCount = a})

instance FromXML InstanceCount where
        parseXML x
          = InstanceCount' <$>
              (x .@? "state") <*> (x .@? "instanceCount")

instance NFData InstanceCount where

-- | Describes the credit option for CPU usage of a T2 instance.
--
--
--
-- /See:/ 'instanceCreditSpecification' smart constructor.
data InstanceCreditSpecification = InstanceCreditSpecification'
  { _icsInstanceId :: !(Maybe Text)
  , _icsCPUCredits :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceCreditSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icsInstanceId' - The ID of the instance.
--
-- * 'icsCPUCredits' - The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
instanceCreditSpecification
    :: InstanceCreditSpecification
instanceCreditSpecification =
  InstanceCreditSpecification'
    {_icsInstanceId = Nothing, _icsCPUCredits = Nothing}


-- | The ID of the instance.
icsInstanceId :: Lens' InstanceCreditSpecification (Maybe Text)
icsInstanceId = lens _icsInstanceId (\ s a -> s{_icsInstanceId = a})

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
icsCPUCredits :: Lens' InstanceCreditSpecification (Maybe Text)
icsCPUCredits = lens _icsCPUCredits (\ s a -> s{_icsCPUCredits = a})

instance FromXML InstanceCreditSpecification where
        parseXML x
          = InstanceCreditSpecification' <$>
              (x .@? "instanceId") <*> (x .@? "cpuCredits")

instance NFData InstanceCreditSpecification where

-- | Describes the credit option for CPU usage of a T2 instance.
--
--
--
-- /See:/ 'instanceCreditSpecificationRequest' smart constructor.
data InstanceCreditSpecificationRequest = InstanceCreditSpecificationRequest'
  { _icsrInstanceId :: !(Maybe Text)
  , _icsrCPUCredits :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceCreditSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icsrInstanceId' - The ID of the instance.
--
-- * 'icsrCPUCredits' - The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
instanceCreditSpecificationRequest
    :: InstanceCreditSpecificationRequest
instanceCreditSpecificationRequest =
  InstanceCreditSpecificationRequest'
    {_icsrInstanceId = Nothing, _icsrCPUCredits = Nothing}


-- | The ID of the instance.
icsrInstanceId :: Lens' InstanceCreditSpecificationRequest (Maybe Text)
icsrInstanceId = lens _icsrInstanceId (\ s a -> s{_icsrInstanceId = a})

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
icsrCPUCredits :: Lens' InstanceCreditSpecificationRequest (Maybe Text)
icsrCPUCredits = lens _icsrCPUCredits (\ s a -> s{_icsrCPUCredits = a})

instance NFData InstanceCreditSpecificationRequest
         where

instance ToQuery InstanceCreditSpecificationRequest
         where
        toQuery InstanceCreditSpecificationRequest'{..}
          = mconcat
              ["InstanceId" =: _icsrInstanceId,
               "CpuCredits" =: _icsrCPUCredits]

-- | Describes an instance to export.
--
--
--
-- /See:/ 'instanceExportDetails' smart constructor.
data InstanceExportDetails = InstanceExportDetails'
  { _iedTargetEnvironment :: !(Maybe ExportEnvironment)
  , _iedInstanceId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceExportDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iedTargetEnvironment' - The target virtualization environment.
--
-- * 'iedInstanceId' - The ID of the resource being exported.
instanceExportDetails
    :: InstanceExportDetails
instanceExportDetails =
  InstanceExportDetails'
    {_iedTargetEnvironment = Nothing, _iedInstanceId = Nothing}


-- | The target virtualization environment.
iedTargetEnvironment :: Lens' InstanceExportDetails (Maybe ExportEnvironment)
iedTargetEnvironment = lens _iedTargetEnvironment (\ s a -> s{_iedTargetEnvironment = a})

-- | The ID of the resource being exported.
iedInstanceId :: Lens' InstanceExportDetails (Maybe Text)
iedInstanceId = lens _iedInstanceId (\ s a -> s{_iedInstanceId = a})

instance FromXML InstanceExportDetails where
        parseXML x
          = InstanceExportDetails' <$>
              (x .@? "targetEnvironment") <*> (x .@? "instanceId")

instance NFData InstanceExportDetails where

-- | Describes an IPv6 address.
--
--
--
-- /See:/ 'instanceIPv6Address' smart constructor.
newtype InstanceIPv6Address = InstanceIPv6Address'
  { _iiaIPv6Address :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceIPv6Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiaIPv6Address' - The IPv6 address.
instanceIPv6Address
    :: InstanceIPv6Address
instanceIPv6Address = InstanceIPv6Address' {_iiaIPv6Address = Nothing}


-- | The IPv6 address.
iiaIPv6Address :: Lens' InstanceIPv6Address (Maybe Text)
iiaIPv6Address = lens _iiaIPv6Address (\ s a -> s{_iiaIPv6Address = a})

instance FromXML InstanceIPv6Address where
        parseXML x
          = InstanceIPv6Address' <$> (x .@? "ipv6Address")

instance NFData InstanceIPv6Address where

instance ToQuery InstanceIPv6Address where
        toQuery InstanceIPv6Address'{..}
          = mconcat ["Ipv6Address" =: _iiaIPv6Address]

-- | Describes an IPv6 address.
--
--
--
-- /See:/ 'instanceIPv6AddressRequest' smart constructor.
newtype InstanceIPv6AddressRequest = InstanceIPv6AddressRequest'
  { _iiarIPv6Address :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceIPv6AddressRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiarIPv6Address' - The IPv6 address.
instanceIPv6AddressRequest
    :: InstanceIPv6AddressRequest
instanceIPv6AddressRequest =
  InstanceIPv6AddressRequest' {_iiarIPv6Address = Nothing}


-- | The IPv6 address.
iiarIPv6Address :: Lens' InstanceIPv6AddressRequest (Maybe Text)
iiarIPv6Address = lens _iiarIPv6Address (\ s a -> s{_iiarIPv6Address = a})

instance NFData InstanceIPv6AddressRequest where

instance ToQuery InstanceIPv6AddressRequest where
        toQuery InstanceIPv6AddressRequest'{..}
          = mconcat ["Ipv6Address" =: _iiarIPv6Address]

-- | Describes the market (purchasing) option for the instances.
--
--
--
-- /See:/ 'instanceMarketOptionsRequest' smart constructor.
data InstanceMarketOptionsRequest = InstanceMarketOptionsRequest'
  { _imorMarketType  :: !(Maybe MarketType)
  , _imorSpotOptions :: !(Maybe SpotMarketOptions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceMarketOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imorMarketType' - The market type.
--
-- * 'imorSpotOptions' - The options for Spot Instances.
instanceMarketOptionsRequest
    :: InstanceMarketOptionsRequest
instanceMarketOptionsRequest =
  InstanceMarketOptionsRequest'
    {_imorMarketType = Nothing, _imorSpotOptions = Nothing}


-- | The market type.
imorMarketType :: Lens' InstanceMarketOptionsRequest (Maybe MarketType)
imorMarketType = lens _imorMarketType (\ s a -> s{_imorMarketType = a})

-- | The options for Spot Instances.
imorSpotOptions :: Lens' InstanceMarketOptionsRequest (Maybe SpotMarketOptions)
imorSpotOptions = lens _imorSpotOptions (\ s a -> s{_imorSpotOptions = a})

instance NFData InstanceMarketOptionsRequest where

instance ToQuery InstanceMarketOptionsRequest where
        toQuery InstanceMarketOptionsRequest'{..}
          = mconcat
              ["MarketType" =: _imorMarketType,
               "SpotOptions" =: _imorSpotOptions]

-- | Describes the monitoring of an instance.
--
--
--
-- /See:/ 'instanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { _imInstanceId :: !(Maybe Text)
  , _imMonitoring :: !(Maybe Monitoring)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imInstanceId' - The ID of the instance.
--
-- * 'imMonitoring' - The monitoring for the instance.
instanceMonitoring
    :: InstanceMonitoring
instanceMonitoring =
  InstanceMonitoring' {_imInstanceId = Nothing, _imMonitoring = Nothing}


-- | The ID of the instance.
imInstanceId :: Lens' InstanceMonitoring (Maybe Text)
imInstanceId = lens _imInstanceId (\ s a -> s{_imInstanceId = a})

-- | The monitoring for the instance.
imMonitoring :: Lens' InstanceMonitoring (Maybe Monitoring)
imMonitoring = lens _imMonitoring (\ s a -> s{_imMonitoring = a})

instance FromXML InstanceMonitoring where
        parseXML x
          = InstanceMonitoring' <$>
              (x .@? "instanceId") <*> (x .@? "monitoring")

instance NFData InstanceMonitoring where

-- | Describes a network interface.
--
--
--
-- /See:/ 'instanceNetworkInterface' smart constructor.
data InstanceNetworkInterface = InstanceNetworkInterface'
  { _iniGroups             :: !(Maybe [GroupIdentifier])
  , _iniStatus             :: !(Maybe NetworkInterfaceStatus)
  , _iniPrivateIPAddresses :: !(Maybe [InstancePrivateIPAddress])
  , _iniSourceDestCheck    :: !(Maybe Bool)
  , _iniVPCId              :: !(Maybe Text)
  , _iniNetworkInterfaceId :: !(Maybe Text)
  , _iniSubnetId           :: !(Maybe Text)
  , _iniMACAddress         :: !(Maybe Text)
  , _iniAttachment         :: !(Maybe InstanceNetworkInterfaceAttachment)
  , _iniOwnerId            :: !(Maybe Text)
  , _iniPrivateIPAddress   :: !(Maybe Text)
  , _iniPrivateDNSName     :: !(Maybe Text)
  , _iniDescription        :: !(Maybe Text)
  , _iniAssociation        :: !(Maybe InstanceNetworkInterfaceAssociation)
  , _iniIPv6Addresses      :: !(Maybe [InstanceIPv6Address])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceNetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iniGroups' - One or more security groups.
--
-- * 'iniStatus' - The status of the network interface.
--
-- * 'iniPrivateIPAddresses' - One or more private IPv4 addresses associated with the network interface.
--
-- * 'iniSourceDestCheck' - Indicates whether to validate network traffic to or from this network interface.
--
-- * 'iniVPCId' - The ID of the VPC.
--
-- * 'iniNetworkInterfaceId' - The ID of the network interface.
--
-- * 'iniSubnetId' - The ID of the subnet.
--
-- * 'iniMACAddress' - The MAC address.
--
-- * 'iniAttachment' - The network interface attachment.
--
-- * 'iniOwnerId' - The ID of the AWS account that created the network interface.
--
-- * 'iniPrivateIPAddress' - The IPv4 address of the network interface within the subnet.
--
-- * 'iniPrivateDNSName' - The private DNS name.
--
-- * 'iniDescription' - The description.
--
-- * 'iniAssociation' - The association information for an Elastic IPv4 associated with the network interface.
--
-- * 'iniIPv6Addresses' - One or more IPv6 addresses associated with the network interface.
instanceNetworkInterface
    :: InstanceNetworkInterface
instanceNetworkInterface =
  InstanceNetworkInterface'
    { _iniGroups = Nothing
    , _iniStatus = Nothing
    , _iniPrivateIPAddresses = Nothing
    , _iniSourceDestCheck = Nothing
    , _iniVPCId = Nothing
    , _iniNetworkInterfaceId = Nothing
    , _iniSubnetId = Nothing
    , _iniMACAddress = Nothing
    , _iniAttachment = Nothing
    , _iniOwnerId = Nothing
    , _iniPrivateIPAddress = Nothing
    , _iniPrivateDNSName = Nothing
    , _iniDescription = Nothing
    , _iniAssociation = Nothing
    , _iniIPv6Addresses = Nothing
    }


-- | One or more security groups.
iniGroups :: Lens' InstanceNetworkInterface [GroupIdentifier]
iniGroups = lens _iniGroups (\ s a -> s{_iniGroups = a}) . _Default . _Coerce

-- | The status of the network interface.
iniStatus :: Lens' InstanceNetworkInterface (Maybe NetworkInterfaceStatus)
iniStatus = lens _iniStatus (\ s a -> s{_iniStatus = a})

-- | One or more private IPv4 addresses associated with the network interface.
iniPrivateIPAddresses :: Lens' InstanceNetworkInterface [InstancePrivateIPAddress]
iniPrivateIPAddresses = lens _iniPrivateIPAddresses (\ s a -> s{_iniPrivateIPAddresses = a}) . _Default . _Coerce

-- | Indicates whether to validate network traffic to or from this network interface.
iniSourceDestCheck :: Lens' InstanceNetworkInterface (Maybe Bool)
iniSourceDestCheck = lens _iniSourceDestCheck (\ s a -> s{_iniSourceDestCheck = a})

-- | The ID of the VPC.
iniVPCId :: Lens' InstanceNetworkInterface (Maybe Text)
iniVPCId = lens _iniVPCId (\ s a -> s{_iniVPCId = a})

-- | The ID of the network interface.
iniNetworkInterfaceId :: Lens' InstanceNetworkInterface (Maybe Text)
iniNetworkInterfaceId = lens _iniNetworkInterfaceId (\ s a -> s{_iniNetworkInterfaceId = a})

-- | The ID of the subnet.
iniSubnetId :: Lens' InstanceNetworkInterface (Maybe Text)
iniSubnetId = lens _iniSubnetId (\ s a -> s{_iniSubnetId = a})

-- | The MAC address.
iniMACAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniMACAddress = lens _iniMACAddress (\ s a -> s{_iniMACAddress = a})

-- | The network interface attachment.
iniAttachment :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAttachment)
iniAttachment = lens _iniAttachment (\ s a -> s{_iniAttachment = a})

-- | The ID of the AWS account that created the network interface.
iniOwnerId :: Lens' InstanceNetworkInterface (Maybe Text)
iniOwnerId = lens _iniOwnerId (\ s a -> s{_iniOwnerId = a})

-- | The IPv4 address of the network interface within the subnet.
iniPrivateIPAddress :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateIPAddress = lens _iniPrivateIPAddress (\ s a -> s{_iniPrivateIPAddress = a})

-- | The private DNS name.
iniPrivateDNSName :: Lens' InstanceNetworkInterface (Maybe Text)
iniPrivateDNSName = lens _iniPrivateDNSName (\ s a -> s{_iniPrivateDNSName = a})

-- | The description.
iniDescription :: Lens' InstanceNetworkInterface (Maybe Text)
iniDescription = lens _iniDescription (\ s a -> s{_iniDescription = a})

-- | The association information for an Elastic IPv4 associated with the network interface.
iniAssociation :: Lens' InstanceNetworkInterface (Maybe InstanceNetworkInterfaceAssociation)
iniAssociation = lens _iniAssociation (\ s a -> s{_iniAssociation = a})

-- | One or more IPv6 addresses associated with the network interface.
iniIPv6Addresses :: Lens' InstanceNetworkInterface [InstanceIPv6Address]
iniIPv6Addresses = lens _iniIPv6Addresses (\ s a -> s{_iniIPv6Addresses = a}) . _Default . _Coerce

instance FromXML InstanceNetworkInterface where
        parseXML x
          = InstanceNetworkInterface' <$>
              (x .@? "groupSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "status")
                <*>
                (x .@? "privateIpAddressesSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "sourceDestCheck")
                <*> (x .@? "vpcId")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "subnetId")
                <*> (x .@? "macAddress")
                <*> (x .@? "attachment")
                <*> (x .@? "ownerId")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "privateDnsName")
                <*> (x .@? "description")
                <*> (x .@? "association")
                <*>
                (x .@? "ipv6AddressesSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData InstanceNetworkInterface where

-- | Describes association information for an Elastic IP address (IPv4).
--
--
--
-- /See:/ 'instanceNetworkInterfaceAssociation' smart constructor.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'
  { _iniaPublicDNSName :: !(Maybe Text)
  , _iniaIPOwnerId     :: !(Maybe Text)
  , _iniaPublicIP      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceNetworkInterfaceAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iniaPublicDNSName' - The public DNS name.
--
-- * 'iniaIPOwnerId' - The ID of the owner of the Elastic IP address.
--
-- * 'iniaPublicIP' - The public IP address or Elastic IP address bound to the network interface.
instanceNetworkInterfaceAssociation
    :: InstanceNetworkInterfaceAssociation
instanceNetworkInterfaceAssociation =
  InstanceNetworkInterfaceAssociation'
    { _iniaPublicDNSName = Nothing
    , _iniaIPOwnerId = Nothing
    , _iniaPublicIP = Nothing
    }


-- | The public DNS name.
iniaPublicDNSName :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicDNSName = lens _iniaPublicDNSName (\ s a -> s{_iniaPublicDNSName = a})

-- | The ID of the owner of the Elastic IP address.
iniaIPOwnerId :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaIPOwnerId = lens _iniaIPOwnerId (\ s a -> s{_iniaIPOwnerId = a})

-- | The public IP address or Elastic IP address bound to the network interface.
iniaPublicIP :: Lens' InstanceNetworkInterfaceAssociation (Maybe Text)
iniaPublicIP = lens _iniaPublicIP (\ s a -> s{_iniaPublicIP = a})

instance FromXML InstanceNetworkInterfaceAssociation
         where
        parseXML x
          = InstanceNetworkInterfaceAssociation' <$>
              (x .@? "publicDnsName") <*> (x .@? "ipOwnerId") <*>
                (x .@? "publicIp")

instance NFData InstanceNetworkInterfaceAssociation
         where

-- | Describes a network interface attachment.
--
--
--
-- /See:/ 'instanceNetworkInterfaceAttachment' smart constructor.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'
  { _iniaStatus              :: !(Maybe AttachmentStatus)
  , _iniaDeleteOnTermination :: !(Maybe Bool)
  , _iniaAttachmentId        :: !(Maybe Text)
  , _iniaAttachTime          :: !(Maybe ISO8601)
  , _iniaDeviceIndex         :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceNetworkInterfaceAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iniaStatus' - The attachment state.
--
-- * 'iniaDeleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
--
-- * 'iniaAttachmentId' - The ID of the network interface attachment.
--
-- * 'iniaAttachTime' - The time stamp when the attachment initiated.
--
-- * 'iniaDeviceIndex' - The index of the device on the instance for the network interface attachment.
instanceNetworkInterfaceAttachment
    :: InstanceNetworkInterfaceAttachment
instanceNetworkInterfaceAttachment =
  InstanceNetworkInterfaceAttachment'
    { _iniaStatus = Nothing
    , _iniaDeleteOnTermination = Nothing
    , _iniaAttachmentId = Nothing
    , _iniaAttachTime = Nothing
    , _iniaDeviceIndex = Nothing
    }


-- | The attachment state.
iniaStatus :: Lens' InstanceNetworkInterfaceAttachment (Maybe AttachmentStatus)
iniaStatus = lens _iniaStatus (\ s a -> s{_iniaStatus = a})

-- | Indicates whether the network interface is deleted when the instance is terminated.
iniaDeleteOnTermination :: Lens' InstanceNetworkInterfaceAttachment (Maybe Bool)
iniaDeleteOnTermination = lens _iniaDeleteOnTermination (\ s a -> s{_iniaDeleteOnTermination = a})

-- | The ID of the network interface attachment.
iniaAttachmentId :: Lens' InstanceNetworkInterfaceAttachment (Maybe Text)
iniaAttachmentId = lens _iniaAttachmentId (\ s a -> s{_iniaAttachmentId = a})

-- | The time stamp when the attachment initiated.
iniaAttachTime :: Lens' InstanceNetworkInterfaceAttachment (Maybe UTCTime)
iniaAttachTime = lens _iniaAttachTime (\ s a -> s{_iniaAttachTime = a}) . mapping _Time

-- | The index of the device on the instance for the network interface attachment.
iniaDeviceIndex :: Lens' InstanceNetworkInterfaceAttachment (Maybe Int)
iniaDeviceIndex = lens _iniaDeviceIndex (\ s a -> s{_iniaDeviceIndex = a})

instance FromXML InstanceNetworkInterfaceAttachment
         where
        parseXML x
          = InstanceNetworkInterfaceAttachment' <$>
              (x .@? "status") <*> (x .@? "deleteOnTermination")
                <*> (x .@? "attachmentId")
                <*> (x .@? "attachTime")
                <*> (x .@? "deviceIndex")

instance NFData InstanceNetworkInterfaceAttachment
         where

-- | Describes a network interface.
--
--
--
-- /See:/ 'instanceNetworkInterfaceSpecification' smart constructor.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification'
  { _inisGroups :: !(Maybe [Text])
  , _inisPrivateIPAddresses :: !(Maybe [PrivateIPAddressSpecification])
  , _inisDeleteOnTermination :: !(Maybe Bool)
  , _inisAssociatePublicIPAddress :: !(Maybe Bool)
  , _inisNetworkInterfaceId :: !(Maybe Text)
  , _inisSubnetId :: !(Maybe Text)
  , _inisIPv6AddressCount :: !(Maybe Int)
  , _inisPrivateIPAddress :: !(Maybe Text)
  , _inisSecondaryPrivateIPAddressCount :: !(Maybe Int)
  , _inisDescription :: !(Maybe Text)
  , _inisDeviceIndex :: !(Maybe Int)
  , _inisIPv6Addresses :: !(Maybe [InstanceIPv6Address])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceNetworkInterfaceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'inisGroups' - The IDs of the security groups for the network interface. Applies only if creating a network interface when launching an instance.
--
-- * 'inisPrivateIPAddresses' - One or more private IPv4 addresses to assign to the network interface. Only one private IPv4 address can be designated as primary. You cannot specify this option if you're launching more than one instance in a 'RunInstances' request.
--
-- * 'inisDeleteOnTermination' - If set to @true@ , the interface is deleted when the instance is terminated. You can specify @true@ only if creating a new network interface when launching an instance.
--
-- * 'inisAssociatePublicIPAddress' - Indicates whether to assign a public IPv4 address to an instance you launch in a VPC. The public IP address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
--
-- * 'inisNetworkInterfaceId' - The ID of the network interface.
--
-- * 'inisSubnetId' - The ID of the subnet associated with the network string. Applies only if creating a network interface when launching an instance.
--
-- * 'inisIPv6AddressCount' - A number of IPv6 addresses to assign to the network interface. Amazon EC2 chooses the IPv6 addresses from the range of the subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- * 'inisPrivateIPAddress' - The private IPv4 address of the network interface. Applies only if creating a network interface when launching an instance. You cannot specify this option if you're launching more than one instance in a 'RunInstances' request.
--
-- * 'inisSecondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses. You can't specify this option and specify more than one private IP address using the private IP addresses option. You cannot specify this option if you're launching more than one instance in a 'RunInstances' request.
--
-- * 'inisDescription' - The description of the network interface. Applies only if creating a network interface when launching an instance.
--
-- * 'inisDeviceIndex' - The index of the device on the instance for the network interface attachment. If you are specifying a network interface in a 'RunInstances' request, you must provide the device index.
--
-- * 'inisIPv6Addresses' - One or more IPv6 addresses to assign to the network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
instanceNetworkInterfaceSpecification
    :: InstanceNetworkInterfaceSpecification
instanceNetworkInterfaceSpecification =
  InstanceNetworkInterfaceSpecification'
    { _inisGroups = Nothing
    , _inisPrivateIPAddresses = Nothing
    , _inisDeleteOnTermination = Nothing
    , _inisAssociatePublicIPAddress = Nothing
    , _inisNetworkInterfaceId = Nothing
    , _inisSubnetId = Nothing
    , _inisIPv6AddressCount = Nothing
    , _inisPrivateIPAddress = Nothing
    , _inisSecondaryPrivateIPAddressCount = Nothing
    , _inisDescription = Nothing
    , _inisDeviceIndex = Nothing
    , _inisIPv6Addresses = Nothing
    }


-- | The IDs of the security groups for the network interface. Applies only if creating a network interface when launching an instance.
inisGroups :: Lens' InstanceNetworkInterfaceSpecification [Text]
inisGroups = lens _inisGroups (\ s a -> s{_inisGroups = a}) . _Default . _Coerce

-- | One or more private IPv4 addresses to assign to the network interface. Only one private IPv4 address can be designated as primary. You cannot specify this option if you're launching more than one instance in a 'RunInstances' request.
inisPrivateIPAddresses :: Lens' InstanceNetworkInterfaceSpecification [PrivateIPAddressSpecification]
inisPrivateIPAddresses = lens _inisPrivateIPAddresses (\ s a -> s{_inisPrivateIPAddresses = a}) . _Default . _Coerce

-- | If set to @true@ , the interface is deleted when the instance is terminated. You can specify @true@ only if creating a new network interface when launching an instance.
inisDeleteOnTermination :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisDeleteOnTermination = lens _inisDeleteOnTermination (\ s a -> s{_inisDeleteOnTermination = a})

-- | Indicates whether to assign a public IPv4 address to an instance you launch in a VPC. The public IP address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
inisAssociatePublicIPAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Bool)
inisAssociatePublicIPAddress = lens _inisAssociatePublicIPAddress (\ s a -> s{_inisAssociatePublicIPAddress = a})

-- | The ID of the network interface.
inisNetworkInterfaceId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisNetworkInterfaceId = lens _inisNetworkInterfaceId (\ s a -> s{_inisNetworkInterfaceId = a})

-- | The ID of the subnet associated with the network string. Applies only if creating a network interface when launching an instance.
inisSubnetId :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisSubnetId = lens _inisSubnetId (\ s a -> s{_inisSubnetId = a})

-- | A number of IPv6 addresses to assign to the network interface. Amazon EC2 chooses the IPv6 addresses from the range of the subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
inisIPv6AddressCount :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisIPv6AddressCount = lens _inisIPv6AddressCount (\ s a -> s{_inisIPv6AddressCount = a})

-- | The private IPv4 address of the network interface. Applies only if creating a network interface when launching an instance. You cannot specify this option if you're launching more than one instance in a 'RunInstances' request.
inisPrivateIPAddress :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisPrivateIPAddress = lens _inisPrivateIPAddress (\ s a -> s{_inisPrivateIPAddress = a})

-- | The number of secondary private IPv4 addresses. You can't specify this option and specify more than one private IP address using the private IP addresses option. You cannot specify this option if you're launching more than one instance in a 'RunInstances' request.
inisSecondaryPrivateIPAddressCount :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisSecondaryPrivateIPAddressCount = lens _inisSecondaryPrivateIPAddressCount (\ s a -> s{_inisSecondaryPrivateIPAddressCount = a})

-- | The description of the network interface. Applies only if creating a network interface when launching an instance.
inisDescription :: Lens' InstanceNetworkInterfaceSpecification (Maybe Text)
inisDescription = lens _inisDescription (\ s a -> s{_inisDescription = a})

-- | The index of the device on the instance for the network interface attachment. If you are specifying a network interface in a 'RunInstances' request, you must provide the device index.
inisDeviceIndex :: Lens' InstanceNetworkInterfaceSpecification (Maybe Int)
inisDeviceIndex = lens _inisDeviceIndex (\ s a -> s{_inisDeviceIndex = a})

-- | One or more IPv6 addresses to assign to the network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
inisIPv6Addresses :: Lens' InstanceNetworkInterfaceSpecification [InstanceIPv6Address]
inisIPv6Addresses = lens _inisIPv6Addresses (\ s a -> s{_inisIPv6Addresses = a}) . _Default . _Coerce

instance FromXML
           InstanceNetworkInterfaceSpecification
         where
        parseXML x
          = InstanceNetworkInterfaceSpecification' <$>
              (x .@? "SecurityGroupId" .!@ mempty >>=
                 may (parseXMLList "SecurityGroupId"))
                <*>
                (x .@? "privateIpAddressesSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "deleteOnTermination")
                <*> (x .@? "associatePublicIpAddress")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "subnetId")
                <*> (x .@? "ipv6AddressCount")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "secondaryPrivateIpAddressCount")
                <*> (x .@? "description")
                <*> (x .@? "deviceIndex")
                <*>
                (x .@? "ipv6AddressesSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData InstanceNetworkInterfaceSpecification
         where

instance ToQuery
           InstanceNetworkInterfaceSpecification
         where
        toQuery InstanceNetworkInterfaceSpecification'{..}
          = mconcat
              [toQuery
                 (toQueryList "SecurityGroupId" <$> _inisGroups),
               toQuery
                 (toQueryList "PrivateIpAddresses" <$>
                    _inisPrivateIPAddresses),
               "DeleteOnTermination" =: _inisDeleteOnTermination,
               "AssociatePublicIpAddress" =:
                 _inisAssociatePublicIPAddress,
               "NetworkInterfaceId" =: _inisNetworkInterfaceId,
               "SubnetId" =: _inisSubnetId,
               "Ipv6AddressCount" =: _inisIPv6AddressCount,
               "PrivateIpAddress" =: _inisPrivateIPAddress,
               "SecondaryPrivateIpAddressCount" =:
                 _inisSecondaryPrivateIPAddressCount,
               "Description" =: _inisDescription,
               "DeviceIndex" =: _inisDeviceIndex,
               toQuery
                 (toQueryList "Ipv6Addresses" <$> _inisIPv6Addresses)]

-- | Describes a private IPv4 address.
--
--
--
-- /See:/ 'instancePrivateIPAddress' smart constructor.
data InstancePrivateIPAddress = InstancePrivateIPAddress'
  { _ipiaPrimary          :: !(Maybe Bool)
  , _ipiaPrivateIPAddress :: !(Maybe Text)
  , _ipiaPrivateDNSName   :: !(Maybe Text)
  , _ipiaAssociation      :: !(Maybe InstanceNetworkInterfaceAssociation)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstancePrivateIPAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipiaPrimary' - Indicates whether this IPv4 address is the primary private IP address of the network interface.
--
-- * 'ipiaPrivateIPAddress' - The private IPv4 address of the network interface.
--
-- * 'ipiaPrivateDNSName' - The private IPv4 DNS name.
--
-- * 'ipiaAssociation' - The association information for an Elastic IP address for the network interface.
instancePrivateIPAddress
    :: InstancePrivateIPAddress
instancePrivateIPAddress =
  InstancePrivateIPAddress'
    { _ipiaPrimary = Nothing
    , _ipiaPrivateIPAddress = Nothing
    , _ipiaPrivateDNSName = Nothing
    , _ipiaAssociation = Nothing
    }


-- | Indicates whether this IPv4 address is the primary private IP address of the network interface.
ipiaPrimary :: Lens' InstancePrivateIPAddress (Maybe Bool)
ipiaPrimary = lens _ipiaPrimary (\ s a -> s{_ipiaPrimary = a})

-- | The private IPv4 address of the network interface.
ipiaPrivateIPAddress :: Lens' InstancePrivateIPAddress (Maybe Text)
ipiaPrivateIPAddress = lens _ipiaPrivateIPAddress (\ s a -> s{_ipiaPrivateIPAddress = a})

-- | The private IPv4 DNS name.
ipiaPrivateDNSName :: Lens' InstancePrivateIPAddress (Maybe Text)
ipiaPrivateDNSName = lens _ipiaPrivateDNSName (\ s a -> s{_ipiaPrivateDNSName = a})

-- | The association information for an Elastic IP address for the network interface.
ipiaAssociation :: Lens' InstancePrivateIPAddress (Maybe InstanceNetworkInterfaceAssociation)
ipiaAssociation = lens _ipiaAssociation (\ s a -> s{_ipiaAssociation = a})

instance FromXML InstancePrivateIPAddress where
        parseXML x
          = InstancePrivateIPAddress' <$>
              (x .@? "primary") <*> (x .@? "privateIpAddress") <*>
                (x .@? "privateDnsName")
                <*> (x .@? "association")

instance NFData InstancePrivateIPAddress where

-- | Describes the current state of an instance.
--
--
--
-- /See:/ 'instanceState' smart constructor.
data InstanceState = InstanceState'
  { _isName :: !InstanceStateName
  , _isCode :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isName' - The current state of the instance.
--
-- * 'isCode' - The low byte represents the state. The high byte is an opaque internal value and should be ignored.     * @0@ : @pending@      * @16@ : @running@      * @32@ : @shutting-down@      * @48@ : @terminated@      * @64@ : @stopping@      * @80@ : @stopped@
instanceState
    :: InstanceStateName -- ^ 'isName'
    -> Int -- ^ 'isCode'
    -> InstanceState
instanceState pName_ pCode_ =
  InstanceState' {_isName = pName_, _isCode = pCode_}


-- | The current state of the instance.
isName :: Lens' InstanceState InstanceStateName
isName = lens _isName (\ s a -> s{_isName = a})

-- | The low byte represents the state. The high byte is an opaque internal value and should be ignored.     * @0@ : @pending@      * @16@ : @running@      * @32@ : @shutting-down@      * @48@ : @terminated@      * @64@ : @stopping@      * @80@ : @stopped@
isCode :: Lens' InstanceState Int
isCode = lens _isCode (\ s a -> s{_isCode = a})

instance FromXML InstanceState where
        parseXML x
          = InstanceState' <$> (x .@ "name") <*> (x .@ "code")

instance NFData InstanceState where

-- | Describes an instance state change.
--
--
--
-- /See:/ 'instanceStateChange' smart constructor.
data InstanceStateChange = InstanceStateChange'
  { _iscInstanceId    :: !(Maybe Text)
  , _iscCurrentState  :: !(Maybe InstanceState)
  , _iscPreviousState :: !(Maybe InstanceState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscInstanceId' - The ID of the instance.
--
-- * 'iscCurrentState' - The current state of the instance.
--
-- * 'iscPreviousState' - The previous state of the instance.
instanceStateChange
    :: InstanceStateChange
instanceStateChange =
  InstanceStateChange'
    { _iscInstanceId = Nothing
    , _iscCurrentState = Nothing
    , _iscPreviousState = Nothing
    }


-- | The ID of the instance.
iscInstanceId :: Lens' InstanceStateChange (Maybe Text)
iscInstanceId = lens _iscInstanceId (\ s a -> s{_iscInstanceId = a})

-- | The current state of the instance.
iscCurrentState :: Lens' InstanceStateChange (Maybe InstanceState)
iscCurrentState = lens _iscCurrentState (\ s a -> s{_iscCurrentState = a})

-- | The previous state of the instance.
iscPreviousState :: Lens' InstanceStateChange (Maybe InstanceState)
iscPreviousState = lens _iscPreviousState (\ s a -> s{_iscPreviousState = a})

instance FromXML InstanceStateChange where
        parseXML x
          = InstanceStateChange' <$>
              (x .@? "instanceId") <*> (x .@? "currentState") <*>
                (x .@? "previousState")

instance NFData InstanceStateChange where

-- | Describes the status of an instance.
--
--
--
-- /See:/ 'instanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { _isInstanceId       :: !(Maybe Text)
  , _isSystemStatus     :: !(Maybe InstanceStatusSummary)
  , _isEvents           :: !(Maybe [InstanceStatusEvent])
  , _isAvailabilityZone :: !(Maybe Text)
  , _isInstanceStatus   :: !(Maybe InstanceStatusSummary)
  , _isInstanceState    :: !(Maybe InstanceState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isInstanceId' - The ID of the instance.
--
-- * 'isSystemStatus' - Reports impaired functionality that stems from issues related to the systems that support an instance, such as hardware failures and network connectivity problems.
--
-- * 'isEvents' - Any scheduled events associated with the instance.
--
-- * 'isAvailabilityZone' - The Availability Zone of the instance.
--
-- * 'isInstanceStatus' - Reports impaired functionality that stems from issues internal to the instance, such as impaired reachability.
--
-- * 'isInstanceState' - The intended state of the instance. 'DescribeInstanceStatus' requires that an instance be in the @running@ state.
instanceStatus
    :: InstanceStatus
instanceStatus =
  InstanceStatus'
    { _isInstanceId = Nothing
    , _isSystemStatus = Nothing
    , _isEvents = Nothing
    , _isAvailabilityZone = Nothing
    , _isInstanceStatus = Nothing
    , _isInstanceState = Nothing
    }


-- | The ID of the instance.
isInstanceId :: Lens' InstanceStatus (Maybe Text)
isInstanceId = lens _isInstanceId (\ s a -> s{_isInstanceId = a})

-- | Reports impaired functionality that stems from issues related to the systems that support an instance, such as hardware failures and network connectivity problems.
isSystemStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isSystemStatus = lens _isSystemStatus (\ s a -> s{_isSystemStatus = a})

-- | Any scheduled events associated with the instance.
isEvents :: Lens' InstanceStatus [InstanceStatusEvent]
isEvents = lens _isEvents (\ s a -> s{_isEvents = a}) . _Default . _Coerce

-- | The Availability Zone of the instance.
isAvailabilityZone :: Lens' InstanceStatus (Maybe Text)
isAvailabilityZone = lens _isAvailabilityZone (\ s a -> s{_isAvailabilityZone = a})

-- | Reports impaired functionality that stems from issues internal to the instance, such as impaired reachability.
isInstanceStatus :: Lens' InstanceStatus (Maybe InstanceStatusSummary)
isInstanceStatus = lens _isInstanceStatus (\ s a -> s{_isInstanceStatus = a})

-- | The intended state of the instance. 'DescribeInstanceStatus' requires that an instance be in the @running@ state.
isInstanceState :: Lens' InstanceStatus (Maybe InstanceState)
isInstanceState = lens _isInstanceState (\ s a -> s{_isInstanceState = a})

instance FromXML InstanceStatus where
        parseXML x
          = InstanceStatus' <$>
              (x .@? "instanceId") <*> (x .@? "systemStatus") <*>
                (x .@? "eventsSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "availabilityZone")
                <*> (x .@? "instanceStatus")
                <*> (x .@? "instanceState")

instance NFData InstanceStatus where

-- | Describes the instance status.
--
--
--
-- /See:/ 'instanceStatusDetails' smart constructor.
data InstanceStatusDetails = InstanceStatusDetails'
  { _isdStatus        :: !(Maybe StatusType)
  , _isdImpairedSince :: !(Maybe ISO8601)
  , _isdName          :: !(Maybe StatusName)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isdStatus' - The status.
--
-- * 'isdImpairedSince' - The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
--
-- * 'isdName' - The type of instance status.
instanceStatusDetails
    :: InstanceStatusDetails
instanceStatusDetails =
  InstanceStatusDetails'
    {_isdStatus = Nothing, _isdImpairedSince = Nothing, _isdName = Nothing}


-- | The status.
isdStatus :: Lens' InstanceStatusDetails (Maybe StatusType)
isdStatus = lens _isdStatus (\ s a -> s{_isdStatus = a})

-- | The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
isdImpairedSince :: Lens' InstanceStatusDetails (Maybe UTCTime)
isdImpairedSince = lens _isdImpairedSince (\ s a -> s{_isdImpairedSince = a}) . mapping _Time

-- | The type of instance status.
isdName :: Lens' InstanceStatusDetails (Maybe StatusName)
isdName = lens _isdName (\ s a -> s{_isdName = a})

instance FromXML InstanceStatusDetails where
        parseXML x
          = InstanceStatusDetails' <$>
              (x .@? "status") <*> (x .@? "impairedSince") <*>
                (x .@? "name")

instance NFData InstanceStatusDetails where

-- | Describes a scheduled event for an instance.
--
--
--
-- /See:/ 'instanceStatusEvent' smart constructor.
data InstanceStatusEvent = InstanceStatusEvent'
  { _iseNotBefore   :: !(Maybe ISO8601)
  , _iseCode        :: !(Maybe EventCode)
  , _iseDescription :: !(Maybe Text)
  , _iseNotAfter    :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceStatusEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iseNotBefore' - The earliest scheduled start time for the event.
--
-- * 'iseCode' - The event code.
--
-- * 'iseDescription' - A description of the event. After a scheduled event is completed, it can still be described for up to a week. If the event has been completed, this description starts with the following text: [Completed].
--
-- * 'iseNotAfter' - The latest scheduled end time for the event.
instanceStatusEvent
    :: InstanceStatusEvent
instanceStatusEvent =
  InstanceStatusEvent'
    { _iseNotBefore = Nothing
    , _iseCode = Nothing
    , _iseDescription = Nothing
    , _iseNotAfter = Nothing
    }


-- | The earliest scheduled start time for the event.
iseNotBefore :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotBefore = lens _iseNotBefore (\ s a -> s{_iseNotBefore = a}) . mapping _Time

-- | The event code.
iseCode :: Lens' InstanceStatusEvent (Maybe EventCode)
iseCode = lens _iseCode (\ s a -> s{_iseCode = a})

-- | A description of the event. After a scheduled event is completed, it can still be described for up to a week. If the event has been completed, this description starts with the following text: [Completed].
iseDescription :: Lens' InstanceStatusEvent (Maybe Text)
iseDescription = lens _iseDescription (\ s a -> s{_iseDescription = a})

-- | The latest scheduled end time for the event.
iseNotAfter :: Lens' InstanceStatusEvent (Maybe UTCTime)
iseNotAfter = lens _iseNotAfter (\ s a -> s{_iseNotAfter = a}) . mapping _Time

instance FromXML InstanceStatusEvent where
        parseXML x
          = InstanceStatusEvent' <$>
              (x .@? "notBefore") <*> (x .@? "code") <*>
                (x .@? "description")
                <*> (x .@? "notAfter")

instance NFData InstanceStatusEvent where

-- | Describes the status of an instance.
--
--
--
-- /See:/ 'instanceStatusSummary' smart constructor.
data InstanceStatusSummary = InstanceStatusSummary'
  { _issDetails :: !(Maybe [InstanceStatusDetails])
  , _issStatus  :: !SummaryStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceStatusSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'issDetails' - The system instance health or application instance health.
--
-- * 'issStatus' - The status.
instanceStatusSummary
    :: SummaryStatus -- ^ 'issStatus'
    -> InstanceStatusSummary
instanceStatusSummary pStatus_ =
  InstanceStatusSummary' {_issDetails = Nothing, _issStatus = pStatus_}


-- | The system instance health or application instance health.
issDetails :: Lens' InstanceStatusSummary [InstanceStatusDetails]
issDetails = lens _issDetails (\ s a -> s{_issDetails = a}) . _Default . _Coerce

-- | The status.
issStatus :: Lens' InstanceStatusSummary SummaryStatus
issStatus = lens _issStatus (\ s a -> s{_issStatus = a})

instance FromXML InstanceStatusSummary where
        parseXML x
          = InstanceStatusSummary' <$>
              (x .@? "details" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@ "status")

instance NFData InstanceStatusSummary where

-- | Describes an Internet gateway.
--
--
--
-- /See:/ 'internetGateway' smart constructor.
data InternetGateway = InternetGateway'
  { _igAttachments       :: !(Maybe [InternetGatewayAttachment])
  , _igTags              :: !(Maybe [Tag])
  , _igInternetGatewayId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igAttachments' - Any VPCs attached to the Internet gateway.
--
-- * 'igTags' - Any tags assigned to the Internet gateway.
--
-- * 'igInternetGatewayId' - The ID of the Internet gateway.
internetGateway
    :: Text -- ^ 'igInternetGatewayId'
    -> InternetGateway
internetGateway pInternetGatewayId_ =
  InternetGateway'
    { _igAttachments = Nothing
    , _igTags = Nothing
    , _igInternetGatewayId = pInternetGatewayId_
    }


-- | Any VPCs attached to the Internet gateway.
igAttachments :: Lens' InternetGateway [InternetGatewayAttachment]
igAttachments = lens _igAttachments (\ s a -> s{_igAttachments = a}) . _Default . _Coerce

-- | Any tags assigned to the Internet gateway.
igTags :: Lens' InternetGateway [Tag]
igTags = lens _igTags (\ s a -> s{_igTags = a}) . _Default . _Coerce

-- | The ID of the Internet gateway.
igInternetGatewayId :: Lens' InternetGateway Text
igInternetGatewayId = lens _igInternetGatewayId (\ s a -> s{_igInternetGatewayId = a})

instance FromXML InternetGateway where
        parseXML x
          = InternetGateway' <$>
              (x .@? "attachmentSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "internetGatewayId")

instance NFData InternetGateway where

-- | Describes the attachment of a VPC to an Internet gateway or an egress-only Internet gateway.
--
--
--
-- /See:/ 'internetGatewayAttachment' smart constructor.
data InternetGatewayAttachment = InternetGatewayAttachment'
  { _igaState :: !AttachmentStatus
  , _igaVPCId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InternetGatewayAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igaState' - The current state of the attachment. For an Internet gateway, the state is @available@ when attached to a VPC; otherwise, this value is not returned.
--
-- * 'igaVPCId' - The ID of the VPC.
internetGatewayAttachment
    :: AttachmentStatus -- ^ 'igaState'
    -> Text -- ^ 'igaVPCId'
    -> InternetGatewayAttachment
internetGatewayAttachment pState_ pVPCId_ =
  InternetGatewayAttachment' {_igaState = pState_, _igaVPCId = pVPCId_}


-- | The current state of the attachment. For an Internet gateway, the state is @available@ when attached to a VPC; otherwise, this value is not returned.
igaState :: Lens' InternetGatewayAttachment AttachmentStatus
igaState = lens _igaState (\ s a -> s{_igaState = a})

-- | The ID of the VPC.
igaVPCId :: Lens' InternetGatewayAttachment Text
igaVPCId = lens _igaVPCId (\ s a -> s{_igaVPCId = a})

instance FromXML InternetGatewayAttachment where
        parseXML x
          = InternetGatewayAttachment' <$>
              (x .@ "state") <*> (x .@ "vpcId")

instance NFData InternetGatewayAttachment where

-- | Describes a key pair.
--
--
--
-- /See:/ 'keyPairInfo' smart constructor.
data KeyPairInfo = KeyPairInfo'
  { _kpiKeyFingerprint :: !(Maybe Text)
  , _kpiKeyName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyPairInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpiKeyFingerprint' - If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the public key, this is the MD5 public key fingerprint as specified in section 4 of RFC4716.
--
-- * 'kpiKeyName' - The name of the key pair.
keyPairInfo
    :: KeyPairInfo
keyPairInfo = KeyPairInfo' {_kpiKeyFingerprint = Nothing, _kpiKeyName = Nothing}


-- | If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the public key, this is the MD5 public key fingerprint as specified in section 4 of RFC4716.
kpiKeyFingerprint :: Lens' KeyPairInfo (Maybe Text)
kpiKeyFingerprint = lens _kpiKeyFingerprint (\ s a -> s{_kpiKeyFingerprint = a})

-- | The name of the key pair.
kpiKeyName :: Lens' KeyPairInfo (Maybe Text)
kpiKeyName = lens _kpiKeyName (\ s a -> s{_kpiKeyName = a})

instance FromXML KeyPairInfo where
        parseXML x
          = KeyPairInfo' <$>
              (x .@? "keyFingerprint") <*> (x .@? "keyName")

instance NFData KeyPairInfo where

-- | Describes a launch permission.
--
--
--
-- /See:/ 'launchPermission' smart constructor.
data LaunchPermission = LaunchPermission'
  { _lGroup  :: !(Maybe PermissionGroup)
  , _lUserId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lGroup' - The name of the group.
--
-- * 'lUserId' - The AWS account ID.
launchPermission
    :: LaunchPermission
launchPermission = LaunchPermission' {_lGroup = Nothing, _lUserId = Nothing}


-- | The name of the group.
lGroup :: Lens' LaunchPermission (Maybe PermissionGroup)
lGroup = lens _lGroup (\ s a -> s{_lGroup = a})

-- | The AWS account ID.
lUserId :: Lens' LaunchPermission (Maybe Text)
lUserId = lens _lUserId (\ s a -> s{_lUserId = a})

instance FromXML LaunchPermission where
        parseXML x
          = LaunchPermission' <$>
              (x .@? "group") <*> (x .@? "userId")

instance NFData LaunchPermission where

instance ToQuery LaunchPermission where
        toQuery LaunchPermission'{..}
          = mconcat ["Group" =: _lGroup, "UserId" =: _lUserId]

-- | Describes a launch permission modification.
--
--
--
-- /See:/ 'launchPermissionModifications' smart constructor.
data LaunchPermissionModifications = LaunchPermissionModifications'
  { _lRemove :: !(Maybe [LaunchPermission])
  , _lAdd    :: !(Maybe [LaunchPermission])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchPermissionModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lRemove' - The AWS account ID to remove from the list of launch permissions for the AMI.
--
-- * 'lAdd' - The AWS account ID to add to the list of launch permissions for the AMI.
launchPermissionModifications
    :: LaunchPermissionModifications
launchPermissionModifications =
  LaunchPermissionModifications' {_lRemove = Nothing, _lAdd = Nothing}


-- | The AWS account ID to remove from the list of launch permissions for the AMI.
lRemove :: Lens' LaunchPermissionModifications [LaunchPermission]
lRemove = lens _lRemove (\ s a -> s{_lRemove = a}) . _Default . _Coerce

-- | The AWS account ID to add to the list of launch permissions for the AMI.
lAdd :: Lens' LaunchPermissionModifications [LaunchPermission]
lAdd = lens _lAdd (\ s a -> s{_lAdd = a}) . _Default . _Coerce

instance NFData LaunchPermissionModifications where

instance ToQuery LaunchPermissionModifications where
        toQuery LaunchPermissionModifications'{..}
          = mconcat
              [toQuery (toQueryList "Remove" <$> _lRemove),
               toQuery (toQueryList "Add" <$> _lAdd)]

-- | Describes the launch specification for an instance.
--
--
--
-- /See:/ 'launchSpecification' smart constructor.
data LaunchSpecification = LaunchSpecification'
  { _lsSecurityGroups      :: !(Maybe [GroupIdentifier])
  , _lsKeyName             :: !(Maybe Text)
  , _lsNetworkInterfaces   :: !(Maybe [InstanceNetworkInterfaceSpecification])
  , _lsRAMDiskId           :: !(Maybe Text)
  , _lsSubnetId            :: !(Maybe Text)
  , _lsKernelId            :: !(Maybe Text)
  , _lsInstanceType        :: !(Maybe InstanceType)
  , _lsEBSOptimized        :: !(Maybe Bool)
  , _lsUserData            :: !(Maybe Text)
  , _lsMonitoring          :: !(Maybe RunInstancesMonitoringEnabled)
  , _lsIAMInstanceProfile  :: !(Maybe IAMInstanceProfileSpecification)
  , _lsImageId             :: !(Maybe Text)
  , _lsAddressingType      :: !(Maybe Text)
  , _lsBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  , _lsPlacement           :: !(Maybe SpotPlacement)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsSecurityGroups' - One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- * 'lsKeyName' - The name of the key pair.
--
-- * 'lsNetworkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- * 'lsRAMDiskId' - The ID of the RAM disk.
--
-- * 'lsSubnetId' - The ID of the subnet in which to launch the instance.
--
-- * 'lsKernelId' - The ID of the kernel.
--
-- * 'lsInstanceType' - The instance type.
--
-- * 'lsEBSOptimized' - Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
--
-- * 'lsUserData' - The Base64-encoded user data for the instance.
--
-- * 'lsMonitoring' - Undocumented member.
--
-- * 'lsIAMInstanceProfile' - The IAM instance profile.
--
-- * 'lsImageId' - The ID of the AMI.
--
-- * 'lsAddressingType' - Deprecated.
--
-- * 'lsBlockDeviceMappings' - One or more block device mapping entries.
--
-- * 'lsPlacement' - The placement information for the instance.
launchSpecification
    :: LaunchSpecification
launchSpecification =
  LaunchSpecification'
    { _lsSecurityGroups = Nothing
    , _lsKeyName = Nothing
    , _lsNetworkInterfaces = Nothing
    , _lsRAMDiskId = Nothing
    , _lsSubnetId = Nothing
    , _lsKernelId = Nothing
    , _lsInstanceType = Nothing
    , _lsEBSOptimized = Nothing
    , _lsUserData = Nothing
    , _lsMonitoring = Nothing
    , _lsIAMInstanceProfile = Nothing
    , _lsImageId = Nothing
    , _lsAddressingType = Nothing
    , _lsBlockDeviceMappings = Nothing
    , _lsPlacement = Nothing
    }


-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
lsSecurityGroups :: Lens' LaunchSpecification [GroupIdentifier]
lsSecurityGroups = lens _lsSecurityGroups (\ s a -> s{_lsSecurityGroups = a}) . _Default . _Coerce

-- | The name of the key pair.
lsKeyName :: Lens' LaunchSpecification (Maybe Text)
lsKeyName = lens _lsKeyName (\ s a -> s{_lsKeyName = a})

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
lsNetworkInterfaces :: Lens' LaunchSpecification [InstanceNetworkInterfaceSpecification]
lsNetworkInterfaces = lens _lsNetworkInterfaces (\ s a -> s{_lsNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk.
lsRAMDiskId :: Lens' LaunchSpecification (Maybe Text)
lsRAMDiskId = lens _lsRAMDiskId (\ s a -> s{_lsRAMDiskId = a})

-- | The ID of the subnet in which to launch the instance.
lsSubnetId :: Lens' LaunchSpecification (Maybe Text)
lsSubnetId = lens _lsSubnetId (\ s a -> s{_lsSubnetId = a})

-- | The ID of the kernel.
lsKernelId :: Lens' LaunchSpecification (Maybe Text)
lsKernelId = lens _lsKernelId (\ s a -> s{_lsKernelId = a})

-- | The instance type.
lsInstanceType :: Lens' LaunchSpecification (Maybe InstanceType)
lsInstanceType = lens _lsInstanceType (\ s a -> s{_lsInstanceType = a})

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
lsEBSOptimized :: Lens' LaunchSpecification (Maybe Bool)
lsEBSOptimized = lens _lsEBSOptimized (\ s a -> s{_lsEBSOptimized = a})

-- | The Base64-encoded user data for the instance.
lsUserData :: Lens' LaunchSpecification (Maybe Text)
lsUserData = lens _lsUserData (\ s a -> s{_lsUserData = a})

-- | Undocumented member.
lsMonitoring :: Lens' LaunchSpecification (Maybe RunInstancesMonitoringEnabled)
lsMonitoring = lens _lsMonitoring (\ s a -> s{_lsMonitoring = a})

-- | The IAM instance profile.
lsIAMInstanceProfile :: Lens' LaunchSpecification (Maybe IAMInstanceProfileSpecification)
lsIAMInstanceProfile = lens _lsIAMInstanceProfile (\ s a -> s{_lsIAMInstanceProfile = a})

-- | The ID of the AMI.
lsImageId :: Lens' LaunchSpecification (Maybe Text)
lsImageId = lens _lsImageId (\ s a -> s{_lsImageId = a})

-- | Deprecated.
lsAddressingType :: Lens' LaunchSpecification (Maybe Text)
lsAddressingType = lens _lsAddressingType (\ s a -> s{_lsAddressingType = a})

-- | One or more block device mapping entries.
lsBlockDeviceMappings :: Lens' LaunchSpecification [BlockDeviceMapping]
lsBlockDeviceMappings = lens _lsBlockDeviceMappings (\ s a -> s{_lsBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement information for the instance.
lsPlacement :: Lens' LaunchSpecification (Maybe SpotPlacement)
lsPlacement = lens _lsPlacement (\ s a -> s{_lsPlacement = a})

instance FromXML LaunchSpecification where
        parseXML x
          = LaunchSpecification' <$>
              (x .@? "groupSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "keyName")
                <*>
                (x .@? "networkInterfaceSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "ramdiskId")
                <*> (x .@? "subnetId")
                <*> (x .@? "kernelId")
                <*> (x .@? "instanceType")
                <*> (x .@? "ebsOptimized")
                <*> (x .@? "userData")
                <*> (x .@? "monitoring")
                <*> (x .@? "iamInstanceProfile")
                <*> (x .@? "imageId")
                <*> (x .@? "addressingType")
                <*>
                (x .@? "blockDeviceMapping" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "placement")

instance NFData LaunchSpecification where

-- | Describes a launch template.
--
--
--
-- /See:/ 'launchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { _ltLaunchTemplateName   :: !(Maybe Text)
  , _ltLatestVersionNumber  :: !(Maybe Integer)
  , _ltLaunchTemplateId     :: !(Maybe Text)
  , _ltCreatedBy            :: !(Maybe Text)
  , _ltDefaultVersionNumber :: !(Maybe Integer)
  , _ltCreateTime           :: !(Maybe ISO8601)
  , _ltTags                 :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltLaunchTemplateName' - The name of the launch template.
--
-- * 'ltLatestVersionNumber' - The version number of the latest version of the launch template.
--
-- * 'ltLaunchTemplateId' - The ID of the launch template.
--
-- * 'ltCreatedBy' - The principal that created the launch template.
--
-- * 'ltDefaultVersionNumber' - The version number of the default version of the launch template.
--
-- * 'ltCreateTime' - The time launch template was created.
--
-- * 'ltTags' - The tags for the launch template.
launchTemplate
    :: LaunchTemplate
launchTemplate =
  LaunchTemplate'
    { _ltLaunchTemplateName = Nothing
    , _ltLatestVersionNumber = Nothing
    , _ltLaunchTemplateId = Nothing
    , _ltCreatedBy = Nothing
    , _ltDefaultVersionNumber = Nothing
    , _ltCreateTime = Nothing
    , _ltTags = Nothing
    }


-- | The name of the launch template.
ltLaunchTemplateName :: Lens' LaunchTemplate (Maybe Text)
ltLaunchTemplateName = lens _ltLaunchTemplateName (\ s a -> s{_ltLaunchTemplateName = a})

-- | The version number of the latest version of the launch template.
ltLatestVersionNumber :: Lens' LaunchTemplate (Maybe Integer)
ltLatestVersionNumber = lens _ltLatestVersionNumber (\ s a -> s{_ltLatestVersionNumber = a})

-- | The ID of the launch template.
ltLaunchTemplateId :: Lens' LaunchTemplate (Maybe Text)
ltLaunchTemplateId = lens _ltLaunchTemplateId (\ s a -> s{_ltLaunchTemplateId = a})

-- | The principal that created the launch template.
ltCreatedBy :: Lens' LaunchTemplate (Maybe Text)
ltCreatedBy = lens _ltCreatedBy (\ s a -> s{_ltCreatedBy = a})

-- | The version number of the default version of the launch template.
ltDefaultVersionNumber :: Lens' LaunchTemplate (Maybe Integer)
ltDefaultVersionNumber = lens _ltDefaultVersionNumber (\ s a -> s{_ltDefaultVersionNumber = a})

-- | The time launch template was created.
ltCreateTime :: Lens' LaunchTemplate (Maybe UTCTime)
ltCreateTime = lens _ltCreateTime (\ s a -> s{_ltCreateTime = a}) . mapping _Time

-- | The tags for the launch template.
ltTags :: Lens' LaunchTemplate [Tag]
ltTags = lens _ltTags (\ s a -> s{_ltTags = a}) . _Default . _Coerce

instance FromXML LaunchTemplate where
        parseXML x
          = LaunchTemplate' <$>
              (x .@? "launchTemplateName") <*>
                (x .@? "latestVersionNumber")
                <*> (x .@? "launchTemplateId")
                <*> (x .@? "createdBy")
                <*> (x .@? "defaultVersionNumber")
                <*> (x .@? "createTime")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData LaunchTemplate where

-- | Describes a block device mapping.
--
--
--
-- /See:/ 'launchTemplateBlockDeviceMapping' smart constructor.
data LaunchTemplateBlockDeviceMapping = LaunchTemplateBlockDeviceMapping'
  { _ltbdmVirtualName :: !(Maybe Text)
  , _ltbdmNoDevice    :: !(Maybe Text)
  , _ltbdmEBS         :: !(Maybe LaunchTemplateEBSBlockDevice)
  , _ltbdmDeviceName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateBlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltbdmVirtualName' - The virtual device name (ephemeralN).
--
-- * 'ltbdmNoDevice' - Suppresses the specified device included in the block device mapping of the AMI.
--
-- * 'ltbdmEBS' - Information about the block device for an EBS volume.
--
-- * 'ltbdmDeviceName' - The device name.
launchTemplateBlockDeviceMapping
    :: LaunchTemplateBlockDeviceMapping
launchTemplateBlockDeviceMapping =
  LaunchTemplateBlockDeviceMapping'
    { _ltbdmVirtualName = Nothing
    , _ltbdmNoDevice = Nothing
    , _ltbdmEBS = Nothing
    , _ltbdmDeviceName = Nothing
    }


-- | The virtual device name (ephemeralN).
ltbdmVirtualName :: Lens' LaunchTemplateBlockDeviceMapping (Maybe Text)
ltbdmVirtualName = lens _ltbdmVirtualName (\ s a -> s{_ltbdmVirtualName = a})

-- | Suppresses the specified device included in the block device mapping of the AMI.
ltbdmNoDevice :: Lens' LaunchTemplateBlockDeviceMapping (Maybe Text)
ltbdmNoDevice = lens _ltbdmNoDevice (\ s a -> s{_ltbdmNoDevice = a})

-- | Information about the block device for an EBS volume.
ltbdmEBS :: Lens' LaunchTemplateBlockDeviceMapping (Maybe LaunchTemplateEBSBlockDevice)
ltbdmEBS = lens _ltbdmEBS (\ s a -> s{_ltbdmEBS = a})

-- | The device name.
ltbdmDeviceName :: Lens' LaunchTemplateBlockDeviceMapping (Maybe Text)
ltbdmDeviceName = lens _ltbdmDeviceName (\ s a -> s{_ltbdmDeviceName = a})

instance FromXML LaunchTemplateBlockDeviceMapping
         where
        parseXML x
          = LaunchTemplateBlockDeviceMapping' <$>
              (x .@? "virtualName") <*> (x .@? "noDevice") <*>
                (x .@? "ebs")
                <*> (x .@? "deviceName")

instance NFData LaunchTemplateBlockDeviceMapping
         where

-- | Describes a block device mapping.
--
--
--
-- /See:/ 'launchTemplateBlockDeviceMappingRequest' smart constructor.
data LaunchTemplateBlockDeviceMappingRequest = LaunchTemplateBlockDeviceMappingRequest'
  { _ltbdmrVirtualName :: !(Maybe Text)
  , _ltbdmrNoDevice    :: !(Maybe Text)
  , _ltbdmrEBS         :: !(Maybe LaunchTemplateEBSBlockDeviceRequest)
  , _ltbdmrDeviceName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateBlockDeviceMappingRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltbdmrVirtualName' - The virtual device name (ephemeralN). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for ephemeral0 and ephemeral1. The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
--
-- * 'ltbdmrNoDevice' - Suppresses the specified device included in the block device mapping of the AMI.
--
-- * 'ltbdmrEBS' - Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- * 'ltbdmrDeviceName' - The device name (for example, /dev/sdh or xvdh).
launchTemplateBlockDeviceMappingRequest
    :: LaunchTemplateBlockDeviceMappingRequest
launchTemplateBlockDeviceMappingRequest =
  LaunchTemplateBlockDeviceMappingRequest'
    { _ltbdmrVirtualName = Nothing
    , _ltbdmrNoDevice = Nothing
    , _ltbdmrEBS = Nothing
    , _ltbdmrDeviceName = Nothing
    }


-- | The virtual device name (ephemeralN). Instance store volumes are numbered starting from 0. An instance type with 2 available instance store volumes can specify mappings for ephemeral0 and ephemeral1. The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume.
ltbdmrVirtualName :: Lens' LaunchTemplateBlockDeviceMappingRequest (Maybe Text)
ltbdmrVirtualName = lens _ltbdmrVirtualName (\ s a -> s{_ltbdmrVirtualName = a})

-- | Suppresses the specified device included in the block device mapping of the AMI.
ltbdmrNoDevice :: Lens' LaunchTemplateBlockDeviceMappingRequest (Maybe Text)
ltbdmrNoDevice = lens _ltbdmrNoDevice (\ s a -> s{_ltbdmrNoDevice = a})

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
ltbdmrEBS :: Lens' LaunchTemplateBlockDeviceMappingRequest (Maybe LaunchTemplateEBSBlockDeviceRequest)
ltbdmrEBS = lens _ltbdmrEBS (\ s a -> s{_ltbdmrEBS = a})

-- | The device name (for example, /dev/sdh or xvdh).
ltbdmrDeviceName :: Lens' LaunchTemplateBlockDeviceMappingRequest (Maybe Text)
ltbdmrDeviceName = lens _ltbdmrDeviceName (\ s a -> s{_ltbdmrDeviceName = a})

instance NFData
           LaunchTemplateBlockDeviceMappingRequest
         where

instance ToQuery
           LaunchTemplateBlockDeviceMappingRequest
         where
        toQuery LaunchTemplateBlockDeviceMappingRequest'{..}
          = mconcat
              ["VirtualName" =: _ltbdmrVirtualName,
               "NoDevice" =: _ltbdmrNoDevice, "Ebs" =: _ltbdmrEBS,
               "DeviceName" =: _ltbdmrDeviceName]

-- | Describes a launch template and overrides.
--
--
--
-- /See:/ 'launchTemplateConfig' smart constructor.
data LaunchTemplateConfig = LaunchTemplateConfig'
  { _ltcOverrides                   :: !(Maybe [LaunchTemplateOverrides])
  , _ltcLaunchTemplateSpecification :: !(Maybe FleetLaunchTemplateSpecification)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltcOverrides' - Any parameters that you specify override the same parameters in the launch template.
--
-- * 'ltcLaunchTemplateSpecification' - The launch template.
launchTemplateConfig
    :: LaunchTemplateConfig
launchTemplateConfig =
  LaunchTemplateConfig'
    {_ltcOverrides = Nothing, _ltcLaunchTemplateSpecification = Nothing}


-- | Any parameters that you specify override the same parameters in the launch template.
ltcOverrides :: Lens' LaunchTemplateConfig [LaunchTemplateOverrides]
ltcOverrides = lens _ltcOverrides (\ s a -> s{_ltcOverrides = a}) . _Default . _Coerce

-- | The launch template.
ltcLaunchTemplateSpecification :: Lens' LaunchTemplateConfig (Maybe FleetLaunchTemplateSpecification)
ltcLaunchTemplateSpecification = lens _ltcLaunchTemplateSpecification (\ s a -> s{_ltcLaunchTemplateSpecification = a})

instance FromXML LaunchTemplateConfig where
        parseXML x
          = LaunchTemplateConfig' <$>
              (x .@? "overrides" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "launchTemplateSpecification")

instance NFData LaunchTemplateConfig where

instance ToQuery LaunchTemplateConfig where
        toQuery LaunchTemplateConfig'{..}
          = mconcat
              [toQuery (toQueryList "Overrides" <$> _ltcOverrides),
               "LaunchTemplateSpecification" =:
                 _ltcLaunchTemplateSpecification]

-- | Describes a block device for an EBS volume.
--
--
--
-- /See:/ 'launchTemplateEBSBlockDevice' smart constructor.
data LaunchTemplateEBSBlockDevice = LaunchTemplateEBSBlockDevice'
  { _ltebdDeleteOnTermination :: !(Maybe Bool)
  , _ltebdVolumeSize          :: !(Maybe Int)
  , _ltebdIOPS                :: !(Maybe Int)
  , _ltebdEncrypted           :: !(Maybe Bool)
  , _ltebdKMSKeyId            :: !(Maybe Text)
  , _ltebdVolumeType          :: !(Maybe VolumeType)
  , _ltebdSnapshotId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateEBSBlockDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltebdDeleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- * 'ltebdVolumeSize' - The size of the volume, in GiB.
--
-- * 'ltebdIOPS' - The number of I/O operations per second (IOPS) that the volume supports.
--
-- * 'ltebdEncrypted' - Indicates whether the EBS volume is encrypted.
--
-- * 'ltebdKMSKeyId' - The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
--
-- * 'ltebdVolumeType' - The volume type.
--
-- * 'ltebdSnapshotId' - The ID of the snapshot.
launchTemplateEBSBlockDevice
    :: LaunchTemplateEBSBlockDevice
launchTemplateEBSBlockDevice =
  LaunchTemplateEBSBlockDevice'
    { _ltebdDeleteOnTermination = Nothing
    , _ltebdVolumeSize = Nothing
    , _ltebdIOPS = Nothing
    , _ltebdEncrypted = Nothing
    , _ltebdKMSKeyId = Nothing
    , _ltebdVolumeType = Nothing
    , _ltebdSnapshotId = Nothing
    }


-- | Indicates whether the EBS volume is deleted on instance termination.
ltebdDeleteOnTermination :: Lens' LaunchTemplateEBSBlockDevice (Maybe Bool)
ltebdDeleteOnTermination = lens _ltebdDeleteOnTermination (\ s a -> s{_ltebdDeleteOnTermination = a})

-- | The size of the volume, in GiB.
ltebdVolumeSize :: Lens' LaunchTemplateEBSBlockDevice (Maybe Int)
ltebdVolumeSize = lens _ltebdVolumeSize (\ s a -> s{_ltebdVolumeSize = a})

-- | The number of I/O operations per second (IOPS) that the volume supports.
ltebdIOPS :: Lens' LaunchTemplateEBSBlockDevice (Maybe Int)
ltebdIOPS = lens _ltebdIOPS (\ s a -> s{_ltebdIOPS = a})

-- | Indicates whether the EBS volume is encrypted.
ltebdEncrypted :: Lens' LaunchTemplateEBSBlockDevice (Maybe Bool)
ltebdEncrypted = lens _ltebdEncrypted (\ s a -> s{_ltebdEncrypted = a})

-- | The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
ltebdKMSKeyId :: Lens' LaunchTemplateEBSBlockDevice (Maybe Text)
ltebdKMSKeyId = lens _ltebdKMSKeyId (\ s a -> s{_ltebdKMSKeyId = a})

-- | The volume type.
ltebdVolumeType :: Lens' LaunchTemplateEBSBlockDevice (Maybe VolumeType)
ltebdVolumeType = lens _ltebdVolumeType (\ s a -> s{_ltebdVolumeType = a})

-- | The ID of the snapshot.
ltebdSnapshotId :: Lens' LaunchTemplateEBSBlockDevice (Maybe Text)
ltebdSnapshotId = lens _ltebdSnapshotId (\ s a -> s{_ltebdSnapshotId = a})

instance FromXML LaunchTemplateEBSBlockDevice where
        parseXML x
          = LaunchTemplateEBSBlockDevice' <$>
              (x .@? "deleteOnTermination") <*>
                (x .@? "volumeSize")
                <*> (x .@? "iops")
                <*> (x .@? "encrypted")
                <*> (x .@? "kmsKeyId")
                <*> (x .@? "volumeType")
                <*> (x .@? "snapshotId")

instance NFData LaunchTemplateEBSBlockDevice where

-- | The parameters for a block device for an EBS volume.
--
--
--
-- /See:/ 'launchTemplateEBSBlockDeviceRequest' smart constructor.
data LaunchTemplateEBSBlockDeviceRequest = LaunchTemplateEBSBlockDeviceRequest'
  { _ltebdrDeleteOnTermination :: !(Maybe Bool)
  , _ltebdrVolumeSize          :: !(Maybe Int)
  , _ltebdrIOPS                :: !(Maybe Int)
  , _ltebdrEncrypted           :: !(Maybe Bool)
  , _ltebdrKMSKeyId            :: !(Maybe Text)
  , _ltebdrVolumeType          :: !(Maybe VolumeType)
  , _ltebdrSnapshotId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateEBSBlockDeviceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltebdrDeleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- * 'ltebdrVolumeSize' - The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'ltebdrIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For io1, this represents the number of IOPS that are provisioned for the volume. For gp2, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about General Purpose SSD baseline performance, I/O credits, and bursting, see Amazon EBS Volume Types in the Amazon Elastic Compute Cloud User Guide. Condition: This parameter is required for requests to create io1 volumes; it is not used in requests to create gp2, st1, sc1, or standard volumes.
--
-- * 'ltebdrEncrypted' - Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
--
-- * 'ltebdrKMSKeyId' - The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
--
-- * 'ltebdrVolumeType' - The volume type.
--
-- * 'ltebdrSnapshotId' - The ID of the snapshot.
launchTemplateEBSBlockDeviceRequest
    :: LaunchTemplateEBSBlockDeviceRequest
launchTemplateEBSBlockDeviceRequest =
  LaunchTemplateEBSBlockDeviceRequest'
    { _ltebdrDeleteOnTermination = Nothing
    , _ltebdrVolumeSize = Nothing
    , _ltebdrIOPS = Nothing
    , _ltebdrEncrypted = Nothing
    , _ltebdrKMSKeyId = Nothing
    , _ltebdrVolumeType = Nothing
    , _ltebdrSnapshotId = Nothing
    }


-- | Indicates whether the EBS volume is deleted on instance termination.
ltebdrDeleteOnTermination :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Bool)
ltebdrDeleteOnTermination = lens _ltebdrDeleteOnTermination (\ s a -> s{_ltebdrDeleteOnTermination = a})

-- | The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
ltebdrVolumeSize :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Int)
ltebdrVolumeSize = lens _ltebdrVolumeSize (\ s a -> s{_ltebdrVolumeSize = a})

-- | The number of I/O operations per second (IOPS) that the volume supports. For io1, this represents the number of IOPS that are provisioned for the volume. For gp2, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about General Purpose SSD baseline performance, I/O credits, and bursting, see Amazon EBS Volume Types in the Amazon Elastic Compute Cloud User Guide. Condition: This parameter is required for requests to create io1 volumes; it is not used in requests to create gp2, st1, sc1, or standard volumes.
ltebdrIOPS :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Int)
ltebdrIOPS = lens _ltebdrIOPS (\ s a -> s{_ltebdrIOPS = a})

-- | Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
ltebdrEncrypted :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Bool)
ltebdrEncrypted = lens _ltebdrEncrypted (\ s a -> s{_ltebdrEncrypted = a})

-- | The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
ltebdrKMSKeyId :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Text)
ltebdrKMSKeyId = lens _ltebdrKMSKeyId (\ s a -> s{_ltebdrKMSKeyId = a})

-- | The volume type.
ltebdrVolumeType :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe VolumeType)
ltebdrVolumeType = lens _ltebdrVolumeType (\ s a -> s{_ltebdrVolumeType = a})

-- | The ID of the snapshot.
ltebdrSnapshotId :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Text)
ltebdrSnapshotId = lens _ltebdrSnapshotId (\ s a -> s{_ltebdrSnapshotId = a})

instance NFData LaunchTemplateEBSBlockDeviceRequest
         where

instance ToQuery LaunchTemplateEBSBlockDeviceRequest
         where
        toQuery LaunchTemplateEBSBlockDeviceRequest'{..}
          = mconcat
              ["DeleteOnTermination" =: _ltebdrDeleteOnTermination,
               "VolumeSize" =: _ltebdrVolumeSize,
               "Iops" =: _ltebdrIOPS,
               "Encrypted" =: _ltebdrEncrypted,
               "KmsKeyId" =: _ltebdrKMSKeyId,
               "VolumeType" =: _ltebdrVolumeType,
               "SnapshotId" =: _ltebdrSnapshotId]

-- | Describes an IAM instance profile.
--
--
--
-- /See:/ 'launchTemplateIAMInstanceProfileSpecification' smart constructor.
data LaunchTemplateIAMInstanceProfileSpecification = LaunchTemplateIAMInstanceProfileSpecification'
  { _ltiapsARN  :: !(Maybe Text)
  , _ltiapsName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateIAMInstanceProfileSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltiapsARN' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'ltiapsName' - The name of the instance profile.
launchTemplateIAMInstanceProfileSpecification
    :: LaunchTemplateIAMInstanceProfileSpecification
launchTemplateIAMInstanceProfileSpecification =
  LaunchTemplateIAMInstanceProfileSpecification'
    {_ltiapsARN = Nothing, _ltiapsName = Nothing}


-- | The Amazon Resource Name (ARN) of the instance profile.
ltiapsARN :: Lens' LaunchTemplateIAMInstanceProfileSpecification (Maybe Text)
ltiapsARN = lens _ltiapsARN (\ s a -> s{_ltiapsARN = a})

-- | The name of the instance profile.
ltiapsName :: Lens' LaunchTemplateIAMInstanceProfileSpecification (Maybe Text)
ltiapsName = lens _ltiapsName (\ s a -> s{_ltiapsName = a})

instance FromXML
           LaunchTemplateIAMInstanceProfileSpecification
         where
        parseXML x
          = LaunchTemplateIAMInstanceProfileSpecification' <$>
              (x .@? "arn") <*> (x .@? "name")

instance NFData
           LaunchTemplateIAMInstanceProfileSpecification
         where

-- | An IAM instance profile.
--
--
--
-- /See:/ 'launchTemplateIAMInstanceProfileSpecificationRequest' smart constructor.
data LaunchTemplateIAMInstanceProfileSpecificationRequest = LaunchTemplateIAMInstanceProfileSpecificationRequest'
  { _ltiapsrARN  :: !(Maybe Text)
  , _ltiapsrName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateIAMInstanceProfileSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltiapsrARN' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'ltiapsrName' - The name of the instance profile.
launchTemplateIAMInstanceProfileSpecificationRequest
    :: LaunchTemplateIAMInstanceProfileSpecificationRequest
launchTemplateIAMInstanceProfileSpecificationRequest =
  LaunchTemplateIAMInstanceProfileSpecificationRequest'
    {_ltiapsrARN = Nothing, _ltiapsrName = Nothing}


-- | The Amazon Resource Name (ARN) of the instance profile.
ltiapsrARN :: Lens' LaunchTemplateIAMInstanceProfileSpecificationRequest (Maybe Text)
ltiapsrARN = lens _ltiapsrARN (\ s a -> s{_ltiapsrARN = a})

-- | The name of the instance profile.
ltiapsrName :: Lens' LaunchTemplateIAMInstanceProfileSpecificationRequest (Maybe Text)
ltiapsrName = lens _ltiapsrName (\ s a -> s{_ltiapsrName = a})

instance NFData
           LaunchTemplateIAMInstanceProfileSpecificationRequest
         where

instance ToQuery
           LaunchTemplateIAMInstanceProfileSpecificationRequest
         where
        toQuery
          LaunchTemplateIAMInstanceProfileSpecificationRequest'{..}
          = mconcat
              ["Arn" =: _ltiapsrARN, "Name" =: _ltiapsrName]

-- | The market (purchasing) option for the instances.
--
--
--
-- /See:/ 'launchTemplateInstanceMarketOptions' smart constructor.
data LaunchTemplateInstanceMarketOptions = LaunchTemplateInstanceMarketOptions'
  { _ltimoMarketType  :: !(Maybe MarketType)
  , _ltimoSpotOptions :: !(Maybe LaunchTemplateSpotMarketOptions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateInstanceMarketOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltimoMarketType' - The market type.
--
-- * 'ltimoSpotOptions' - The options for Spot Instances.
launchTemplateInstanceMarketOptions
    :: LaunchTemplateInstanceMarketOptions
launchTemplateInstanceMarketOptions =
  LaunchTemplateInstanceMarketOptions'
    {_ltimoMarketType = Nothing, _ltimoSpotOptions = Nothing}


-- | The market type.
ltimoMarketType :: Lens' LaunchTemplateInstanceMarketOptions (Maybe MarketType)
ltimoMarketType = lens _ltimoMarketType (\ s a -> s{_ltimoMarketType = a})

-- | The options for Spot Instances.
ltimoSpotOptions :: Lens' LaunchTemplateInstanceMarketOptions (Maybe LaunchTemplateSpotMarketOptions)
ltimoSpotOptions = lens _ltimoSpotOptions (\ s a -> s{_ltimoSpotOptions = a})

instance FromXML LaunchTemplateInstanceMarketOptions
         where
        parseXML x
          = LaunchTemplateInstanceMarketOptions' <$>
              (x .@? "marketType") <*> (x .@? "spotOptions")

instance NFData LaunchTemplateInstanceMarketOptions
         where

-- | The market (purchasing) option for the instances.
--
--
--
-- /See:/ 'launchTemplateInstanceMarketOptionsRequest' smart constructor.
data LaunchTemplateInstanceMarketOptionsRequest = LaunchTemplateInstanceMarketOptionsRequest'
  { _ltimorMarketType  :: !(Maybe MarketType)
  , _ltimorSpotOptions :: !(Maybe LaunchTemplateSpotMarketOptionsRequest)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateInstanceMarketOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltimorMarketType' - The market type.
--
-- * 'ltimorSpotOptions' - The options for Spot Instances.
launchTemplateInstanceMarketOptionsRequest
    :: LaunchTemplateInstanceMarketOptionsRequest
launchTemplateInstanceMarketOptionsRequest =
  LaunchTemplateInstanceMarketOptionsRequest'
    {_ltimorMarketType = Nothing, _ltimorSpotOptions = Nothing}


-- | The market type.
ltimorMarketType :: Lens' LaunchTemplateInstanceMarketOptionsRequest (Maybe MarketType)
ltimorMarketType = lens _ltimorMarketType (\ s a -> s{_ltimorMarketType = a})

-- | The options for Spot Instances.
ltimorSpotOptions :: Lens' LaunchTemplateInstanceMarketOptionsRequest (Maybe LaunchTemplateSpotMarketOptionsRequest)
ltimorSpotOptions = lens _ltimorSpotOptions (\ s a -> s{_ltimorSpotOptions = a})

instance NFData
           LaunchTemplateInstanceMarketOptionsRequest
         where

instance ToQuery
           LaunchTemplateInstanceMarketOptionsRequest
         where
        toQuery
          LaunchTemplateInstanceMarketOptionsRequest'{..}
          = mconcat
              ["MarketType" =: _ltimorMarketType,
               "SpotOptions" =: _ltimorSpotOptions]

-- | Describes a network interface.
--
--
--
-- /See:/ 'launchTemplateInstanceNetworkInterfaceSpecification' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecification = LaunchTemplateInstanceNetworkInterfaceSpecification'
  { _ltinisGroups :: !(Maybe [Text])
  , _ltinisPrivateIPAddresses :: !(Maybe [PrivateIPAddressSpecification])
  , _ltinisDeleteOnTermination :: !(Maybe Bool)
  , _ltinisAssociatePublicIPAddress :: !(Maybe Bool)
  , _ltinisNetworkInterfaceId :: !(Maybe Text)
  , _ltinisSubnetId :: !(Maybe Text)
  , _ltinisIPv6AddressCount :: !(Maybe Int)
  , _ltinisPrivateIPAddress :: !(Maybe Text)
  , _ltinisSecondaryPrivateIPAddressCount :: !(Maybe Int)
  , _ltinisDescription :: !(Maybe Text)
  , _ltinisDeviceIndex :: !(Maybe Int)
  , _ltinisIPv6Addresses :: !(Maybe [InstanceIPv6Address])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateInstanceNetworkInterfaceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltinisGroups' - The IDs of one or more security groups.
--
-- * 'ltinisPrivateIPAddresses' - One or more private IPv4 addresses.
--
-- * 'ltinisDeleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
--
-- * 'ltinisAssociatePublicIPAddress' - Indicates whether to associate a public IPv4 address with eth0 for a new network interface.
--
-- * 'ltinisNetworkInterfaceId' - The ID of the network interface.
--
-- * 'ltinisSubnetId' - The ID of the subnet for the network interface.
--
-- * 'ltinisIPv6AddressCount' - The number of IPv6 addresses for the network interface.
--
-- * 'ltinisPrivateIPAddress' - The primary private IPv4 address of the network interface.
--
-- * 'ltinisSecondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses for the network interface.
--
-- * 'ltinisDescription' - A description for the network interface.
--
-- * 'ltinisDeviceIndex' - The device index for the network interface attachment.
--
-- * 'ltinisIPv6Addresses' - The IPv6 addresses for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification
    :: LaunchTemplateInstanceNetworkInterfaceSpecification
launchTemplateInstanceNetworkInterfaceSpecification =
  LaunchTemplateInstanceNetworkInterfaceSpecification'
    { _ltinisGroups = Nothing
    , _ltinisPrivateIPAddresses = Nothing
    , _ltinisDeleteOnTermination = Nothing
    , _ltinisAssociatePublicIPAddress = Nothing
    , _ltinisNetworkInterfaceId = Nothing
    , _ltinisSubnetId = Nothing
    , _ltinisIPv6AddressCount = Nothing
    , _ltinisPrivateIPAddress = Nothing
    , _ltinisSecondaryPrivateIPAddressCount = Nothing
    , _ltinisDescription = Nothing
    , _ltinisDeviceIndex = Nothing
    , _ltinisIPv6Addresses = Nothing
    }


-- | The IDs of one or more security groups.
ltinisGroups :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification [Text]
ltinisGroups = lens _ltinisGroups (\ s a -> s{_ltinisGroups = a}) . _Default . _Coerce

-- | One or more private IPv4 addresses.
ltinisPrivateIPAddresses :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification [PrivateIPAddressSpecification]
ltinisPrivateIPAddresses = lens _ltinisPrivateIPAddresses (\ s a -> s{_ltinisPrivateIPAddresses = a}) . _Default . _Coerce

-- | Indicates whether the network interface is deleted when the instance is terminated.
ltinisDeleteOnTermination :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Bool)
ltinisDeleteOnTermination = lens _ltinisDeleteOnTermination (\ s a -> s{_ltinisDeleteOnTermination = a})

-- | Indicates whether to associate a public IPv4 address with eth0 for a new network interface.
ltinisAssociatePublicIPAddress :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Bool)
ltinisAssociatePublicIPAddress = lens _ltinisAssociatePublicIPAddress (\ s a -> s{_ltinisAssociatePublicIPAddress = a})

-- | The ID of the network interface.
ltinisNetworkInterfaceId :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Text)
ltinisNetworkInterfaceId = lens _ltinisNetworkInterfaceId (\ s a -> s{_ltinisNetworkInterfaceId = a})

-- | The ID of the subnet for the network interface.
ltinisSubnetId :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Text)
ltinisSubnetId = lens _ltinisSubnetId (\ s a -> s{_ltinisSubnetId = a})

-- | The number of IPv6 addresses for the network interface.
ltinisIPv6AddressCount :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Int)
ltinisIPv6AddressCount = lens _ltinisIPv6AddressCount (\ s a -> s{_ltinisIPv6AddressCount = a})

-- | The primary private IPv4 address of the network interface.
ltinisPrivateIPAddress :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Text)
ltinisPrivateIPAddress = lens _ltinisPrivateIPAddress (\ s a -> s{_ltinisPrivateIPAddress = a})

-- | The number of secondary private IPv4 addresses for the network interface.
ltinisSecondaryPrivateIPAddressCount :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Int)
ltinisSecondaryPrivateIPAddressCount = lens _ltinisSecondaryPrivateIPAddressCount (\ s a -> s{_ltinisSecondaryPrivateIPAddressCount = a})

-- | A description for the network interface.
ltinisDescription :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Text)
ltinisDescription = lens _ltinisDescription (\ s a -> s{_ltinisDescription = a})

-- | The device index for the network interface attachment.
ltinisDeviceIndex :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Maybe Int)
ltinisDeviceIndex = lens _ltinisDeviceIndex (\ s a -> s{_ltinisDeviceIndex = a})

-- | The IPv6 addresses for the network interface.
ltinisIPv6Addresses :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecification [InstanceIPv6Address]
ltinisIPv6Addresses = lens _ltinisIPv6Addresses (\ s a -> s{_ltinisIPv6Addresses = a}) . _Default . _Coerce

instance FromXML
           LaunchTemplateInstanceNetworkInterfaceSpecification
         where
        parseXML x
          = LaunchTemplateInstanceNetworkInterfaceSpecification'
              <$>
              (x .@? "groupSet" .!@ mempty >>=
                 may (parseXMLList "groupId"))
                <*>
                (x .@? "privateIpAddressesSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "deleteOnTermination")
                <*> (x .@? "associatePublicIpAddress")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "subnetId")
                <*> (x .@? "ipv6AddressCount")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "secondaryPrivateIpAddressCount")
                <*> (x .@? "description")
                <*> (x .@? "deviceIndex")
                <*>
                (x .@? "ipv6AddressesSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData
           LaunchTemplateInstanceNetworkInterfaceSpecification
         where

-- | The parameters for a network interface.
--
--
--
-- /See:/ 'launchTemplateInstanceNetworkInterfaceSpecificationRequest' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecificationRequest = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
  { _ltinisrGroups :: !(Maybe [Text])
  , _ltinisrPrivateIPAddresses :: !(Maybe [PrivateIPAddressSpecification])
  , _ltinisrDeleteOnTermination :: !(Maybe Bool)
  , _ltinisrAssociatePublicIPAddress :: !(Maybe Bool)
  , _ltinisrNetworkInterfaceId :: !(Maybe Text)
  , _ltinisrSubnetId :: !(Maybe Text)
  , _ltinisrIPv6AddressCount :: !(Maybe Int)
  , _ltinisrPrivateIPAddress :: !(Maybe Text)
  , _ltinisrSecondaryPrivateIPAddressCount :: !(Maybe Int)
  , _ltinisrDescription :: !(Maybe Text)
  , _ltinisrDeviceIndex :: !(Maybe Int)
  , _ltinisrIPv6Addresses :: !(Maybe [InstanceIPv6AddressRequest])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltinisrGroups' - The IDs of one or more security groups.
--
-- * 'ltinisrPrivateIPAddresses' - One or more private IPv4 addresses.
--
-- * 'ltinisrDeleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
--
-- * 'ltinisrAssociatePublicIPAddress' - Associates a public IPv4 address with eth0 for a new network interface.
--
-- * 'ltinisrNetworkInterfaceId' - The ID of the network interface.
--
-- * 'ltinisrSubnetId' - The ID of the subnet for the network interface.
--
-- * 'ltinisrIPv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
--
-- * 'ltinisrPrivateIPAddress' - The primary private IPv4 address of the network interface.
--
-- * 'ltinisrSecondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses to assign to a network interface.
--
-- * 'ltinisrDescription' - A description for the network interface.
--
-- * 'ltinisrDeviceIndex' - The device index for the network interface attachment.
--
-- * 'ltinisrIPv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest
    :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
launchTemplateInstanceNetworkInterfaceSpecificationRequest =
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
    { _ltinisrGroups = Nothing
    , _ltinisrPrivateIPAddresses = Nothing
    , _ltinisrDeleteOnTermination = Nothing
    , _ltinisrAssociatePublicIPAddress = Nothing
    , _ltinisrNetworkInterfaceId = Nothing
    , _ltinisrSubnetId = Nothing
    , _ltinisrIPv6AddressCount = Nothing
    , _ltinisrPrivateIPAddress = Nothing
    , _ltinisrSecondaryPrivateIPAddressCount = Nothing
    , _ltinisrDescription = Nothing
    , _ltinisrDeviceIndex = Nothing
    , _ltinisrIPv6Addresses = Nothing
    }


-- | The IDs of one or more security groups.
ltinisrGroups :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest [Text]
ltinisrGroups = lens _ltinisrGroups (\ s a -> s{_ltinisrGroups = a}) . _Default . _Coerce

-- | One or more private IPv4 addresses.
ltinisrPrivateIPAddresses :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest [PrivateIPAddressSpecification]
ltinisrPrivateIPAddresses = lens _ltinisrPrivateIPAddresses (\ s a -> s{_ltinisrPrivateIPAddresses = a}) . _Default . _Coerce

-- | Indicates whether the network interface is deleted when the instance is terminated.
ltinisrDeleteOnTermination :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Bool)
ltinisrDeleteOnTermination = lens _ltinisrDeleteOnTermination (\ s a -> s{_ltinisrDeleteOnTermination = a})

-- | Associates a public IPv4 address with eth0 for a new network interface.
ltinisrAssociatePublicIPAddress :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Bool)
ltinisrAssociatePublicIPAddress = lens _ltinisrAssociatePublicIPAddress (\ s a -> s{_ltinisrAssociatePublicIPAddress = a})

-- | The ID of the network interface.
ltinisrNetworkInterfaceId :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrNetworkInterfaceId = lens _ltinisrNetworkInterfaceId (\ s a -> s{_ltinisrNetworkInterfaceId = a})

-- | The ID of the subnet for the network interface.
ltinisrSubnetId :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrSubnetId = lens _ltinisrSubnetId (\ s a -> s{_ltinisrSubnetId = a})

-- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
ltinisrIPv6AddressCount :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Int)
ltinisrIPv6AddressCount = lens _ltinisrIPv6AddressCount (\ s a -> s{_ltinisrIPv6AddressCount = a})

-- | The primary private IPv4 address of the network interface.
ltinisrPrivateIPAddress :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrPrivateIPAddress = lens _ltinisrPrivateIPAddress (\ s a -> s{_ltinisrPrivateIPAddress = a})

-- | The number of secondary private IPv4 addresses to assign to a network interface.
ltinisrSecondaryPrivateIPAddressCount :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Int)
ltinisrSecondaryPrivateIPAddressCount = lens _ltinisrSecondaryPrivateIPAddressCount (\ s a -> s{_ltinisrSecondaryPrivateIPAddressCount = a})

-- | A description for the network interface.
ltinisrDescription :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Text)
ltinisrDescription = lens _ltinisrDescription (\ s a -> s{_ltinisrDescription = a})

-- | The device index for the network interface attachment.
ltinisrDeviceIndex :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Maybe Int)
ltinisrDeviceIndex = lens _ltinisrDeviceIndex (\ s a -> s{_ltinisrDeviceIndex = a})

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
ltinisrIPv6Addresses :: Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest [InstanceIPv6AddressRequest]
ltinisrIPv6Addresses = lens _ltinisrIPv6Addresses (\ s a -> s{_ltinisrIPv6Addresses = a}) . _Default . _Coerce

instance NFData
           LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
         where

instance ToQuery
           LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
         where
        toQuery
          LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'{..}
          = mconcat
              [toQuery
                 (toQueryList "SecurityGroupId" <$> _ltinisrGroups),
               toQuery
                 (toQueryList "PrivateIpAddresses" <$>
                    _ltinisrPrivateIPAddresses),
               "DeleteOnTermination" =: _ltinisrDeleteOnTermination,
               "AssociatePublicIpAddress" =:
                 _ltinisrAssociatePublicIPAddress,
               "NetworkInterfaceId" =: _ltinisrNetworkInterfaceId,
               "SubnetId" =: _ltinisrSubnetId,
               "Ipv6AddressCount" =: _ltinisrIPv6AddressCount,
               "PrivateIpAddress" =: _ltinisrPrivateIPAddress,
               "SecondaryPrivateIpAddressCount" =:
                 _ltinisrSecondaryPrivateIPAddressCount,
               "Description" =: _ltinisrDescription,
               "DeviceIndex" =: _ltinisrDeviceIndex,
               toQuery
                 (toQueryList "Ipv6Addresses" <$>
                    _ltinisrIPv6Addresses)]

-- | Describes overrides for a launch template.
--
--
--
-- /See:/ 'launchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { _ltoSpotPrice        :: !(Maybe Text)
  , _ltoWeightedCapacity :: !(Maybe Double)
  , _ltoSubnetId         :: !(Maybe Text)
  , _ltoInstanceType     :: !(Maybe InstanceType)
  , _ltoAvailabilityZone :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateOverrides' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltoSpotPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance.
--
-- * 'ltoWeightedCapacity' - The number of units provided by the specified instance type.
--
-- * 'ltoSubnetId' - The ID of the subnet in which to launch the instances.
--
-- * 'ltoInstanceType' - The instance type.
--
-- * 'ltoAvailabilityZone' - The Availability Zone in which to launch the instances.
launchTemplateOverrides
    :: LaunchTemplateOverrides
launchTemplateOverrides =
  LaunchTemplateOverrides'
    { _ltoSpotPrice = Nothing
    , _ltoWeightedCapacity = Nothing
    , _ltoSubnetId = Nothing
    , _ltoInstanceType = Nothing
    , _ltoAvailabilityZone = Nothing
    }


-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
ltoSpotPrice :: Lens' LaunchTemplateOverrides (Maybe Text)
ltoSpotPrice = lens _ltoSpotPrice (\ s a -> s{_ltoSpotPrice = a})

-- | The number of units provided by the specified instance type.
ltoWeightedCapacity :: Lens' LaunchTemplateOverrides (Maybe Double)
ltoWeightedCapacity = lens _ltoWeightedCapacity (\ s a -> s{_ltoWeightedCapacity = a})

-- | The ID of the subnet in which to launch the instances.
ltoSubnetId :: Lens' LaunchTemplateOverrides (Maybe Text)
ltoSubnetId = lens _ltoSubnetId (\ s a -> s{_ltoSubnetId = a})

-- | The instance type.
ltoInstanceType :: Lens' LaunchTemplateOverrides (Maybe InstanceType)
ltoInstanceType = lens _ltoInstanceType (\ s a -> s{_ltoInstanceType = a})

-- | The Availability Zone in which to launch the instances.
ltoAvailabilityZone :: Lens' LaunchTemplateOverrides (Maybe Text)
ltoAvailabilityZone = lens _ltoAvailabilityZone (\ s a -> s{_ltoAvailabilityZone = a})

instance FromXML LaunchTemplateOverrides where
        parseXML x
          = LaunchTemplateOverrides' <$>
              (x .@? "spotPrice") <*> (x .@? "weightedCapacity")
                <*> (x .@? "subnetId")
                <*> (x .@? "instanceType")
                <*> (x .@? "availabilityZone")

instance NFData LaunchTemplateOverrides where

instance ToQuery LaunchTemplateOverrides where
        toQuery LaunchTemplateOverrides'{..}
          = mconcat
              ["SpotPrice" =: _ltoSpotPrice,
               "WeightedCapacity" =: _ltoWeightedCapacity,
               "SubnetId" =: _ltoSubnetId,
               "InstanceType" =: _ltoInstanceType,
               "AvailabilityZone" =: _ltoAvailabilityZone]

-- | Describes the placement of an instance.
--
--
--
-- /See:/ 'launchTemplatePlacement' smart constructor.
data LaunchTemplatePlacement = LaunchTemplatePlacement'
  { _ltpAffinity         :: !(Maybe Text)
  , _ltpHostId           :: !(Maybe Text)
  , _ltpSpreadDomain     :: !(Maybe Text)
  , _ltpAvailabilityZone :: !(Maybe Text)
  , _ltpTenancy          :: !(Maybe Tenancy)
  , _ltpGroupName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplatePlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpAffinity' - The affinity setting for the instance on the Dedicated Host.
--
-- * 'ltpHostId' - The ID of the Dedicated Host for the instance.
--
-- * 'ltpSpreadDomain' - Reserved for future use.
--
-- * 'ltpAvailabilityZone' - The Availability Zone of the instance.
--
-- * 'ltpTenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware.
--
-- * 'ltpGroupName' - The name of the placement group for the instance.
launchTemplatePlacement
    :: LaunchTemplatePlacement
launchTemplatePlacement =
  LaunchTemplatePlacement'
    { _ltpAffinity = Nothing
    , _ltpHostId = Nothing
    , _ltpSpreadDomain = Nothing
    , _ltpAvailabilityZone = Nothing
    , _ltpTenancy = Nothing
    , _ltpGroupName = Nothing
    }


-- | The affinity setting for the instance on the Dedicated Host.
ltpAffinity :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpAffinity = lens _ltpAffinity (\ s a -> s{_ltpAffinity = a})

-- | The ID of the Dedicated Host for the instance.
ltpHostId :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpHostId = lens _ltpHostId (\ s a -> s{_ltpHostId = a})

-- | Reserved for future use.
ltpSpreadDomain :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpSpreadDomain = lens _ltpSpreadDomain (\ s a -> s{_ltpSpreadDomain = a})

-- | The Availability Zone of the instance.
ltpAvailabilityZone :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpAvailabilityZone = lens _ltpAvailabilityZone (\ s a -> s{_ltpAvailabilityZone = a})

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware.
ltpTenancy :: Lens' LaunchTemplatePlacement (Maybe Tenancy)
ltpTenancy = lens _ltpTenancy (\ s a -> s{_ltpTenancy = a})

-- | The name of the placement group for the instance.
ltpGroupName :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpGroupName = lens _ltpGroupName (\ s a -> s{_ltpGroupName = a})

instance FromXML LaunchTemplatePlacement where
        parseXML x
          = LaunchTemplatePlacement' <$>
              (x .@? "affinity") <*> (x .@? "hostId") <*>
                (x .@? "spreadDomain")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "tenancy")
                <*> (x .@? "groupName")

instance NFData LaunchTemplatePlacement where

-- | The placement for the instance.
--
--
--
-- /See:/ 'launchTemplatePlacementRequest' smart constructor.
data LaunchTemplatePlacementRequest = LaunchTemplatePlacementRequest'
  { _ltprAffinity         :: !(Maybe Text)
  , _ltprHostId           :: !(Maybe Text)
  , _ltprSpreadDomain     :: !(Maybe Text)
  , _ltprAvailabilityZone :: !(Maybe Text)
  , _ltprTenancy          :: !(Maybe Tenancy)
  , _ltprGroupName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplatePlacementRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltprAffinity' - The affinity setting for an instance on a Dedicated Host.
--
-- * 'ltprHostId' - The ID of the Dedicated Host for the instance.
--
-- * 'ltprSpreadDomain' - Reserved for future use.
--
-- * 'ltprAvailabilityZone' - The Availability Zone for the instance.
--
-- * 'ltprTenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of dedicated runs on single-tenant hardware.
--
-- * 'ltprGroupName' - The name of the placement group for the instance.
launchTemplatePlacementRequest
    :: LaunchTemplatePlacementRequest
launchTemplatePlacementRequest =
  LaunchTemplatePlacementRequest'
    { _ltprAffinity = Nothing
    , _ltprHostId = Nothing
    , _ltprSpreadDomain = Nothing
    , _ltprAvailabilityZone = Nothing
    , _ltprTenancy = Nothing
    , _ltprGroupName = Nothing
    }


-- | The affinity setting for an instance on a Dedicated Host.
ltprAffinity :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprAffinity = lens _ltprAffinity (\ s a -> s{_ltprAffinity = a})

-- | The ID of the Dedicated Host for the instance.
ltprHostId :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprHostId = lens _ltprHostId (\ s a -> s{_ltprHostId = a})

-- | Reserved for future use.
ltprSpreadDomain :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprSpreadDomain = lens _ltprSpreadDomain (\ s a -> s{_ltprSpreadDomain = a})

-- | The Availability Zone for the instance.
ltprAvailabilityZone :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprAvailabilityZone = lens _ltprAvailabilityZone (\ s a -> s{_ltprAvailabilityZone = a})

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of dedicated runs on single-tenant hardware.
ltprTenancy :: Lens' LaunchTemplatePlacementRequest (Maybe Tenancy)
ltprTenancy = lens _ltprTenancy (\ s a -> s{_ltprTenancy = a})

-- | The name of the placement group for the instance.
ltprGroupName :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprGroupName = lens _ltprGroupName (\ s a -> s{_ltprGroupName = a})

instance NFData LaunchTemplatePlacementRequest where

instance ToQuery LaunchTemplatePlacementRequest where
        toQuery LaunchTemplatePlacementRequest'{..}
          = mconcat
              ["Affinity" =: _ltprAffinity,
               "HostId" =: _ltprHostId,
               "SpreadDomain" =: _ltprSpreadDomain,
               "AvailabilityZone" =: _ltprAvailabilityZone,
               "Tenancy" =: _ltprTenancy,
               "GroupName" =: _ltprGroupName]

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request, but not both.
--
--
--
-- /See:/ 'launchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { _ltsLaunchTemplateName :: !(Maybe Text)
  , _ltsLaunchTemplateId   :: !(Maybe Text)
  , _ltsVersion            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsLaunchTemplateName' - The name of the launch template.
--
-- * 'ltsLaunchTemplateId' - The ID of the launch template.
--
-- * 'ltsVersion' - The version number of the launch template. Default: The default version for the launch template.
launchTemplateSpecification
    :: LaunchTemplateSpecification
launchTemplateSpecification =
  LaunchTemplateSpecification'
    { _ltsLaunchTemplateName = Nothing
    , _ltsLaunchTemplateId = Nothing
    , _ltsVersion = Nothing
    }


-- | The name of the launch template.
ltsLaunchTemplateName :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateName = lens _ltsLaunchTemplateName (\ s a -> s{_ltsLaunchTemplateName = a})

-- | The ID of the launch template.
ltsLaunchTemplateId :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateId = lens _ltsLaunchTemplateId (\ s a -> s{_ltsLaunchTemplateId = a})

-- | The version number of the launch template. Default: The default version for the launch template.
ltsVersion :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsVersion = lens _ltsVersion (\ s a -> s{_ltsVersion = a})

instance NFData LaunchTemplateSpecification where

instance ToQuery LaunchTemplateSpecification where
        toQuery LaunchTemplateSpecification'{..}
          = mconcat
              ["LaunchTemplateName" =: _ltsLaunchTemplateName,
               "LaunchTemplateId" =: _ltsLaunchTemplateId,
               "Version" =: _ltsVersion]

-- | The options for Spot Instances.
--
--
--
-- /See:/ 'launchTemplateSpotMarketOptions' smart constructor.
data LaunchTemplateSpotMarketOptions = LaunchTemplateSpotMarketOptions'
  { _ltsmoBlockDurationMinutes         :: !(Maybe Int)
  , _ltsmoInstanceInterruptionBehavior :: !(Maybe InstanceInterruptionBehavior)
  , _ltsmoValidUntil                   :: !(Maybe ISO8601)
  , _ltsmoSpotInstanceType             :: !(Maybe SpotInstanceType)
  , _ltsmoMaxPrice                     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateSpotMarketOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsmoBlockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- * 'ltsmoInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- * 'ltsmoValidUntil' - The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached.
--
-- * 'ltsmoSpotInstanceType' - The Spot Instance request type.
--
-- * 'ltsmoMaxPrice' - The maximum hourly price you're willing to pay for the Spot Instances.
launchTemplateSpotMarketOptions
    :: LaunchTemplateSpotMarketOptions
launchTemplateSpotMarketOptions =
  LaunchTemplateSpotMarketOptions'
    { _ltsmoBlockDurationMinutes = Nothing
    , _ltsmoInstanceInterruptionBehavior = Nothing
    , _ltsmoValidUntil = Nothing
    , _ltsmoSpotInstanceType = Nothing
    , _ltsmoMaxPrice = Nothing
    }


-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
ltsmoBlockDurationMinutes :: Lens' LaunchTemplateSpotMarketOptions (Maybe Int)
ltsmoBlockDurationMinutes = lens _ltsmoBlockDurationMinutes (\ s a -> s{_ltsmoBlockDurationMinutes = a})

-- | The behavior when a Spot Instance is interrupted.
ltsmoInstanceInterruptionBehavior :: Lens' LaunchTemplateSpotMarketOptions (Maybe InstanceInterruptionBehavior)
ltsmoInstanceInterruptionBehavior = lens _ltsmoInstanceInterruptionBehavior (\ s a -> s{_ltsmoInstanceInterruptionBehavior = a})

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached.
ltsmoValidUntil :: Lens' LaunchTemplateSpotMarketOptions (Maybe UTCTime)
ltsmoValidUntil = lens _ltsmoValidUntil (\ s a -> s{_ltsmoValidUntil = a}) . mapping _Time

-- | The Spot Instance request type.
ltsmoSpotInstanceType :: Lens' LaunchTemplateSpotMarketOptions (Maybe SpotInstanceType)
ltsmoSpotInstanceType = lens _ltsmoSpotInstanceType (\ s a -> s{_ltsmoSpotInstanceType = a})

-- | The maximum hourly price you're willing to pay for the Spot Instances.
ltsmoMaxPrice :: Lens' LaunchTemplateSpotMarketOptions (Maybe Text)
ltsmoMaxPrice = lens _ltsmoMaxPrice (\ s a -> s{_ltsmoMaxPrice = a})

instance FromXML LaunchTemplateSpotMarketOptions
         where
        parseXML x
          = LaunchTemplateSpotMarketOptions' <$>
              (x .@? "blockDurationMinutes") <*>
                (x .@? "instanceInterruptionBehavior")
                <*> (x .@? "validUntil")
                <*> (x .@? "spotInstanceType")
                <*> (x .@? "maxPrice")

instance NFData LaunchTemplateSpotMarketOptions where

-- | The options for Spot Instances.
--
--
--
-- /See:/ 'launchTemplateSpotMarketOptionsRequest' smart constructor.
data LaunchTemplateSpotMarketOptionsRequest = LaunchTemplateSpotMarketOptionsRequest'
  { _ltsmorBlockDurationMinutes         :: !(Maybe Int)
  , _ltsmorInstanceInterruptionBehavior :: !(Maybe InstanceInterruptionBehavior)
  , _ltsmorValidUntil                   :: !(Maybe ISO8601)
  , _ltsmorSpotInstanceType             :: !(Maybe SpotInstanceType)
  , _ltsmorMaxPrice                     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateSpotMarketOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsmorBlockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- * 'ltsmorInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'ltsmorValidUntil' - The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
--
-- * 'ltsmorSpotInstanceType' - The Spot Instance request type.
--
-- * 'ltsmorMaxPrice' - The maximum hourly price you're willing to pay for the Spot Instances.
launchTemplateSpotMarketOptionsRequest
    :: LaunchTemplateSpotMarketOptionsRequest
launchTemplateSpotMarketOptionsRequest =
  LaunchTemplateSpotMarketOptionsRequest'
    { _ltsmorBlockDurationMinutes = Nothing
    , _ltsmorInstanceInterruptionBehavior = Nothing
    , _ltsmorValidUntil = Nothing
    , _ltsmorSpotInstanceType = Nothing
    , _ltsmorMaxPrice = Nothing
    }


-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
ltsmorBlockDurationMinutes :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe Int)
ltsmorBlockDurationMinutes = lens _ltsmorBlockDurationMinutes (\ s a -> s{_ltsmorBlockDurationMinutes = a})

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
ltsmorInstanceInterruptionBehavior :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe InstanceInterruptionBehavior)
ltsmorInstanceInterruptionBehavior = lens _ltsmorInstanceInterruptionBehavior (\ s a -> s{_ltsmorInstanceInterruptionBehavior = a})

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
ltsmorValidUntil :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe UTCTime)
ltsmorValidUntil = lens _ltsmorValidUntil (\ s a -> s{_ltsmorValidUntil = a}) . mapping _Time

-- | The Spot Instance request type.
ltsmorSpotInstanceType :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe SpotInstanceType)
ltsmorSpotInstanceType = lens _ltsmorSpotInstanceType (\ s a -> s{_ltsmorSpotInstanceType = a})

-- | The maximum hourly price you're willing to pay for the Spot Instances.
ltsmorMaxPrice :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe Text)
ltsmorMaxPrice = lens _ltsmorMaxPrice (\ s a -> s{_ltsmorMaxPrice = a})

instance NFData
           LaunchTemplateSpotMarketOptionsRequest
         where

instance ToQuery
           LaunchTemplateSpotMarketOptionsRequest
         where
        toQuery LaunchTemplateSpotMarketOptionsRequest'{..}
          = mconcat
              ["BlockDurationMinutes" =:
                 _ltsmorBlockDurationMinutes,
               "InstanceInterruptionBehavior" =:
                 _ltsmorInstanceInterruptionBehavior,
               "ValidUntil" =: _ltsmorValidUntil,
               "SpotInstanceType" =: _ltsmorSpotInstanceType,
               "MaxPrice" =: _ltsmorMaxPrice]

-- | The tag specification for the launch template.
--
--
--
-- /See:/ 'launchTemplateTagSpecification' smart constructor.
data LaunchTemplateTagSpecification = LaunchTemplateTagSpecification'
  { _lttsResourceType :: !(Maybe ResourceType)
  , _lttsTags         :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateTagSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lttsResourceType' - The type of resource.
--
-- * 'lttsTags' - The tags for the resource.
launchTemplateTagSpecification
    :: LaunchTemplateTagSpecification
launchTemplateTagSpecification =
  LaunchTemplateTagSpecification'
    {_lttsResourceType = Nothing, _lttsTags = Nothing}


-- | The type of resource.
lttsResourceType :: Lens' LaunchTemplateTagSpecification (Maybe ResourceType)
lttsResourceType = lens _lttsResourceType (\ s a -> s{_lttsResourceType = a})

-- | The tags for the resource.
lttsTags :: Lens' LaunchTemplateTagSpecification [Tag]
lttsTags = lens _lttsTags (\ s a -> s{_lttsTags = a}) . _Default . _Coerce

instance FromXML LaunchTemplateTagSpecification where
        parseXML x
          = LaunchTemplateTagSpecification' <$>
              (x .@? "resourceType") <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData LaunchTemplateTagSpecification where

-- | The tags specification for the launch template.
--
--
--
-- /See:/ 'launchTemplateTagSpecificationRequest' smart constructor.
data LaunchTemplateTagSpecificationRequest = LaunchTemplateTagSpecificationRequest'
  { _lttsrResourceType :: !(Maybe ResourceType)
  , _lttsrTags         :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateTagSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lttsrResourceType' - The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ .
--
-- * 'lttsrTags' - The tags to apply to the resource.
launchTemplateTagSpecificationRequest
    :: LaunchTemplateTagSpecificationRequest
launchTemplateTagSpecificationRequest =
  LaunchTemplateTagSpecificationRequest'
    {_lttsrResourceType = Nothing, _lttsrTags = Nothing}


-- | The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ .
lttsrResourceType :: Lens' LaunchTemplateTagSpecificationRequest (Maybe ResourceType)
lttsrResourceType = lens _lttsrResourceType (\ s a -> s{_lttsrResourceType = a})

-- | The tags to apply to the resource.
lttsrTags :: Lens' LaunchTemplateTagSpecificationRequest [Tag]
lttsrTags = lens _lttsrTags (\ s a -> s{_lttsrTags = a}) . _Default . _Coerce

instance NFData LaunchTemplateTagSpecificationRequest
         where

instance ToQuery
           LaunchTemplateTagSpecificationRequest
         where
        toQuery LaunchTemplateTagSpecificationRequest'{..}
          = mconcat
              ["ResourceType" =: _lttsrResourceType,
               toQuery (toQueryList "Tag" <$> _lttsrTags)]

-- | Describes a launch template version.
--
--
--
-- /See:/ 'launchTemplateVersion' smart constructor.
data LaunchTemplateVersion = LaunchTemplateVersion'
  { _ltvLaunchTemplateName :: !(Maybe Text)
  , _ltvLaunchTemplateId   :: !(Maybe Text)
  , _ltvCreatedBy          :: !(Maybe Text)
  , _ltvDefaultVersion     :: !(Maybe Bool)
  , _ltvVersionNumber      :: !(Maybe Integer)
  , _ltvVersionDescription :: !(Maybe Text)
  , _ltvLaunchTemplateData :: !(Maybe ResponseLaunchTemplateData)
  , _ltvCreateTime         :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplateVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltvLaunchTemplateName' - The name of the launch template.
--
-- * 'ltvLaunchTemplateId' - The ID of the launch template.
--
-- * 'ltvCreatedBy' - The principal that created the version.
--
-- * 'ltvDefaultVersion' - Indicates whether the version is the default version.
--
-- * 'ltvVersionNumber' - The version number.
--
-- * 'ltvVersionDescription' - The description for the version.
--
-- * 'ltvLaunchTemplateData' - Information about the launch template.
--
-- * 'ltvCreateTime' - The time the version was created.
launchTemplateVersion
    :: LaunchTemplateVersion
launchTemplateVersion =
  LaunchTemplateVersion'
    { _ltvLaunchTemplateName = Nothing
    , _ltvLaunchTemplateId = Nothing
    , _ltvCreatedBy = Nothing
    , _ltvDefaultVersion = Nothing
    , _ltvVersionNumber = Nothing
    , _ltvVersionDescription = Nothing
    , _ltvLaunchTemplateData = Nothing
    , _ltvCreateTime = Nothing
    }


-- | The name of the launch template.
ltvLaunchTemplateName :: Lens' LaunchTemplateVersion (Maybe Text)
ltvLaunchTemplateName = lens _ltvLaunchTemplateName (\ s a -> s{_ltvLaunchTemplateName = a})

-- | The ID of the launch template.
ltvLaunchTemplateId :: Lens' LaunchTemplateVersion (Maybe Text)
ltvLaunchTemplateId = lens _ltvLaunchTemplateId (\ s a -> s{_ltvLaunchTemplateId = a})

-- | The principal that created the version.
ltvCreatedBy :: Lens' LaunchTemplateVersion (Maybe Text)
ltvCreatedBy = lens _ltvCreatedBy (\ s a -> s{_ltvCreatedBy = a})

-- | Indicates whether the version is the default version.
ltvDefaultVersion :: Lens' LaunchTemplateVersion (Maybe Bool)
ltvDefaultVersion = lens _ltvDefaultVersion (\ s a -> s{_ltvDefaultVersion = a})

-- | The version number.
ltvVersionNumber :: Lens' LaunchTemplateVersion (Maybe Integer)
ltvVersionNumber = lens _ltvVersionNumber (\ s a -> s{_ltvVersionNumber = a})

-- | The description for the version.
ltvVersionDescription :: Lens' LaunchTemplateVersion (Maybe Text)
ltvVersionDescription = lens _ltvVersionDescription (\ s a -> s{_ltvVersionDescription = a})

-- | Information about the launch template.
ltvLaunchTemplateData :: Lens' LaunchTemplateVersion (Maybe ResponseLaunchTemplateData)
ltvLaunchTemplateData = lens _ltvLaunchTemplateData (\ s a -> s{_ltvLaunchTemplateData = a})

-- | The time the version was created.
ltvCreateTime :: Lens' LaunchTemplateVersion (Maybe UTCTime)
ltvCreateTime = lens _ltvCreateTime (\ s a -> s{_ltvCreateTime = a}) . mapping _Time

instance FromXML LaunchTemplateVersion where
        parseXML x
          = LaunchTemplateVersion' <$>
              (x .@? "launchTemplateName") <*>
                (x .@? "launchTemplateId")
                <*> (x .@? "createdBy")
                <*> (x .@? "defaultVersion")
                <*> (x .@? "versionNumber")
                <*> (x .@? "versionDescription")
                <*> (x .@? "launchTemplateData")
                <*> (x .@? "createTime")

instance NFData LaunchTemplateVersion where

-- | Describes the monitoring for the instance.
--
--
--
-- /See:/ 'launchTemplatesMonitoring' smart constructor.
newtype LaunchTemplatesMonitoring = LaunchTemplatesMonitoring'
  { _ltmEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplatesMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltmEnabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
launchTemplatesMonitoring
    :: LaunchTemplatesMonitoring
launchTemplatesMonitoring = LaunchTemplatesMonitoring' {_ltmEnabled = Nothing}


-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
ltmEnabled :: Lens' LaunchTemplatesMonitoring (Maybe Bool)
ltmEnabled = lens _ltmEnabled (\ s a -> s{_ltmEnabled = a})

instance FromXML LaunchTemplatesMonitoring where
        parseXML x
          = LaunchTemplatesMonitoring' <$> (x .@? "enabled")

instance NFData LaunchTemplatesMonitoring where

-- | Describes the monitoring for the instance.
--
--
--
-- /See:/ 'launchTemplatesMonitoringRequest' smart constructor.
newtype LaunchTemplatesMonitoringRequest = LaunchTemplatesMonitoringRequest'
  { _ltmrEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchTemplatesMonitoringRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltmrEnabled' - Specify @true@ to enable detailed monitoring. Otherwise, basic monitoring is enabled.
launchTemplatesMonitoringRequest
    :: LaunchTemplatesMonitoringRequest
launchTemplatesMonitoringRequest =
  LaunchTemplatesMonitoringRequest' {_ltmrEnabled = Nothing}


-- | Specify @true@ to enable detailed monitoring. Otherwise, basic monitoring is enabled.
ltmrEnabled :: Lens' LaunchTemplatesMonitoringRequest (Maybe Bool)
ltmrEnabled = lens _ltmrEnabled (\ s a -> s{_ltmrEnabled = a})

instance NFData LaunchTemplatesMonitoringRequest
         where

instance ToQuery LaunchTemplatesMonitoringRequest
         where
        toQuery LaunchTemplatesMonitoringRequest'{..}
          = mconcat ["Enabled" =: _ltmrEnabled]

-- | Describes the Classic Load Balancers and target groups to attach to a Spot Fleet request.
--
--
--
-- /See:/ 'loadBalancersConfig' smart constructor.
data LoadBalancersConfig = LoadBalancersConfig'
  { _lbcClassicLoadBalancersConfig :: !(Maybe ClassicLoadBalancersConfig)
  , _lbcTargetGroupsConfig         :: !(Maybe TargetGroupsConfig)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancersConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbcClassicLoadBalancersConfig' - The Classic Load Balancers.
--
-- * 'lbcTargetGroupsConfig' - The target groups.
loadBalancersConfig
    :: LoadBalancersConfig
loadBalancersConfig =
  LoadBalancersConfig'
    {_lbcClassicLoadBalancersConfig = Nothing, _lbcTargetGroupsConfig = Nothing}


-- | The Classic Load Balancers.
lbcClassicLoadBalancersConfig :: Lens' LoadBalancersConfig (Maybe ClassicLoadBalancersConfig)
lbcClassicLoadBalancersConfig = lens _lbcClassicLoadBalancersConfig (\ s a -> s{_lbcClassicLoadBalancersConfig = a})

-- | The target groups.
lbcTargetGroupsConfig :: Lens' LoadBalancersConfig (Maybe TargetGroupsConfig)
lbcTargetGroupsConfig = lens _lbcTargetGroupsConfig (\ s a -> s{_lbcTargetGroupsConfig = a})

instance FromXML LoadBalancersConfig where
        parseXML x
          = LoadBalancersConfig' <$>
              (x .@? "classicLoadBalancersConfig") <*>
                (x .@? "targetGroupsConfig")

instance NFData LoadBalancersConfig where

instance ToQuery LoadBalancersConfig where
        toQuery LoadBalancersConfig'{..}
          = mconcat
              ["ClassicLoadBalancersConfig" =:
                 _lbcClassicLoadBalancersConfig,
               "TargetGroupsConfig" =: _lbcTargetGroupsConfig]

-- | Describes a load permission.
--
--
--
-- /See:/ 'loadPermission' smart constructor.
data LoadPermission = LoadPermission'
  { _lpGroup  :: !(Maybe PermissionGroup)
  , _lpUserId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpGroup' - The name of the group.
--
-- * 'lpUserId' - The AWS account ID.
loadPermission
    :: LoadPermission
loadPermission = LoadPermission' {_lpGroup = Nothing, _lpUserId = Nothing}


-- | The name of the group.
lpGroup :: Lens' LoadPermission (Maybe PermissionGroup)
lpGroup = lens _lpGroup (\ s a -> s{_lpGroup = a})

-- | The AWS account ID.
lpUserId :: Lens' LoadPermission (Maybe Text)
lpUserId = lens _lpUserId (\ s a -> s{_lpUserId = a})

instance FromXML LoadPermission where
        parseXML x
          = LoadPermission' <$>
              (x .@? "group") <*> (x .@? "userId")

instance NFData LoadPermission where

-- | Describes modifications to the load permissions of an Amazon FPGA image (AFI).
--
--
--
-- /See:/ 'loadPermissionModifications' smart constructor.
data LoadPermissionModifications = LoadPermissionModifications'
  { _lpmRemove :: !(Maybe [LoadPermissionRequest])
  , _lpmAdd    :: !(Maybe [LoadPermissionRequest])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadPermissionModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpmRemove' - The load permissions to remove.
--
-- * 'lpmAdd' - The load permissions to add.
loadPermissionModifications
    :: LoadPermissionModifications
loadPermissionModifications =
  LoadPermissionModifications' {_lpmRemove = Nothing, _lpmAdd = Nothing}


-- | The load permissions to remove.
lpmRemove :: Lens' LoadPermissionModifications [LoadPermissionRequest]
lpmRemove = lens _lpmRemove (\ s a -> s{_lpmRemove = a}) . _Default . _Coerce

-- | The load permissions to add.
lpmAdd :: Lens' LoadPermissionModifications [LoadPermissionRequest]
lpmAdd = lens _lpmAdd (\ s a -> s{_lpmAdd = a}) . _Default . _Coerce

instance NFData LoadPermissionModifications where

instance ToQuery LoadPermissionModifications where
        toQuery LoadPermissionModifications'{..}
          = mconcat
              [toQuery (toQueryList "Remove" <$> _lpmRemove),
               toQuery (toQueryList "Add" <$> _lpmAdd)]

-- | Describes a load permission.
--
--
--
-- /See:/ 'loadPermissionRequest' smart constructor.
data LoadPermissionRequest = LoadPermissionRequest'
  { _lprGroup  :: !(Maybe PermissionGroup)
  , _lprUserId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadPermissionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprGroup' - The name of the group.
--
-- * 'lprUserId' - The AWS account ID.
loadPermissionRequest
    :: LoadPermissionRequest
loadPermissionRequest =
  LoadPermissionRequest' {_lprGroup = Nothing, _lprUserId = Nothing}


-- | The name of the group.
lprGroup :: Lens' LoadPermissionRequest (Maybe PermissionGroup)
lprGroup = lens _lprGroup (\ s a -> s{_lprGroup = a})

-- | The AWS account ID.
lprUserId :: Lens' LoadPermissionRequest (Maybe Text)
lprUserId = lens _lprUserId (\ s a -> s{_lprUserId = a})

instance NFData LoadPermissionRequest where

instance ToQuery LoadPermissionRequest where
        toQuery LoadPermissionRequest'{..}
          = mconcat
              ["Group" =: _lprGroup, "UserId" =: _lprUserId]

-- | Describes the monitoring of an instance.
--
--
--
-- /See:/ 'monitoring' smart constructor.
newtype Monitoring = Monitoring'
  { _mState :: Maybe MonitoringState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Monitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mState' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
monitoring
    :: Monitoring
monitoring = Monitoring' {_mState = Nothing}


-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
mState :: Lens' Monitoring (Maybe MonitoringState)
mState = lens _mState (\ s a -> s{_mState = a})

instance FromXML Monitoring where
        parseXML x = Monitoring' <$> (x .@? "state")

instance NFData Monitoring where

-- | Describes the status of a moving Elastic IP address.
--
--
--
-- /See:/ 'movingAddressStatus' smart constructor.
data MovingAddressStatus = MovingAddressStatus'
  { _masMoveStatus :: !(Maybe MoveStatus)
  , _masPublicIP   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MovingAddressStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'masMoveStatus' - The status of the Elastic IP address that's being moved to the EC2-VPC platform, or restored to the EC2-Classic platform.
--
-- * 'masPublicIP' - The Elastic IP address.
movingAddressStatus
    :: MovingAddressStatus
movingAddressStatus =
  MovingAddressStatus' {_masMoveStatus = Nothing, _masPublicIP = Nothing}


-- | The status of the Elastic IP address that's being moved to the EC2-VPC platform, or restored to the EC2-Classic platform.
masMoveStatus :: Lens' MovingAddressStatus (Maybe MoveStatus)
masMoveStatus = lens _masMoveStatus (\ s a -> s{_masMoveStatus = a})

-- | The Elastic IP address.
masPublicIP :: Lens' MovingAddressStatus (Maybe Text)
masPublicIP = lens _masPublicIP (\ s a -> s{_masPublicIP = a})

instance FromXML MovingAddressStatus where
        parseXML x
          = MovingAddressStatus' <$>
              (x .@? "moveStatus") <*> (x .@? "publicIp")

instance NFData MovingAddressStatus where

-- | Describes a NAT gateway.
--
--
--
-- /See:/ 'natGateway' smart constructor.
data NatGateway = NatGateway'
  { _ngState                :: !(Maybe NatGatewayState)
  , _ngFailureCode          :: !(Maybe Text)
  , _ngVPCId                :: !(Maybe Text)
  , _ngFailureMessage       :: !(Maybe Text)
  , _ngNatGatewayId         :: !(Maybe Text)
  , _ngSubnetId             :: !(Maybe Text)
  , _ngDeleteTime           :: !(Maybe ISO8601)
  , _ngProvisionedBandwidth :: !(Maybe ProvisionedBandwidth)
  , _ngNatGatewayAddresses  :: !(Maybe [NatGatewayAddress])
  , _ngCreateTime           :: !(Maybe ISO8601)
  , _ngTags                 :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NatGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngState' - The state of the NAT gateway.     * @pending@ : The NAT gateway is being created and is not ready to process traffic.     * @failed@ : The NAT gateway could not be created. Check the @failureCode@ and @failureMessage@ fields for the reason.     * @available@ : The NAT gateway is able to process traffic. This status remains until you delete the NAT gateway, and does not indicate the health of the NAT gateway.     * @deleting@ : The NAT gateway is in the process of being terminated and may still be processing traffic.     * @deleted@ : The NAT gateway has been terminated and is no longer processing traffic.
--
-- * 'ngFailureCode' - If the NAT gateway could not be created, specifies the error code for the failure. (@InsufficientFreeAddressesInSubnet@ | @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ | @Resource.AlreadyAssociated@ | @InternalError@ | @InvalidSubnetID.NotFound@ )
--
-- * 'ngVPCId' - The ID of the VPC in which the NAT gateway is located.
--
-- * 'ngFailureMessage' - If the NAT gateway could not be created, specifies the error message for the failure, that corresponds to the error code.     * For InsufficientFreeAddressesInSubnet: "Subnet has insufficient free addresses to create this NAT gateway"     * For Gateway.NotAttached: "Network vpc-xxxxxxxx has no Internet gateway attached"     * For InvalidAllocationID.NotFound: "Elastic IP address eipalloc-xxxxxxxx could not be associated with this NAT gateway"     * For Resource.AlreadyAssociated: "Elastic IP address eipalloc-xxxxxxxx is already associated"     * For InternalError: "Network interface eni-xxxxxxxx, created and used internally by this NAT gateway is in an invalid state. Please try again."     * For InvalidSubnetID.NotFound: "The specified subnet subnet-xxxxxxxx does not exist or could not be found."
--
-- * 'ngNatGatewayId' - The ID of the NAT gateway.
--
-- * 'ngSubnetId' - The ID of the subnet in which the NAT gateway is located.
--
-- * 'ngDeleteTime' - The date and time the NAT gateway was deleted, if applicable.
--
-- * 'ngProvisionedBandwidth' - Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'ngNatGatewayAddresses' - Information about the IP addresses and network interface associated with the NAT gateway.
--
-- * 'ngCreateTime' - The date and time the NAT gateway was created.
--
-- * 'ngTags' - The tags for the NAT gateway.
natGateway
    :: NatGateway
natGateway =
  NatGateway'
    { _ngState = Nothing
    , _ngFailureCode = Nothing
    , _ngVPCId = Nothing
    , _ngFailureMessage = Nothing
    , _ngNatGatewayId = Nothing
    , _ngSubnetId = Nothing
    , _ngDeleteTime = Nothing
    , _ngProvisionedBandwidth = Nothing
    , _ngNatGatewayAddresses = Nothing
    , _ngCreateTime = Nothing
    , _ngTags = Nothing
    }


-- | The state of the NAT gateway.     * @pending@ : The NAT gateway is being created and is not ready to process traffic.     * @failed@ : The NAT gateway could not be created. Check the @failureCode@ and @failureMessage@ fields for the reason.     * @available@ : The NAT gateway is able to process traffic. This status remains until you delete the NAT gateway, and does not indicate the health of the NAT gateway.     * @deleting@ : The NAT gateway is in the process of being terminated and may still be processing traffic.     * @deleted@ : The NAT gateway has been terminated and is no longer processing traffic.
ngState :: Lens' NatGateway (Maybe NatGatewayState)
ngState = lens _ngState (\ s a -> s{_ngState = a})

-- | If the NAT gateway could not be created, specifies the error code for the failure. (@InsufficientFreeAddressesInSubnet@ | @Gateway.NotAttached@ | @InvalidAllocationID.NotFound@ | @Resource.AlreadyAssociated@ | @InternalError@ | @InvalidSubnetID.NotFound@ )
ngFailureCode :: Lens' NatGateway (Maybe Text)
ngFailureCode = lens _ngFailureCode (\ s a -> s{_ngFailureCode = a})

-- | The ID of the VPC in which the NAT gateway is located.
ngVPCId :: Lens' NatGateway (Maybe Text)
ngVPCId = lens _ngVPCId (\ s a -> s{_ngVPCId = a})

-- | If the NAT gateway could not be created, specifies the error message for the failure, that corresponds to the error code.     * For InsufficientFreeAddressesInSubnet: "Subnet has insufficient free addresses to create this NAT gateway"     * For Gateway.NotAttached: "Network vpc-xxxxxxxx has no Internet gateway attached"     * For InvalidAllocationID.NotFound: "Elastic IP address eipalloc-xxxxxxxx could not be associated with this NAT gateway"     * For Resource.AlreadyAssociated: "Elastic IP address eipalloc-xxxxxxxx is already associated"     * For InternalError: "Network interface eni-xxxxxxxx, created and used internally by this NAT gateway is in an invalid state. Please try again."     * For InvalidSubnetID.NotFound: "The specified subnet subnet-xxxxxxxx does not exist or could not be found."
ngFailureMessage :: Lens' NatGateway (Maybe Text)
ngFailureMessage = lens _ngFailureMessage (\ s a -> s{_ngFailureMessage = a})

-- | The ID of the NAT gateway.
ngNatGatewayId :: Lens' NatGateway (Maybe Text)
ngNatGatewayId = lens _ngNatGatewayId (\ s a -> s{_ngNatGatewayId = a})

-- | The ID of the subnet in which the NAT gateway is located.
ngSubnetId :: Lens' NatGateway (Maybe Text)
ngSubnetId = lens _ngSubnetId (\ s a -> s{_ngSubnetId = a})

-- | The date and time the NAT gateway was deleted, if applicable.
ngDeleteTime :: Lens' NatGateway (Maybe UTCTime)
ngDeleteTime = lens _ngDeleteTime (\ s a -> s{_ngDeleteTime = a}) . mapping _Time

-- | Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
ngProvisionedBandwidth :: Lens' NatGateway (Maybe ProvisionedBandwidth)
ngProvisionedBandwidth = lens _ngProvisionedBandwidth (\ s a -> s{_ngProvisionedBandwidth = a})

-- | Information about the IP addresses and network interface associated with the NAT gateway.
ngNatGatewayAddresses :: Lens' NatGateway [NatGatewayAddress]
ngNatGatewayAddresses = lens _ngNatGatewayAddresses (\ s a -> s{_ngNatGatewayAddresses = a}) . _Default . _Coerce

-- | The date and time the NAT gateway was created.
ngCreateTime :: Lens' NatGateway (Maybe UTCTime)
ngCreateTime = lens _ngCreateTime (\ s a -> s{_ngCreateTime = a}) . mapping _Time

-- | The tags for the NAT gateway.
ngTags :: Lens' NatGateway [Tag]
ngTags = lens _ngTags (\ s a -> s{_ngTags = a}) . _Default . _Coerce

instance FromXML NatGateway where
        parseXML x
          = NatGateway' <$>
              (x .@? "state") <*> (x .@? "failureCode") <*>
                (x .@? "vpcId")
                <*> (x .@? "failureMessage")
                <*> (x .@? "natGatewayId")
                <*> (x .@? "subnetId")
                <*> (x .@? "deleteTime")
                <*> (x .@? "provisionedBandwidth")
                <*>
                (x .@? "natGatewayAddressSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "createTime")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData NatGateway where

-- | Describes the IP addresses and network interface associated with a NAT gateway.
--
--
--
-- /See:/ 'natGatewayAddress' smart constructor.
data NatGatewayAddress = NatGatewayAddress'
  { _ngaPrivateIP          :: !(Maybe Text)
  , _ngaAllocationId       :: !(Maybe Text)
  , _ngaNetworkInterfaceId :: !(Maybe Text)
  , _ngaPublicIP           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NatGatewayAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngaPrivateIP' - The private IP address associated with the Elastic IP address.
--
-- * 'ngaAllocationId' - The allocation ID of the Elastic IP address that's associated with the NAT gateway.
--
-- * 'ngaNetworkInterfaceId' - The ID of the network interface associated with the NAT gateway.
--
-- * 'ngaPublicIP' - The Elastic IP address associated with the NAT gateway.
natGatewayAddress
    :: NatGatewayAddress
natGatewayAddress =
  NatGatewayAddress'
    { _ngaPrivateIP = Nothing
    , _ngaAllocationId = Nothing
    , _ngaNetworkInterfaceId = Nothing
    , _ngaPublicIP = Nothing
    }


-- | The private IP address associated with the Elastic IP address.
ngaPrivateIP :: Lens' NatGatewayAddress (Maybe Text)
ngaPrivateIP = lens _ngaPrivateIP (\ s a -> s{_ngaPrivateIP = a})

-- | The allocation ID of the Elastic IP address that's associated with the NAT gateway.
ngaAllocationId :: Lens' NatGatewayAddress (Maybe Text)
ngaAllocationId = lens _ngaAllocationId (\ s a -> s{_ngaAllocationId = a})

-- | The ID of the network interface associated with the NAT gateway.
ngaNetworkInterfaceId :: Lens' NatGatewayAddress (Maybe Text)
ngaNetworkInterfaceId = lens _ngaNetworkInterfaceId (\ s a -> s{_ngaNetworkInterfaceId = a})

-- | The Elastic IP address associated with the NAT gateway.
ngaPublicIP :: Lens' NatGatewayAddress (Maybe Text)
ngaPublicIP = lens _ngaPublicIP (\ s a -> s{_ngaPublicIP = a})

instance FromXML NatGatewayAddress where
        parseXML x
          = NatGatewayAddress' <$>
              (x .@? "privateIp") <*> (x .@? "allocationId") <*>
                (x .@? "networkInterfaceId")
                <*> (x .@? "publicIp")

instance NFData NatGatewayAddress where

-- | Describes a network ACL.
--
--
--
-- /See:/ 'networkACL' smart constructor.
data NetworkACL = NetworkACL'
  { _naEntries      :: !(Maybe [NetworkACLEntry])
  , _naNetworkACLId :: !(Maybe Text)
  , _naVPCId        :: !(Maybe Text)
  , _naAssociations :: !(Maybe [NetworkACLAssociation])
  , _naTags         :: !(Maybe [Tag])
  , _naIsDefault    :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'naEntries' - One or more entries (rules) in the network ACL.
--
-- * 'naNetworkACLId' - The ID of the network ACL.
--
-- * 'naVPCId' - The ID of the VPC for the network ACL.
--
-- * 'naAssociations' - Any associations between the network ACL and one or more subnets
--
-- * 'naTags' - Any tags assigned to the network ACL.
--
-- * 'naIsDefault' - Indicates whether this is the default network ACL for the VPC.
networkACL
    :: NetworkACL
networkACL =
  NetworkACL'
    { _naEntries = Nothing
    , _naNetworkACLId = Nothing
    , _naVPCId = Nothing
    , _naAssociations = Nothing
    , _naTags = Nothing
    , _naIsDefault = Nothing
    }


-- | One or more entries (rules) in the network ACL.
naEntries :: Lens' NetworkACL [NetworkACLEntry]
naEntries = lens _naEntries (\ s a -> s{_naEntries = a}) . _Default . _Coerce

-- | The ID of the network ACL.
naNetworkACLId :: Lens' NetworkACL (Maybe Text)
naNetworkACLId = lens _naNetworkACLId (\ s a -> s{_naNetworkACLId = a})

-- | The ID of the VPC for the network ACL.
naVPCId :: Lens' NetworkACL (Maybe Text)
naVPCId = lens _naVPCId (\ s a -> s{_naVPCId = a})

-- | Any associations between the network ACL and one or more subnets
naAssociations :: Lens' NetworkACL [NetworkACLAssociation]
naAssociations = lens _naAssociations (\ s a -> s{_naAssociations = a}) . _Default . _Coerce

-- | Any tags assigned to the network ACL.
naTags :: Lens' NetworkACL [Tag]
naTags = lens _naTags (\ s a -> s{_naTags = a}) . _Default . _Coerce

-- | Indicates whether this is the default network ACL for the VPC.
naIsDefault :: Lens' NetworkACL (Maybe Bool)
naIsDefault = lens _naIsDefault (\ s a -> s{_naIsDefault = a})

instance FromXML NetworkACL where
        parseXML x
          = NetworkACL' <$>
              (x .@? "entrySet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "networkAclId")
                <*> (x .@? "vpcId")
                <*>
                (x .@? "associationSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "default")

instance NFData NetworkACL where

-- | Describes an association between a network ACL and a subnet.
--
--
--
-- /See:/ 'networkACLAssociation' smart constructor.
data NetworkACLAssociation = NetworkACLAssociation'
  { _naaNetworkACLId            :: !(Maybe Text)
  , _naaSubnetId                :: !(Maybe Text)
  , _naaNetworkACLAssociationId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkACLAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'naaNetworkACLId' - The ID of the network ACL.
--
-- * 'naaSubnetId' - The ID of the subnet.
--
-- * 'naaNetworkACLAssociationId' - The ID of the association between a network ACL and a subnet.
networkACLAssociation
    :: NetworkACLAssociation
networkACLAssociation =
  NetworkACLAssociation'
    { _naaNetworkACLId = Nothing
    , _naaSubnetId = Nothing
    , _naaNetworkACLAssociationId = Nothing
    }


-- | The ID of the network ACL.
naaNetworkACLId :: Lens' NetworkACLAssociation (Maybe Text)
naaNetworkACLId = lens _naaNetworkACLId (\ s a -> s{_naaNetworkACLId = a})

-- | The ID of the subnet.
naaSubnetId :: Lens' NetworkACLAssociation (Maybe Text)
naaSubnetId = lens _naaSubnetId (\ s a -> s{_naaSubnetId = a})

-- | The ID of the association between a network ACL and a subnet.
naaNetworkACLAssociationId :: Lens' NetworkACLAssociation (Maybe Text)
naaNetworkACLAssociationId = lens _naaNetworkACLAssociationId (\ s a -> s{_naaNetworkACLAssociationId = a})

instance FromXML NetworkACLAssociation where
        parseXML x
          = NetworkACLAssociation' <$>
              (x .@? "networkAclId") <*> (x .@? "subnetId") <*>
                (x .@? "networkAclAssociationId")

instance NFData NetworkACLAssociation where

-- | Describes an entry in a network ACL.
--
--
--
-- /See:/ 'networkACLEntry' smart constructor.
data NetworkACLEntry = NetworkACLEntry'
  { _naeIPv6CidrBlock :: !(Maybe Text)
  , _naeICMPTypeCode  :: !(Maybe ICMPTypeCode)
  , _naeRuleNumber    :: !(Maybe Int)
  , _naeRuleAction    :: !(Maybe RuleAction)
  , _naeProtocol      :: !(Maybe Text)
  , _naePortRange     :: !(Maybe PortRange)
  , _naeCidrBlock     :: !(Maybe Text)
  , _naeEgress        :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkACLEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'naeIPv6CidrBlock' - The IPv6 network range to allow or deny, in CIDR notation.
--
-- * 'naeICMPTypeCode' - ICMP protocol: The ICMP type and code.
--
-- * 'naeRuleNumber' - The rule number for the entry. ACL entries are processed in ascending order by rule number.
--
-- * 'naeRuleAction' - Indicates whether to allow or deny the traffic that matches the rule.
--
-- * 'naeProtocol' - The protocol. A value of @-1@ means all protocols.
--
-- * 'naePortRange' - TCP or UDP protocols: The range of ports the rule applies to.
--
-- * 'naeCidrBlock' - The IPv4 network range to allow or deny, in CIDR notation.
--
-- * 'naeEgress' - Indicates whether the rule is an egress rule (applied to traffic leaving the subnet).
networkACLEntry
    :: NetworkACLEntry
networkACLEntry =
  NetworkACLEntry'
    { _naeIPv6CidrBlock = Nothing
    , _naeICMPTypeCode = Nothing
    , _naeRuleNumber = Nothing
    , _naeRuleAction = Nothing
    , _naeProtocol = Nothing
    , _naePortRange = Nothing
    , _naeCidrBlock = Nothing
    , _naeEgress = Nothing
    }


-- | The IPv6 network range to allow or deny, in CIDR notation.
naeIPv6CidrBlock :: Lens' NetworkACLEntry (Maybe Text)
naeIPv6CidrBlock = lens _naeIPv6CidrBlock (\ s a -> s{_naeIPv6CidrBlock = a})

-- | ICMP protocol: The ICMP type and code.
naeICMPTypeCode :: Lens' NetworkACLEntry (Maybe ICMPTypeCode)
naeICMPTypeCode = lens _naeICMPTypeCode (\ s a -> s{_naeICMPTypeCode = a})

-- | The rule number for the entry. ACL entries are processed in ascending order by rule number.
naeRuleNumber :: Lens' NetworkACLEntry (Maybe Int)
naeRuleNumber = lens _naeRuleNumber (\ s a -> s{_naeRuleNumber = a})

-- | Indicates whether to allow or deny the traffic that matches the rule.
naeRuleAction :: Lens' NetworkACLEntry (Maybe RuleAction)
naeRuleAction = lens _naeRuleAction (\ s a -> s{_naeRuleAction = a})

-- | The protocol. A value of @-1@ means all protocols.
naeProtocol :: Lens' NetworkACLEntry (Maybe Text)
naeProtocol = lens _naeProtocol (\ s a -> s{_naeProtocol = a})

-- | TCP or UDP protocols: The range of ports the rule applies to.
naePortRange :: Lens' NetworkACLEntry (Maybe PortRange)
naePortRange = lens _naePortRange (\ s a -> s{_naePortRange = a})

-- | The IPv4 network range to allow or deny, in CIDR notation.
naeCidrBlock :: Lens' NetworkACLEntry (Maybe Text)
naeCidrBlock = lens _naeCidrBlock (\ s a -> s{_naeCidrBlock = a})

-- | Indicates whether the rule is an egress rule (applied to traffic leaving the subnet).
naeEgress :: Lens' NetworkACLEntry (Maybe Bool)
naeEgress = lens _naeEgress (\ s a -> s{_naeEgress = a})

instance FromXML NetworkACLEntry where
        parseXML x
          = NetworkACLEntry' <$>
              (x .@? "ipv6CidrBlock") <*> (x .@? "icmpTypeCode")
                <*> (x .@? "ruleNumber")
                <*> (x .@? "ruleAction")
                <*> (x .@? "protocol")
                <*> (x .@? "portRange")
                <*> (x .@? "cidrBlock")
                <*> (x .@? "egress")

instance NFData NetworkACLEntry where

-- | Describes a network interface.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niGroups             :: !(Maybe [GroupIdentifier])
  , _niStatus             :: !(Maybe NetworkInterfaceStatus)
  , _niPrivateIPAddresses :: !(Maybe [NetworkInterfacePrivateIPAddress])
  , _niSourceDestCheck    :: !(Maybe Bool)
  , _niInterfaceType      :: !(Maybe NetworkInterfaceType)
  , _niVPCId              :: !(Maybe Text)
  , _niTagSet             :: !(Maybe [Tag])
  , _niRequesterManaged   :: !(Maybe Bool)
  , _niNetworkInterfaceId :: !(Maybe Text)
  , _niSubnetId           :: !(Maybe Text)
  , _niMACAddress         :: !(Maybe Text)
  , _niAttachment         :: !(Maybe NetworkInterfaceAttachment)
  , _niOwnerId            :: !(Maybe Text)
  , _niAvailabilityZone   :: !(Maybe Text)
  , _niPrivateIPAddress   :: !(Maybe Text)
  , _niPrivateDNSName     :: !(Maybe Text)
  , _niRequesterId        :: !(Maybe Text)
  , _niDescription        :: !(Maybe Text)
  , _niAssociation        :: !(Maybe NetworkInterfaceAssociation)
  , _niIPv6Addresses      :: !(Maybe [NetworkInterfaceIPv6Address])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niGroups' - Any security groups for the network interface.
--
-- * 'niStatus' - The status of the network interface.
--
-- * 'niPrivateIPAddresses' - The private IPv4 addresses associated with the network interface.
--
-- * 'niSourceDestCheck' - Indicates whether traffic to or from the instance is validated.
--
-- * 'niInterfaceType' - The type of interface.
--
-- * 'niVPCId' - The ID of the VPC.
--
-- * 'niTagSet' - Any tags assigned to the network interface.
--
-- * 'niRequesterManaged' - Indicates whether the network interface is being managed by AWS.
--
-- * 'niNetworkInterfaceId' - The ID of the network interface.
--
-- * 'niSubnetId' - The ID of the subnet.
--
-- * 'niMACAddress' - The MAC address.
--
-- * 'niAttachment' - The network interface attachment.
--
-- * 'niOwnerId' - The AWS account ID of the owner of the network interface.
--
-- * 'niAvailabilityZone' - The Availability Zone.
--
-- * 'niPrivateIPAddress' - The IPv4 address of the network interface within the subnet.
--
-- * 'niPrivateDNSName' - The private DNS name.
--
-- * 'niRequesterId' - The ID of the entity that launched the instance on your behalf (for example, AWS Management Console or Auto Scaling).
--
-- * 'niDescription' - A description.
--
-- * 'niAssociation' - The association information for an Elastic IP address (IPv4) associated with the network interface.
--
-- * 'niIPv6Addresses' - The IPv6 addresses associated with the network interface.
networkInterface
    :: NetworkInterface
networkInterface =
  NetworkInterface'
    { _niGroups = Nothing
    , _niStatus = Nothing
    , _niPrivateIPAddresses = Nothing
    , _niSourceDestCheck = Nothing
    , _niInterfaceType = Nothing
    , _niVPCId = Nothing
    , _niTagSet = Nothing
    , _niRequesterManaged = Nothing
    , _niNetworkInterfaceId = Nothing
    , _niSubnetId = Nothing
    , _niMACAddress = Nothing
    , _niAttachment = Nothing
    , _niOwnerId = Nothing
    , _niAvailabilityZone = Nothing
    , _niPrivateIPAddress = Nothing
    , _niPrivateDNSName = Nothing
    , _niRequesterId = Nothing
    , _niDescription = Nothing
    , _niAssociation = Nothing
    , _niIPv6Addresses = Nothing
    }


-- | Any security groups for the network interface.
niGroups :: Lens' NetworkInterface [GroupIdentifier]
niGroups = lens _niGroups (\ s a -> s{_niGroups = a}) . _Default . _Coerce

-- | The status of the network interface.
niStatus :: Lens' NetworkInterface (Maybe NetworkInterfaceStatus)
niStatus = lens _niStatus (\ s a -> s{_niStatus = a})

-- | The private IPv4 addresses associated with the network interface.
niPrivateIPAddresses :: Lens' NetworkInterface [NetworkInterfacePrivateIPAddress]
niPrivateIPAddresses = lens _niPrivateIPAddresses (\ s a -> s{_niPrivateIPAddresses = a}) . _Default . _Coerce

-- | Indicates whether traffic to or from the instance is validated.
niSourceDestCheck :: Lens' NetworkInterface (Maybe Bool)
niSourceDestCheck = lens _niSourceDestCheck (\ s a -> s{_niSourceDestCheck = a})

-- | The type of interface.
niInterfaceType :: Lens' NetworkInterface (Maybe NetworkInterfaceType)
niInterfaceType = lens _niInterfaceType (\ s a -> s{_niInterfaceType = a})

-- | The ID of the VPC.
niVPCId :: Lens' NetworkInterface (Maybe Text)
niVPCId = lens _niVPCId (\ s a -> s{_niVPCId = a})

-- | Any tags assigned to the network interface.
niTagSet :: Lens' NetworkInterface [Tag]
niTagSet = lens _niTagSet (\ s a -> s{_niTagSet = a}) . _Default . _Coerce

-- | Indicates whether the network interface is being managed by AWS.
niRequesterManaged :: Lens' NetworkInterface (Maybe Bool)
niRequesterManaged = lens _niRequesterManaged (\ s a -> s{_niRequesterManaged = a})

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId = lens _niNetworkInterfaceId (\ s a -> s{_niNetworkInterfaceId = a})

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\ s a -> s{_niSubnetId = a})

-- | The MAC address.
niMACAddress :: Lens' NetworkInterface (Maybe Text)
niMACAddress = lens _niMACAddress (\ s a -> s{_niMACAddress = a})

-- | The network interface attachment.
niAttachment :: Lens' NetworkInterface (Maybe NetworkInterfaceAttachment)
niAttachment = lens _niAttachment (\ s a -> s{_niAttachment = a})

-- | The AWS account ID of the owner of the network interface.
niOwnerId :: Lens' NetworkInterface (Maybe Text)
niOwnerId = lens _niOwnerId (\ s a -> s{_niOwnerId = a})

-- | The Availability Zone.
niAvailabilityZone :: Lens' NetworkInterface (Maybe Text)
niAvailabilityZone = lens _niAvailabilityZone (\ s a -> s{_niAvailabilityZone = a})

-- | The IPv4 address of the network interface within the subnet.
niPrivateIPAddress :: Lens' NetworkInterface (Maybe Text)
niPrivateIPAddress = lens _niPrivateIPAddress (\ s a -> s{_niPrivateIPAddress = a})

-- | The private DNS name.
niPrivateDNSName :: Lens' NetworkInterface (Maybe Text)
niPrivateDNSName = lens _niPrivateDNSName (\ s a -> s{_niPrivateDNSName = a})

-- | The ID of the entity that launched the instance on your behalf (for example, AWS Management Console or Auto Scaling).
niRequesterId :: Lens' NetworkInterface (Maybe Text)
niRequesterId = lens _niRequesterId (\ s a -> s{_niRequesterId = a})

-- | A description.
niDescription :: Lens' NetworkInterface (Maybe Text)
niDescription = lens _niDescription (\ s a -> s{_niDescription = a})

-- | The association information for an Elastic IP address (IPv4) associated with the network interface.
niAssociation :: Lens' NetworkInterface (Maybe NetworkInterfaceAssociation)
niAssociation = lens _niAssociation (\ s a -> s{_niAssociation = a})

-- | The IPv6 addresses associated with the network interface.
niIPv6Addresses :: Lens' NetworkInterface [NetworkInterfaceIPv6Address]
niIPv6Addresses = lens _niIPv6Addresses (\ s a -> s{_niIPv6Addresses = a}) . _Default . _Coerce

instance FromXML NetworkInterface where
        parseXML x
          = NetworkInterface' <$>
              (x .@? "groupSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "status")
                <*>
                (x .@? "privateIpAddressesSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "sourceDestCheck")
                <*> (x .@? "interfaceType")
                <*> (x .@? "vpcId")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "requesterManaged")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "subnetId")
                <*> (x .@? "macAddress")
                <*> (x .@? "attachment")
                <*> (x .@? "ownerId")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "privateIpAddress")
                <*> (x .@? "privateDnsName")
                <*> (x .@? "requesterId")
                <*> (x .@? "description")
                <*> (x .@? "association")
                <*>
                (x .@? "ipv6AddressesSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData NetworkInterface where

-- | Describes association information for an Elastic IP address (IPv4 only).
--
--
--
-- /See:/ 'networkInterfaceAssociation' smart constructor.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation'
  { _niaAssociationId :: !(Maybe Text)
  , _niaPublicDNSName :: !(Maybe Text)
  , _niaAllocationId  :: !(Maybe Text)
  , _niaIPOwnerId     :: !(Maybe Text)
  , _niaPublicIP      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterfaceAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niaAssociationId' - The association ID.
--
-- * 'niaPublicDNSName' - The public DNS name.
--
-- * 'niaAllocationId' - The allocation ID.
--
-- * 'niaIPOwnerId' - The ID of the Elastic IP address owner.
--
-- * 'niaPublicIP' - The address of the Elastic IP address bound to the network interface.
networkInterfaceAssociation
    :: NetworkInterfaceAssociation
networkInterfaceAssociation =
  NetworkInterfaceAssociation'
    { _niaAssociationId = Nothing
    , _niaPublicDNSName = Nothing
    , _niaAllocationId = Nothing
    , _niaIPOwnerId = Nothing
    , _niaPublicIP = Nothing
    }


-- | The association ID.
niaAssociationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAssociationId = lens _niaAssociationId (\ s a -> s{_niaAssociationId = a})

-- | The public DNS name.
niaPublicDNSName :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicDNSName = lens _niaPublicDNSName (\ s a -> s{_niaPublicDNSName = a})

-- | The allocation ID.
niaAllocationId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaAllocationId = lens _niaAllocationId (\ s a -> s{_niaAllocationId = a})

-- | The ID of the Elastic IP address owner.
niaIPOwnerId :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaIPOwnerId = lens _niaIPOwnerId (\ s a -> s{_niaIPOwnerId = a})

-- | The address of the Elastic IP address bound to the network interface.
niaPublicIP :: Lens' NetworkInterfaceAssociation (Maybe Text)
niaPublicIP = lens _niaPublicIP (\ s a -> s{_niaPublicIP = a})

instance FromXML NetworkInterfaceAssociation where
        parseXML x
          = NetworkInterfaceAssociation' <$>
              (x .@? "associationId") <*> (x .@? "publicDnsName")
                <*> (x .@? "allocationId")
                <*> (x .@? "ipOwnerId")
                <*> (x .@? "publicIp")

instance NFData NetworkInterfaceAssociation where

-- | Describes a network interface attachment.
--
--
--
-- /See:/ 'networkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { _niaInstanceId          :: !(Maybe Text)
  , _niaStatus              :: !(Maybe AttachmentStatus)
  , _niaDeleteOnTermination :: !(Maybe Bool)
  , _niaAttachmentId        :: !(Maybe Text)
  , _niaInstanceOwnerId     :: !(Maybe Text)
  , _niaAttachTime          :: !(Maybe ISO8601)
  , _niaDeviceIndex         :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterfaceAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niaInstanceId' - The ID of the instance.
--
-- * 'niaStatus' - The attachment state.
--
-- * 'niaDeleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
--
-- * 'niaAttachmentId' - The ID of the network interface attachment.
--
-- * 'niaInstanceOwnerId' - The AWS account ID of the owner of the instance.
--
-- * 'niaAttachTime' - The timestamp indicating when the attachment initiated.
--
-- * 'niaDeviceIndex' - The device index of the network interface attachment on the instance.
networkInterfaceAttachment
    :: NetworkInterfaceAttachment
networkInterfaceAttachment =
  NetworkInterfaceAttachment'
    { _niaInstanceId = Nothing
    , _niaStatus = Nothing
    , _niaDeleteOnTermination = Nothing
    , _niaAttachmentId = Nothing
    , _niaInstanceOwnerId = Nothing
    , _niaAttachTime = Nothing
    , _niaDeviceIndex = Nothing
    }


-- | The ID of the instance.
niaInstanceId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceId = lens _niaInstanceId (\ s a -> s{_niaInstanceId = a})

-- | The attachment state.
niaStatus :: Lens' NetworkInterfaceAttachment (Maybe AttachmentStatus)
niaStatus = lens _niaStatus (\ s a -> s{_niaStatus = a})

-- | Indicates whether the network interface is deleted when the instance is terminated.
niaDeleteOnTermination :: Lens' NetworkInterfaceAttachment (Maybe Bool)
niaDeleteOnTermination = lens _niaDeleteOnTermination (\ s a -> s{_niaDeleteOnTermination = a})

-- | The ID of the network interface attachment.
niaAttachmentId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaAttachmentId = lens _niaAttachmentId (\ s a -> s{_niaAttachmentId = a})

-- | The AWS account ID of the owner of the instance.
niaInstanceOwnerId :: Lens' NetworkInterfaceAttachment (Maybe Text)
niaInstanceOwnerId = lens _niaInstanceOwnerId (\ s a -> s{_niaInstanceOwnerId = a})

-- | The timestamp indicating when the attachment initiated.
niaAttachTime :: Lens' NetworkInterfaceAttachment (Maybe UTCTime)
niaAttachTime = lens _niaAttachTime (\ s a -> s{_niaAttachTime = a}) . mapping _Time

-- | The device index of the network interface attachment on the instance.
niaDeviceIndex :: Lens' NetworkInterfaceAttachment (Maybe Int)
niaDeviceIndex = lens _niaDeviceIndex (\ s a -> s{_niaDeviceIndex = a})

instance FromXML NetworkInterfaceAttachment where
        parseXML x
          = NetworkInterfaceAttachment' <$>
              (x .@? "instanceId") <*> (x .@? "status") <*>
                (x .@? "deleteOnTermination")
                <*> (x .@? "attachmentId")
                <*> (x .@? "instanceOwnerId")
                <*> (x .@? "attachTime")
                <*> (x .@? "deviceIndex")

instance NFData NetworkInterfaceAttachment where

-- | Describes an attachment change.
--
--
--
-- /See:/ 'networkInterfaceAttachmentChanges' smart constructor.
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges'
  { _niacDeleteOnTermination :: !(Maybe Bool)
  , _niacAttachmentId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterfaceAttachmentChanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niacDeleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
--
-- * 'niacAttachmentId' - The ID of the network interface attachment.
networkInterfaceAttachmentChanges
    :: NetworkInterfaceAttachmentChanges
networkInterfaceAttachmentChanges =
  NetworkInterfaceAttachmentChanges'
    {_niacDeleteOnTermination = Nothing, _niacAttachmentId = Nothing}


-- | Indicates whether the network interface is deleted when the instance is terminated.
niacDeleteOnTermination :: Lens' NetworkInterfaceAttachmentChanges (Maybe Bool)
niacDeleteOnTermination = lens _niacDeleteOnTermination (\ s a -> s{_niacDeleteOnTermination = a})

-- | The ID of the network interface attachment.
niacAttachmentId :: Lens' NetworkInterfaceAttachmentChanges (Maybe Text)
niacAttachmentId = lens _niacAttachmentId (\ s a -> s{_niacAttachmentId = a})

instance NFData NetworkInterfaceAttachmentChanges
         where

instance ToQuery NetworkInterfaceAttachmentChanges
         where
        toQuery NetworkInterfaceAttachmentChanges'{..}
          = mconcat
              ["DeleteOnTermination" =: _niacDeleteOnTermination,
               "AttachmentId" =: _niacAttachmentId]

-- | Describes an IPv6 address associated with a network interface.
--
--
--
-- /See:/ 'networkInterfaceIPv6Address' smart constructor.
newtype NetworkInterfaceIPv6Address = NetworkInterfaceIPv6Address'
  { _niiaIPv6Address :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterfaceIPv6Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niiaIPv6Address' - The IPv6 address.
networkInterfaceIPv6Address
    :: NetworkInterfaceIPv6Address
networkInterfaceIPv6Address =
  NetworkInterfaceIPv6Address' {_niiaIPv6Address = Nothing}


-- | The IPv6 address.
niiaIPv6Address :: Lens' NetworkInterfaceIPv6Address (Maybe Text)
niiaIPv6Address = lens _niiaIPv6Address (\ s a -> s{_niiaIPv6Address = a})

instance FromXML NetworkInterfaceIPv6Address where
        parseXML x
          = NetworkInterfaceIPv6Address' <$>
              (x .@? "ipv6Address")

instance NFData NetworkInterfaceIPv6Address where

-- | Describes a permission for a network interface.
--
--
--
-- /See:/ 'networkInterfacePermission' smart constructor.
data NetworkInterfacePermission = NetworkInterfacePermission'
  { _nipPermissionState              :: !(Maybe NetworkInterfacePermissionState)
  , _nipNetworkInterfacePermissionId :: !(Maybe Text)
  , _nipNetworkInterfaceId           :: !(Maybe Text)
  , _nipAWSAccountId                 :: !(Maybe Text)
  , _nipAWSService                   :: !(Maybe Text)
  , _nipPermission                   :: !(Maybe InterfacePermissionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterfacePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nipPermissionState' - Information about the state of the permission.
--
-- * 'nipNetworkInterfacePermissionId' - The ID of the network interface permission.
--
-- * 'nipNetworkInterfaceId' - The ID of the network interface.
--
-- * 'nipAWSAccountId' - The AWS account ID.
--
-- * 'nipAWSService' - The AWS service.
--
-- * 'nipPermission' - The type of permission.
networkInterfacePermission
    :: NetworkInterfacePermission
networkInterfacePermission =
  NetworkInterfacePermission'
    { _nipPermissionState = Nothing
    , _nipNetworkInterfacePermissionId = Nothing
    , _nipNetworkInterfaceId = Nothing
    , _nipAWSAccountId = Nothing
    , _nipAWSService = Nothing
    , _nipPermission = Nothing
    }


-- | Information about the state of the permission.
nipPermissionState :: Lens' NetworkInterfacePermission (Maybe NetworkInterfacePermissionState)
nipPermissionState = lens _nipPermissionState (\ s a -> s{_nipPermissionState = a})

-- | The ID of the network interface permission.
nipNetworkInterfacePermissionId :: Lens' NetworkInterfacePermission (Maybe Text)
nipNetworkInterfacePermissionId = lens _nipNetworkInterfacePermissionId (\ s a -> s{_nipNetworkInterfacePermissionId = a})

-- | The ID of the network interface.
nipNetworkInterfaceId :: Lens' NetworkInterfacePermission (Maybe Text)
nipNetworkInterfaceId = lens _nipNetworkInterfaceId (\ s a -> s{_nipNetworkInterfaceId = a})

-- | The AWS account ID.
nipAWSAccountId :: Lens' NetworkInterfacePermission (Maybe Text)
nipAWSAccountId = lens _nipAWSAccountId (\ s a -> s{_nipAWSAccountId = a})

-- | The AWS service.
nipAWSService :: Lens' NetworkInterfacePermission (Maybe Text)
nipAWSService = lens _nipAWSService (\ s a -> s{_nipAWSService = a})

-- | The type of permission.
nipPermission :: Lens' NetworkInterfacePermission (Maybe InterfacePermissionType)
nipPermission = lens _nipPermission (\ s a -> s{_nipPermission = a})

instance FromXML NetworkInterfacePermission where
        parseXML x
          = NetworkInterfacePermission' <$>
              (x .@? "permissionState") <*>
                (x .@? "networkInterfacePermissionId")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "awsAccountId")
                <*> (x .@? "awsService")
                <*> (x .@? "permission")

instance NFData NetworkInterfacePermission where

-- | Describes the state of a network interface permission.
--
--
--
-- /See:/ 'networkInterfacePermissionState' smart constructor.
data NetworkInterfacePermissionState = NetworkInterfacePermissionState'
  { _nipsState         :: !(Maybe NetworkInterfacePermissionStateCode)
  , _nipsStatusMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterfacePermissionState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nipsState' - The state of the permission.
--
-- * 'nipsStatusMessage' - A status message, if applicable.
networkInterfacePermissionState
    :: NetworkInterfacePermissionState
networkInterfacePermissionState =
  NetworkInterfacePermissionState'
    {_nipsState = Nothing, _nipsStatusMessage = Nothing}


-- | The state of the permission.
nipsState :: Lens' NetworkInterfacePermissionState (Maybe NetworkInterfacePermissionStateCode)
nipsState = lens _nipsState (\ s a -> s{_nipsState = a})

-- | A status message, if applicable.
nipsStatusMessage :: Lens' NetworkInterfacePermissionState (Maybe Text)
nipsStatusMessage = lens _nipsStatusMessage (\ s a -> s{_nipsStatusMessage = a})

instance FromXML NetworkInterfacePermissionState
         where
        parseXML x
          = NetworkInterfacePermissionState' <$>
              (x .@? "state") <*> (x .@? "statusMessage")

instance NFData NetworkInterfacePermissionState where

-- | Describes the private IPv4 address of a network interface.
--
--
--
-- /See:/ 'networkInterfacePrivateIPAddress' smart constructor.
data NetworkInterfacePrivateIPAddress = NetworkInterfacePrivateIPAddress'
  { _nipiaPrimary          :: !(Maybe Bool)
  , _nipiaPrivateIPAddress :: !(Maybe Text)
  , _nipiaPrivateDNSName   :: !(Maybe Text)
  , _nipiaAssociation      :: !(Maybe NetworkInterfaceAssociation)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterfacePrivateIPAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nipiaPrimary' - Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
--
-- * 'nipiaPrivateIPAddress' - The private IPv4 address.
--
-- * 'nipiaPrivateDNSName' - The private DNS name.
--
-- * 'nipiaAssociation' - The association information for an Elastic IP address (IPv4) associated with the network interface.
networkInterfacePrivateIPAddress
    :: NetworkInterfacePrivateIPAddress
networkInterfacePrivateIPAddress =
  NetworkInterfacePrivateIPAddress'
    { _nipiaPrimary = Nothing
    , _nipiaPrivateIPAddress = Nothing
    , _nipiaPrivateDNSName = Nothing
    , _nipiaAssociation = Nothing
    }


-- | Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
nipiaPrimary :: Lens' NetworkInterfacePrivateIPAddress (Maybe Bool)
nipiaPrimary = lens _nipiaPrimary (\ s a -> s{_nipiaPrimary = a})

-- | The private IPv4 address.
nipiaPrivateIPAddress :: Lens' NetworkInterfacePrivateIPAddress (Maybe Text)
nipiaPrivateIPAddress = lens _nipiaPrivateIPAddress (\ s a -> s{_nipiaPrivateIPAddress = a})

-- | The private DNS name.
nipiaPrivateDNSName :: Lens' NetworkInterfacePrivateIPAddress (Maybe Text)
nipiaPrivateDNSName = lens _nipiaPrivateDNSName (\ s a -> s{_nipiaPrivateDNSName = a})

-- | The association information for an Elastic IP address (IPv4) associated with the network interface.
nipiaAssociation :: Lens' NetworkInterfacePrivateIPAddress (Maybe NetworkInterfaceAssociation)
nipiaAssociation = lens _nipiaAssociation (\ s a -> s{_nipiaAssociation = a})

instance FromXML NetworkInterfacePrivateIPAddress
         where
        parseXML x
          = NetworkInterfacePrivateIPAddress' <$>
              (x .@? "primary") <*> (x .@? "privateIpAddress") <*>
                (x .@? "privateDnsName")
                <*> (x .@? "association")

instance NFData NetworkInterfacePrivateIPAddress
         where

-- | /See:/ 'newDHCPConfiguration' smart constructor.
data NewDHCPConfiguration = NewDHCPConfiguration'
  { _ndcValues :: !(Maybe [Text])
  , _ndcKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NewDHCPConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ndcValues' - Undocumented member.
--
-- * 'ndcKey' - Undocumented member.
newDHCPConfiguration
    :: NewDHCPConfiguration
newDHCPConfiguration =
  NewDHCPConfiguration' {_ndcValues = Nothing, _ndcKey = Nothing}


-- | Undocumented member.
ndcValues :: Lens' NewDHCPConfiguration [Text]
ndcValues = lens _ndcValues (\ s a -> s{_ndcValues = a}) . _Default . _Coerce

-- | Undocumented member.
ndcKey :: Lens' NewDHCPConfiguration (Maybe Text)
ndcKey = lens _ndcKey (\ s a -> s{_ndcKey = a})

instance NFData NewDHCPConfiguration where

instance ToQuery NewDHCPConfiguration where
        toQuery NewDHCPConfiguration'{..}
          = mconcat
              [toQuery (toQueryList "Value" <$> _ndcValues),
               "Key" =: _ndcKey]

-- | Describes the data that identifies an Amazon FPGA image (AFI) on the PCI bus.
--
--
--
-- /See:/ 'pciId' smart constructor.
data PciId = PciId'
  { _piSubsystemId       :: !(Maybe Text)
  , _piDeviceId          :: !(Maybe Text)
  , _piSubsystemVendorId :: !(Maybe Text)
  , _piVendorId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PciId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piSubsystemId' - The ID of the subsystem.
--
-- * 'piDeviceId' - The ID of the device.
--
-- * 'piSubsystemVendorId' - The ID of the vendor for the subsystem.
--
-- * 'piVendorId' - The ID of the vendor.
pciId
    :: PciId
pciId =
  PciId'
    { _piSubsystemId = Nothing
    , _piDeviceId = Nothing
    , _piSubsystemVendorId = Nothing
    , _piVendorId = Nothing
    }


-- | The ID of the subsystem.
piSubsystemId :: Lens' PciId (Maybe Text)
piSubsystemId = lens _piSubsystemId (\ s a -> s{_piSubsystemId = a})

-- | The ID of the device.
piDeviceId :: Lens' PciId (Maybe Text)
piDeviceId = lens _piDeviceId (\ s a -> s{_piDeviceId = a})

-- | The ID of the vendor for the subsystem.
piSubsystemVendorId :: Lens' PciId (Maybe Text)
piSubsystemVendorId = lens _piSubsystemVendorId (\ s a -> s{_piSubsystemVendorId = a})

-- | The ID of the vendor.
piVendorId :: Lens' PciId (Maybe Text)
piVendorId = lens _piVendorId (\ s a -> s{_piVendorId = a})

instance FromXML PciId where
        parseXML x
          = PciId' <$>
              (x .@? "SubsystemId") <*> (x .@? "DeviceId") <*>
                (x .@? "SubsystemVendorId")
                <*> (x .@? "VendorId")

instance NFData PciId where

-- | Describes the VPC peering connection options.
--
--
--
-- /See:/ 'peeringConnectionOptions' smart constructor.
data PeeringConnectionOptions = PeeringConnectionOptions'
  { _pcoAllowEgressFromLocalVPCToRemoteClassicLink :: !(Maybe Bool)
  , _pcoAllowEgressFromLocalClassicLinkToRemoteVPC :: !(Maybe Bool)
  , _pcoAllowDNSResolutionFromRemoteVPC            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PeeringConnectionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcoAllowEgressFromLocalVPCToRemoteClassicLink' - If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC via ClassicLink.
--
-- * 'pcoAllowEgressFromLocalClassicLinkToRemoteVPC' - If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC via ClassicLink to instances in a peer VPC.
--
-- * 'pcoAllowDNSResolutionFromRemoteVPC' - If true, the public DNS hostnames of instances in the specified VPC resolve to private IP addresses when queried from instances in the peer VPC.
peeringConnectionOptions
    :: PeeringConnectionOptions
peeringConnectionOptions =
  PeeringConnectionOptions'
    { _pcoAllowEgressFromLocalVPCToRemoteClassicLink = Nothing
    , _pcoAllowEgressFromLocalClassicLinkToRemoteVPC = Nothing
    , _pcoAllowDNSResolutionFromRemoteVPC = Nothing
    }


-- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC via ClassicLink.
pcoAllowEgressFromLocalVPCToRemoteClassicLink :: Lens' PeeringConnectionOptions (Maybe Bool)
pcoAllowEgressFromLocalVPCToRemoteClassicLink = lens _pcoAllowEgressFromLocalVPCToRemoteClassicLink (\ s a -> s{_pcoAllowEgressFromLocalVPCToRemoteClassicLink = a})

-- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC via ClassicLink to instances in a peer VPC.
pcoAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens' PeeringConnectionOptions (Maybe Bool)
pcoAllowEgressFromLocalClassicLinkToRemoteVPC = lens _pcoAllowEgressFromLocalClassicLinkToRemoteVPC (\ s a -> s{_pcoAllowEgressFromLocalClassicLinkToRemoteVPC = a})

-- | If true, the public DNS hostnames of instances in the specified VPC resolve to private IP addresses when queried from instances in the peer VPC.
pcoAllowDNSResolutionFromRemoteVPC :: Lens' PeeringConnectionOptions (Maybe Bool)
pcoAllowDNSResolutionFromRemoteVPC = lens _pcoAllowDNSResolutionFromRemoteVPC (\ s a -> s{_pcoAllowDNSResolutionFromRemoteVPC = a})

instance FromXML PeeringConnectionOptions where
        parseXML x
          = PeeringConnectionOptions' <$>
              (x .@? "allowEgressFromLocalVpcToRemoteClassicLink")
                <*>
                (x .@? "allowEgressFromLocalClassicLinkToRemoteVpc")
                <*> (x .@? "allowDnsResolutionFromRemoteVpc")

instance NFData PeeringConnectionOptions where

-- | The VPC peering connection options.
--
--
--
-- /See:/ 'peeringConnectionOptionsRequest' smart constructor.
data PeeringConnectionOptionsRequest = PeeringConnectionOptionsRequest'
  { _pcorAllowEgressFromLocalVPCToRemoteClassicLink :: !(Maybe Bool)
  , _pcorAllowEgressFromLocalClassicLinkToRemoteVPC :: !(Maybe Bool)
  , _pcorAllowDNSResolutionFromRemoteVPC            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PeeringConnectionOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcorAllowEgressFromLocalVPCToRemoteClassicLink' - If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC via ClassicLink.
--
-- * 'pcorAllowEgressFromLocalClassicLinkToRemoteVPC' - If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC via ClassicLink to instances in a peer VPC.
--
-- * 'pcorAllowDNSResolutionFromRemoteVPC' - If true, enables a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
peeringConnectionOptionsRequest
    :: PeeringConnectionOptionsRequest
peeringConnectionOptionsRequest =
  PeeringConnectionOptionsRequest'
    { _pcorAllowEgressFromLocalVPCToRemoteClassicLink = Nothing
    , _pcorAllowEgressFromLocalClassicLinkToRemoteVPC = Nothing
    , _pcorAllowDNSResolutionFromRemoteVPC = Nothing
    }


-- | If true, enables outbound communication from instances in a local VPC to an EC2-Classic instance that's linked to a peer VPC via ClassicLink.
pcorAllowEgressFromLocalVPCToRemoteClassicLink :: Lens' PeeringConnectionOptionsRequest (Maybe Bool)
pcorAllowEgressFromLocalVPCToRemoteClassicLink = lens _pcorAllowEgressFromLocalVPCToRemoteClassicLink (\ s a -> s{_pcorAllowEgressFromLocalVPCToRemoteClassicLink = a})

-- | If true, enables outbound communication from an EC2-Classic instance that's linked to a local VPC via ClassicLink to instances in a peer VPC.
pcorAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens' PeeringConnectionOptionsRequest (Maybe Bool)
pcorAllowEgressFromLocalClassicLinkToRemoteVPC = lens _pcorAllowEgressFromLocalClassicLinkToRemoteVPC (\ s a -> s{_pcorAllowEgressFromLocalClassicLinkToRemoteVPC = a})

-- | If true, enables a local VPC to resolve public DNS hostnames to private IP addresses when queried from instances in the peer VPC.
pcorAllowDNSResolutionFromRemoteVPC :: Lens' PeeringConnectionOptionsRequest (Maybe Bool)
pcorAllowDNSResolutionFromRemoteVPC = lens _pcorAllowDNSResolutionFromRemoteVPC (\ s a -> s{_pcorAllowDNSResolutionFromRemoteVPC = a})

instance NFData PeeringConnectionOptionsRequest where

instance ToQuery PeeringConnectionOptionsRequest
         where
        toQuery PeeringConnectionOptionsRequest'{..}
          = mconcat
              ["AllowEgressFromLocalVpcToRemoteClassicLink" =:
                 _pcorAllowEgressFromLocalVPCToRemoteClassicLink,
               "AllowEgressFromLocalClassicLinkToRemoteVpc" =:
                 _pcorAllowEgressFromLocalClassicLinkToRemoteVPC,
               "AllowDnsResolutionFromRemoteVpc" =:
                 _pcorAllowDNSResolutionFromRemoteVPC]

-- | Describes the placement of an instance.
--
--
--
-- /See:/ 'placement' smart constructor.
data Placement = Placement'
  { _pAffinity         :: !(Maybe Text)
  , _pHostId           :: !(Maybe Text)
  , _pSpreadDomain     :: !(Maybe Text)
  , _pAvailabilityZone :: !(Maybe Text)
  , _pTenancy          :: !(Maybe Tenancy)
  , _pGroupName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Placement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pAffinity' - The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the 'ImportInstance' command.
--
-- * 'pHostId' - The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the 'ImportInstance' command.
--
-- * 'pSpreadDomain' - Reserved for future use.
--
-- * 'pAvailabilityZone' - The Availability Zone of the instance.
--
-- * 'pTenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the 'ImportInstance' command.
--
-- * 'pGroupName' - The name of the placement group the instance is in (for cluster compute instances).
placement
    :: Placement
placement =
  Placement'
    { _pAffinity = Nothing
    , _pHostId = Nothing
    , _pSpreadDomain = Nothing
    , _pAvailabilityZone = Nothing
    , _pTenancy = Nothing
    , _pGroupName = Nothing
    }


-- | The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the 'ImportInstance' command.
pAffinity :: Lens' Placement (Maybe Text)
pAffinity = lens _pAffinity (\ s a -> s{_pAffinity = a})

-- | The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the 'ImportInstance' command.
pHostId :: Lens' Placement (Maybe Text)
pHostId = lens _pHostId (\ s a -> s{_pHostId = a})

-- | Reserved for future use.
pSpreadDomain :: Lens' Placement (Maybe Text)
pSpreadDomain = lens _pSpreadDomain (\ s a -> s{_pSpreadDomain = a})

-- | The Availability Zone of the instance.
pAvailabilityZone :: Lens' Placement (Maybe Text)
pAvailabilityZone = lens _pAvailabilityZone (\ s a -> s{_pAvailabilityZone = a})

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the 'ImportInstance' command.
pTenancy :: Lens' Placement (Maybe Tenancy)
pTenancy = lens _pTenancy (\ s a -> s{_pTenancy = a})

-- | The name of the placement group the instance is in (for cluster compute instances).
pGroupName :: Lens' Placement (Maybe Text)
pGroupName = lens _pGroupName (\ s a -> s{_pGroupName = a})

instance FromXML Placement where
        parseXML x
          = Placement' <$>
              (x .@? "affinity") <*> (x .@? "hostId") <*>
                (x .@? "spreadDomain")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "tenancy")
                <*> (x .@? "groupName")

instance NFData Placement where

instance ToQuery Placement where
        toQuery Placement'{..}
          = mconcat
              ["Affinity" =: _pAffinity, "HostId" =: _pHostId,
               "SpreadDomain" =: _pSpreadDomain,
               "AvailabilityZone" =: _pAvailabilityZone,
               "Tenancy" =: _pTenancy, "GroupName" =: _pGroupName]

-- | Describes a placement group.
--
--
--
-- /See:/ 'placementGroup' smart constructor.
data PlacementGroup = PlacementGroup'
  { _pgState     :: !(Maybe PlacementGroupState)
  , _pgStrategy  :: !(Maybe PlacementStrategy)
  , _pgGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlacementGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgState' - The state of the placement group.
--
-- * 'pgStrategy' - The placement strategy.
--
-- * 'pgGroupName' - The name of the placement group.
placementGroup
    :: PlacementGroup
placementGroup =
  PlacementGroup'
    {_pgState = Nothing, _pgStrategy = Nothing, _pgGroupName = Nothing}


-- | The state of the placement group.
pgState :: Lens' PlacementGroup (Maybe PlacementGroupState)
pgState = lens _pgState (\ s a -> s{_pgState = a})

-- | The placement strategy.
pgStrategy :: Lens' PlacementGroup (Maybe PlacementStrategy)
pgStrategy = lens _pgStrategy (\ s a -> s{_pgStrategy = a})

-- | The name of the placement group.
pgGroupName :: Lens' PlacementGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\ s a -> s{_pgGroupName = a})

instance FromXML PlacementGroup where
        parseXML x
          = PlacementGroup' <$>
              (x .@? "state") <*> (x .@? "strategy") <*>
                (x .@? "groupName")

instance NFData PlacementGroup where

-- | Describes a range of ports.
--
--
--
-- /See:/ 'portRange' smart constructor.
data PortRange = PortRange'
  { _prTo   :: !(Maybe Int)
  , _prFrom :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PortRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prTo' - The last port in the range.
--
-- * 'prFrom' - The first port in the range.
portRange
    :: PortRange
portRange = PortRange' {_prTo = Nothing, _prFrom = Nothing}


-- | The last port in the range.
prTo :: Lens' PortRange (Maybe Int)
prTo = lens _prTo (\ s a -> s{_prTo = a})

-- | The first port in the range.
prFrom :: Lens' PortRange (Maybe Int)
prFrom = lens _prFrom (\ s a -> s{_prFrom = a})

instance FromXML PortRange where
        parseXML x
          = PortRange' <$> (x .@? "to") <*> (x .@? "from")

instance NFData PortRange where

instance ToQuery PortRange where
        toQuery PortRange'{..}
          = mconcat ["To" =: _prTo, "From" =: _prFrom]

-- | Describes prefixes for AWS services.
--
--
--
-- /See:/ 'prefixList' smart constructor.
data PrefixList = PrefixList'
  { _plCidrs          :: !(Maybe [Text])
  , _plPrefixListId   :: !(Maybe Text)
  , _plPrefixListName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PrefixList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plCidrs' - The IP address range of the AWS service.
--
-- * 'plPrefixListId' - The ID of the prefix.
--
-- * 'plPrefixListName' - The name of the prefix.
prefixList
    :: PrefixList
prefixList =
  PrefixList'
    {_plCidrs = Nothing, _plPrefixListId = Nothing, _plPrefixListName = Nothing}


-- | The IP address range of the AWS service.
plCidrs :: Lens' PrefixList [Text]
plCidrs = lens _plCidrs (\ s a -> s{_plCidrs = a}) . _Default . _Coerce

-- | The ID of the prefix.
plPrefixListId :: Lens' PrefixList (Maybe Text)
plPrefixListId = lens _plPrefixListId (\ s a -> s{_plPrefixListId = a})

-- | The name of the prefix.
plPrefixListName :: Lens' PrefixList (Maybe Text)
plPrefixListName = lens _plPrefixListName (\ s a -> s{_plPrefixListName = a})

instance FromXML PrefixList where
        parseXML x
          = PrefixList' <$>
              (x .@? "cidrSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "prefixListId")
                <*> (x .@? "prefixListName")

instance NFData PrefixList where

-- | [EC2-VPC only] The ID of the prefix.
--
--
--
-- /See:/ 'prefixListId' smart constructor.
data PrefixListId = PrefixListId'
  { _pliPrefixListId :: !(Maybe Text)
  , _pliDescription  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PrefixListId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pliPrefixListId' - The ID of the prefix.
--
-- * 'pliDescription' - A description for the security group rule that references this prefix list ID. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
prefixListId
    :: PrefixListId
prefixListId =
  PrefixListId' {_pliPrefixListId = Nothing, _pliDescription = Nothing}


-- | The ID of the prefix.
pliPrefixListId :: Lens' PrefixListId (Maybe Text)
pliPrefixListId = lens _pliPrefixListId (\ s a -> s{_pliPrefixListId = a})

-- | A description for the security group rule that references this prefix list ID. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
pliDescription :: Lens' PrefixListId (Maybe Text)
pliDescription = lens _pliDescription (\ s a -> s{_pliDescription = a})

instance FromXML PrefixListId where
        parseXML x
          = PrefixListId' <$>
              (x .@? "prefixListId") <*> (x .@? "description")

instance NFData PrefixListId where

instance ToQuery PrefixListId where
        toQuery PrefixListId'{..}
          = mconcat
              ["PrefixListId" =: _pliPrefixListId,
               "Description" =: _pliDescription]

-- | Describes the price for a Reserved Instance.
--
--
--
-- /See:/ 'priceSchedule' smart constructor.
data PriceSchedule = PriceSchedule'
  { _psCurrencyCode :: !(Maybe CurrencyCodeValues)
  , _psTerm         :: !(Maybe Integer)
  , _psActive       :: !(Maybe Bool)
  , _psPrice        :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PriceSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psCurrencyCode' - The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
--
-- * 'psTerm' - The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
--
-- * 'psActive' - The current price schedule, as determined by the term remaining for the Reserved Instance in the listing. A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
--
-- * 'psPrice' - The fixed price for the term.
priceSchedule
    :: PriceSchedule
priceSchedule =
  PriceSchedule'
    { _psCurrencyCode = Nothing
    , _psTerm = Nothing
    , _psActive = Nothing
    , _psPrice = Nothing
    }


-- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
psCurrencyCode :: Lens' PriceSchedule (Maybe CurrencyCodeValues)
psCurrencyCode = lens _psCurrencyCode (\ s a -> s{_psCurrencyCode = a})

-- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
psTerm :: Lens' PriceSchedule (Maybe Integer)
psTerm = lens _psTerm (\ s a -> s{_psTerm = a})

-- | The current price schedule, as determined by the term remaining for the Reserved Instance in the listing. A specific price schedule is always in effect, but only one price schedule can be active at any time. Take, for example, a Reserved Instance listing that has five months remaining in its term. When you specify price schedules for five months and two months, this means that schedule 1, covering the first three months of the remaining term, will be active during months 5, 4, and 3. Then schedule 2, covering the last two months of the term, will be active for months 2 and 1.
psActive :: Lens' PriceSchedule (Maybe Bool)
psActive = lens _psActive (\ s a -> s{_psActive = a})

-- | The fixed price for the term.
psPrice :: Lens' PriceSchedule (Maybe Double)
psPrice = lens _psPrice (\ s a -> s{_psPrice = a})

instance FromXML PriceSchedule where
        parseXML x
          = PriceSchedule' <$>
              (x .@? "currencyCode") <*> (x .@? "term") <*>
                (x .@? "active")
                <*> (x .@? "price")

instance NFData PriceSchedule where

-- | Describes the price for a Reserved Instance.
--
--
--
-- /See:/ 'priceScheduleSpecification' smart constructor.
data PriceScheduleSpecification = PriceScheduleSpecification'
  { _pssCurrencyCode :: !(Maybe CurrencyCodeValues)
  , _pssTerm         :: !(Maybe Integer)
  , _pssPrice        :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PriceScheduleSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pssCurrencyCode' - The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
--
-- * 'pssTerm' - The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
--
-- * 'pssPrice' - The fixed price for the term.
priceScheduleSpecification
    :: PriceScheduleSpecification
priceScheduleSpecification =
  PriceScheduleSpecification'
    {_pssCurrencyCode = Nothing, _pssTerm = Nothing, _pssPrice = Nothing}


-- | The currency for transacting the Reserved Instance resale. At this time, the only supported currency is @USD@ .
pssCurrencyCode :: Lens' PriceScheduleSpecification (Maybe CurrencyCodeValues)
pssCurrencyCode = lens _pssCurrencyCode (\ s a -> s{_pssCurrencyCode = a})

-- | The number of months remaining in the reservation. For example, 2 is the second to the last month before the capacity reservation expires.
pssTerm :: Lens' PriceScheduleSpecification (Maybe Integer)
pssTerm = lens _pssTerm (\ s a -> s{_pssTerm = a})

-- | The fixed price for the term.
pssPrice :: Lens' PriceScheduleSpecification (Maybe Double)
pssPrice = lens _pssPrice (\ s a -> s{_pssPrice = a})

instance NFData PriceScheduleSpecification where

instance ToQuery PriceScheduleSpecification where
        toQuery PriceScheduleSpecification'{..}
          = mconcat
              ["CurrencyCode" =: _pssCurrencyCode,
               "Term" =: _pssTerm, "Price" =: _pssPrice]

-- | Describes a Reserved Instance offering.
--
--
--
-- /See:/ 'pricingDetail' smart constructor.
data PricingDetail = PricingDetail'
  { _pdCount :: !(Maybe Int)
  , _pdPrice :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PricingDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdCount' - The number of reservations available for the price.
--
-- * 'pdPrice' - The price per instance.
pricingDetail
    :: PricingDetail
pricingDetail = PricingDetail' {_pdCount = Nothing, _pdPrice = Nothing}


-- | The number of reservations available for the price.
pdCount :: Lens' PricingDetail (Maybe Int)
pdCount = lens _pdCount (\ s a -> s{_pdCount = a})

-- | The price per instance.
pdPrice :: Lens' PricingDetail (Maybe Double)
pdPrice = lens _pdPrice (\ s a -> s{_pdPrice = a})

instance FromXML PricingDetail where
        parseXML x
          = PricingDetail' <$>
              (x .@? "count") <*> (x .@? "price")

instance NFData PricingDetail where

-- | PrincipalIdFormat description
--
--
--
-- /See:/ 'principalIdFormat' smart constructor.
data PrincipalIdFormat = PrincipalIdFormat'
  { _pifARN      :: !(Maybe Text)
  , _pifStatuses :: !(Maybe [IdFormat])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PrincipalIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pifARN' - PrincipalIdFormatARN description
--
-- * 'pifStatuses' - PrincipalIdFormatStatuses description
principalIdFormat
    :: PrincipalIdFormat
principalIdFormat =
  PrincipalIdFormat' {_pifARN = Nothing, _pifStatuses = Nothing}


-- | PrincipalIdFormatARN description
pifARN :: Lens' PrincipalIdFormat (Maybe Text)
pifARN = lens _pifARN (\ s a -> s{_pifARN = a})

-- | PrincipalIdFormatStatuses description
pifStatuses :: Lens' PrincipalIdFormat [IdFormat]
pifStatuses = lens _pifStatuses (\ s a -> s{_pifStatuses = a}) . _Default . _Coerce

instance FromXML PrincipalIdFormat where
        parseXML x
          = PrincipalIdFormat' <$>
              (x .@? "arn") <*>
                (x .@? "statusSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData PrincipalIdFormat where

-- | Describes a secondary private IPv4 address for a network interface.
--
--
--
-- /See:/ 'privateIPAddressSpecification' smart constructor.
data PrivateIPAddressSpecification = PrivateIPAddressSpecification'
  { _piasPrimary          :: !(Maybe Bool)
  , _piasPrivateIPAddress :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PrivateIPAddressSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piasPrimary' - Indicates whether the private IPv4 address is the primary private IPv4 address. Only one IPv4 address can be designated as primary.
--
-- * 'piasPrivateIPAddress' - The private IPv4 addresses.
privateIPAddressSpecification
    :: Text -- ^ 'piasPrivateIPAddress'
    -> PrivateIPAddressSpecification
privateIPAddressSpecification pPrivateIPAddress_ =
  PrivateIPAddressSpecification'
    {_piasPrimary = Nothing, _piasPrivateIPAddress = pPrivateIPAddress_}


-- | Indicates whether the private IPv4 address is the primary private IPv4 address. Only one IPv4 address can be designated as primary.
piasPrimary :: Lens' PrivateIPAddressSpecification (Maybe Bool)
piasPrimary = lens _piasPrimary (\ s a -> s{_piasPrimary = a})

-- | The private IPv4 addresses.
piasPrivateIPAddress :: Lens' PrivateIPAddressSpecification Text
piasPrivateIPAddress = lens _piasPrivateIPAddress (\ s a -> s{_piasPrivateIPAddress = a})

instance FromXML PrivateIPAddressSpecification where
        parseXML x
          = PrivateIPAddressSpecification' <$>
              (x .@? "primary") <*> (x .@ "privateIpAddress")

instance NFData PrivateIPAddressSpecification where

instance ToQuery PrivateIPAddressSpecification where
        toQuery PrivateIPAddressSpecification'{..}
          = mconcat
              ["Primary" =: _piasPrimary,
               "PrivateIpAddress" =: _piasPrivateIPAddress]

-- | Describes a product code.
--
--
--
-- /See:/ 'productCode' smart constructor.
data ProductCode = ProductCode'
  { _pcProductCodeType :: !(Maybe ProductCodeValues)
  , _pcProductCodeId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProductCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcProductCodeType' - The type of product code.
--
-- * 'pcProductCodeId' - The product code.
productCode
    :: ProductCode
productCode =
  ProductCode' {_pcProductCodeType = Nothing, _pcProductCodeId = Nothing}


-- | The type of product code.
pcProductCodeType :: Lens' ProductCode (Maybe ProductCodeValues)
pcProductCodeType = lens _pcProductCodeType (\ s a -> s{_pcProductCodeType = a})

-- | The product code.
pcProductCodeId :: Lens' ProductCode (Maybe Text)
pcProductCodeId = lens _pcProductCodeId (\ s a -> s{_pcProductCodeId = a})

instance FromXML ProductCode where
        parseXML x
          = ProductCode' <$>
              (x .@? "type") <*> (x .@? "productCode")

instance NFData ProductCode where

-- | Describes a virtual private gateway propagating route.
--
--
--
-- /See:/ 'propagatingVGW' smart constructor.
newtype PropagatingVGW = PropagatingVGW'
  { _pvGatewayId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PropagatingVGW' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvGatewayId' - The ID of the virtual private gateway (VGW).
propagatingVGW
    :: PropagatingVGW
propagatingVGW = PropagatingVGW' {_pvGatewayId = Nothing}


-- | The ID of the virtual private gateway (VGW).
pvGatewayId :: Lens' PropagatingVGW (Maybe Text)
pvGatewayId = lens _pvGatewayId (\ s a -> s{_pvGatewayId = a})

instance FromXML PropagatingVGW where
        parseXML x = PropagatingVGW' <$> (x .@? "gatewayId")

instance NFData PropagatingVGW where

-- | Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
--
--
-- /See:/ 'provisionedBandwidth' smart constructor.
data ProvisionedBandwidth = ProvisionedBandwidth'
  { _pbStatus        :: !(Maybe Text)
  , _pbRequested     :: !(Maybe Text)
  , _pbProvisioned   :: !(Maybe Text)
  , _pbRequestTime   :: !(Maybe ISO8601)
  , _pbProvisionTime :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionedBandwidth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbStatus' - Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'pbRequested' - Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'pbProvisioned' - Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'pbRequestTime' - Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- * 'pbProvisionTime' - Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
provisionedBandwidth
    :: ProvisionedBandwidth
provisionedBandwidth =
  ProvisionedBandwidth'
    { _pbStatus = Nothing
    , _pbRequested = Nothing
    , _pbProvisioned = Nothing
    , _pbRequestTime = Nothing
    , _pbProvisionTime = Nothing
    }


-- | Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbStatus :: Lens' ProvisionedBandwidth (Maybe Text)
pbStatus = lens _pbStatus (\ s a -> s{_pbStatus = a})

-- | Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbRequested :: Lens' ProvisionedBandwidth (Maybe Text)
pbRequested = lens _pbRequested (\ s a -> s{_pbRequested = a})

-- | Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbProvisioned :: Lens' ProvisionedBandwidth (Maybe Text)
pbProvisioned = lens _pbProvisioned (\ s a -> s{_pbProvisioned = a})

-- | Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbRequestTime :: Lens' ProvisionedBandwidth (Maybe UTCTime)
pbRequestTime = lens _pbRequestTime (\ s a -> s{_pbRequestTime = a}) . mapping _Time

-- | Reserved. If you need to sustain traffic greater than the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
pbProvisionTime :: Lens' ProvisionedBandwidth (Maybe UTCTime)
pbProvisionTime = lens _pbProvisionTime (\ s a -> s{_pbProvisionTime = a}) . mapping _Time

instance FromXML ProvisionedBandwidth where
        parseXML x
          = ProvisionedBandwidth' <$>
              (x .@? "status") <*> (x .@? "requested") <*>
                (x .@? "provisioned")
                <*> (x .@? "requestTime")
                <*> (x .@? "provisionTime")

instance NFData ProvisionedBandwidth where

-- | Describes the result of the purchase.
--
--
--
-- /See:/ 'purchase' smart constructor.
data Purchase = Purchase'
  { _pInstanceFamily    :: !(Maybe Text)
  , _pCurrencyCode      :: !(Maybe CurrencyCodeValues)
  , _pHostReservationId :: !(Maybe Text)
  , _pHourlyPrice       :: !(Maybe Text)
  , _pUpfrontPrice      :: !(Maybe Text)
  , _pHostIdSet         :: !(Maybe [Text])
  , _pDuration          :: !(Maybe Int)
  , _pPaymentOption     :: !(Maybe PaymentOption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Purchase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pInstanceFamily' - The instance family on the Dedicated Host that the reservation can be associated with.
--
-- * 'pCurrencyCode' - The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- * 'pHostReservationId' - The ID of the reservation.
--
-- * 'pHourlyPrice' - The hourly price of the reservation per hour.
--
-- * 'pUpfrontPrice' - The upfront price of the reservation.
--
-- * 'pHostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
--
-- * 'pDuration' - The duration of the reservation's term in seconds.
--
-- * 'pPaymentOption' - The payment option for the reservation.
purchase
    :: Purchase
purchase =
  Purchase'
    { _pInstanceFamily = Nothing
    , _pCurrencyCode = Nothing
    , _pHostReservationId = Nothing
    , _pHourlyPrice = Nothing
    , _pUpfrontPrice = Nothing
    , _pHostIdSet = Nothing
    , _pDuration = Nothing
    , _pPaymentOption = Nothing
    }


-- | The instance family on the Dedicated Host that the reservation can be associated with.
pInstanceFamily :: Lens' Purchase (Maybe Text)
pInstanceFamily = lens _pInstanceFamily (\ s a -> s{_pInstanceFamily = a})

-- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
pCurrencyCode :: Lens' Purchase (Maybe CurrencyCodeValues)
pCurrencyCode = lens _pCurrencyCode (\ s a -> s{_pCurrencyCode = a})

-- | The ID of the reservation.
pHostReservationId :: Lens' Purchase (Maybe Text)
pHostReservationId = lens _pHostReservationId (\ s a -> s{_pHostReservationId = a})

-- | The hourly price of the reservation per hour.
pHourlyPrice :: Lens' Purchase (Maybe Text)
pHourlyPrice = lens _pHourlyPrice (\ s a -> s{_pHourlyPrice = a})

-- | The upfront price of the reservation.
pUpfrontPrice :: Lens' Purchase (Maybe Text)
pUpfrontPrice = lens _pUpfrontPrice (\ s a -> s{_pUpfrontPrice = a})

-- | The IDs of the Dedicated Hosts associated with the reservation.
pHostIdSet :: Lens' Purchase [Text]
pHostIdSet = lens _pHostIdSet (\ s a -> s{_pHostIdSet = a}) . _Default . _Coerce

-- | The duration of the reservation's term in seconds.
pDuration :: Lens' Purchase (Maybe Int)
pDuration = lens _pDuration (\ s a -> s{_pDuration = a})

-- | The payment option for the reservation.
pPaymentOption :: Lens' Purchase (Maybe PaymentOption)
pPaymentOption = lens _pPaymentOption (\ s a -> s{_pPaymentOption = a})

instance FromXML Purchase where
        parseXML x
          = Purchase' <$>
              (x .@? "instanceFamily") <*> (x .@? "currencyCode")
                <*> (x .@? "hostReservationId")
                <*> (x .@? "hourlyPrice")
                <*> (x .@? "upfrontPrice")
                <*>
                (x .@? "hostIdSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "duration")
                <*> (x .@? "paymentOption")

instance NFData Purchase where

-- | Describes a request to purchase Scheduled Instances.
--
--
--
-- /See:/ 'purchaseRequest' smart constructor.
data PurchaseRequest = PurchaseRequest'
  { _prInstanceCount :: !Int
  , _prPurchaseToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prInstanceCount' - The number of instances.
--
-- * 'prPurchaseToken' - The purchase token.
purchaseRequest
    :: Int -- ^ 'prInstanceCount'
    -> Text -- ^ 'prPurchaseToken'
    -> PurchaseRequest
purchaseRequest pInstanceCount_ pPurchaseToken_ =
  PurchaseRequest'
    {_prInstanceCount = pInstanceCount_, _prPurchaseToken = pPurchaseToken_}


-- | The number of instances.
prInstanceCount :: Lens' PurchaseRequest Int
prInstanceCount = lens _prInstanceCount (\ s a -> s{_prInstanceCount = a})

-- | The purchase token.
prPurchaseToken :: Lens' PurchaseRequest Text
prPurchaseToken = lens _prPurchaseToken (\ s a -> s{_prPurchaseToken = a})

instance NFData PurchaseRequest where

instance ToQuery PurchaseRequest where
        toQuery PurchaseRequest'{..}
          = mconcat
              ["InstanceCount" =: _prInstanceCount,
               "PurchaseToken" =: _prPurchaseToken]

-- | Describes a recurring charge.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcAmount    :: !(Maybe Double)
  , _rcFrequency :: !(Maybe RecurringChargeFrequency)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcAmount' - The amount of the recurring charge.
--
-- * 'rcFrequency' - The frequency of the recurring charge.
recurringCharge
    :: RecurringCharge
recurringCharge = RecurringCharge' {_rcAmount = Nothing, _rcFrequency = Nothing}


-- | The amount of the recurring charge.
rcAmount :: Lens' RecurringCharge (Maybe Double)
rcAmount = lens _rcAmount (\ s a -> s{_rcAmount = a})

-- | The frequency of the recurring charge.
rcFrequency :: Lens' RecurringCharge (Maybe RecurringChargeFrequency)
rcFrequency = lens _rcFrequency (\ s a -> s{_rcFrequency = a})

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "amount") <*> (x .@? "frequency")

instance NFData RecurringCharge where

-- | Describes a region.
--
--
--
-- /See:/ 'regionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { _riRegionName :: !(Maybe Text)
  , _riEndpoint   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riRegionName' - The name of the region.
--
-- * 'riEndpoint' - The region service endpoint.
regionInfo
    :: RegionInfo
regionInfo = RegionInfo' {_riRegionName = Nothing, _riEndpoint = Nothing}


-- | The name of the region.
riRegionName :: Lens' RegionInfo (Maybe Text)
riRegionName = lens _riRegionName (\ s a -> s{_riRegionName = a})

-- | The region service endpoint.
riEndpoint :: Lens' RegionInfo (Maybe Text)
riEndpoint = lens _riEndpoint (\ s a -> s{_riEndpoint = a})

instance FromXML RegionInfo where
        parseXML x
          = RegionInfo' <$>
              (x .@? "regionName") <*> (x .@? "regionEndpoint")

instance NFData RegionInfo where

-- | The information to include in the launch template.
--
--
--
-- /See:/ 'requestLaunchTemplateData' smart constructor.
data RequestLaunchTemplateData = RequestLaunchTemplateData'
  { _rltdSecurityGroupIds :: !(Maybe [Text])
  , _rltdSecurityGroups :: !(Maybe [Text])
  , _rltdInstanceMarketOptions :: !(Maybe LaunchTemplateInstanceMarketOptionsRequest)
  , _rltdDisableAPITermination :: !(Maybe Bool)
  , _rltdKeyName :: !(Maybe Text)
  , _rltdNetworkInterfaces :: !(Maybe [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest])
  , _rltdRamDiskId :: !(Maybe Text)
  , _rltdKernelId :: !(Maybe Text)
  , _rltdElasticGpuSpecifications :: !(Maybe [ElasticGpuSpecification])
  , _rltdInstanceType :: !(Maybe InstanceType)
  , _rltdEBSOptimized :: !(Maybe Bool)
  , _rltdUserData :: !(Maybe Text)
  , _rltdMonitoring :: !(Maybe LaunchTemplatesMonitoringRequest)
  , _rltdTagSpecifications :: !(Maybe [LaunchTemplateTagSpecificationRequest])
  , _rltdIAMInstanceProfile :: !(Maybe LaunchTemplateIAMInstanceProfileSpecificationRequest)
  , _rltdImageId :: !(Maybe Text)
  , _rltdInstanceInitiatedShutdownBehavior :: !(Maybe ShutdownBehavior)
  , _rltdCreditSpecification :: !(Maybe CreditSpecificationRequest)
  , _rltdBlockDeviceMappings :: !(Maybe [LaunchTemplateBlockDeviceMappingRequest])
  , _rltdPlacement :: !(Maybe LaunchTemplatePlacementRequest)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestLaunchTemplateData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rltdSecurityGroupIds' - One or more security group IDs. You can create a security group using 'CreateSecurityGroup' . You cannot specify both a security group ID and security name in the same request.
--
-- * 'rltdSecurityGroups' - [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. You cannot specify both a security group ID and security name in the same request.
--
-- * 'rltdInstanceMarketOptions' - The market (purchasing) option for the instances.
--
-- * 'rltdDisableAPITermination' - If set to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API. To change this attribute to @false@ after launch, use 'ModifyInstanceAttribute' .
--
-- * 'rltdKeyName' - The name of the key pair. You can create a key pair using 'CreateKeyPair' or 'ImportKeyPair' . /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
--
-- * 'rltdNetworkInterfaces' - One or more network interfaces.
--
-- * 'rltdRamDiskId' - The ID of the RAM disk. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdKernelId' - The ID of the kernel. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdElasticGpuSpecifications' - An elastic GPU to associate with the instance.
--
-- * 'rltdInstanceType' - The instance type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'rltdEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
--
-- * 'rltdUserData' - The Base64-encoded user data to make available to the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows).
--
-- * 'rltdMonitoring' - The monitoring for the instance.
--
-- * 'rltdTagSpecifications' - The tags to apply to the resources during launch. You can tag instances and volumes. The specified tags are applied to all instances or volumes that are created during launch.
--
-- * 'rltdIAMInstanceProfile' - The IAM instance profile.
--
-- * 'rltdImageId' - The ID of the AMI, which you can get by using 'DescribeImages' .
--
-- * 'rltdInstanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown). Default: @stop@
--
-- * 'rltdCreditSpecification' - The credit option for CPU usage of the instance. Valid for T2 instances only.
--
-- * 'rltdBlockDeviceMappings' - The block device mapping. /Important:/ Supplying both a snapshot ID and an encryption value as arguments for block-device mapping results in an error. This is because only blank volumes can be encrypted on start, and these are not created from a snapshot. If a snapshot is the basis for the volume, it contains data by definition and its encryption status cannot be changed using this action.
--
-- * 'rltdPlacement' - The placement for the instance.
requestLaunchTemplateData
    :: RequestLaunchTemplateData
requestLaunchTemplateData =
  RequestLaunchTemplateData'
    { _rltdSecurityGroupIds = Nothing
    , _rltdSecurityGroups = Nothing
    , _rltdInstanceMarketOptions = Nothing
    , _rltdDisableAPITermination = Nothing
    , _rltdKeyName = Nothing
    , _rltdNetworkInterfaces = Nothing
    , _rltdRamDiskId = Nothing
    , _rltdKernelId = Nothing
    , _rltdElasticGpuSpecifications = Nothing
    , _rltdInstanceType = Nothing
    , _rltdEBSOptimized = Nothing
    , _rltdUserData = Nothing
    , _rltdMonitoring = Nothing
    , _rltdTagSpecifications = Nothing
    , _rltdIAMInstanceProfile = Nothing
    , _rltdImageId = Nothing
    , _rltdInstanceInitiatedShutdownBehavior = Nothing
    , _rltdCreditSpecification = Nothing
    , _rltdBlockDeviceMappings = Nothing
    , _rltdPlacement = Nothing
    }


-- | One or more security group IDs. You can create a security group using 'CreateSecurityGroup' . You cannot specify both a security group ID and security name in the same request.
rltdSecurityGroupIds :: Lens' RequestLaunchTemplateData [Text]
rltdSecurityGroupIds = lens _rltdSecurityGroupIds (\ s a -> s{_rltdSecurityGroupIds = a}) . _Default . _Coerce

-- | [EC2-Classic, default VPC] One or more security group names. For a nondefault VPC, you must use security group IDs instead. You cannot specify both a security group ID and security name in the same request.
rltdSecurityGroups :: Lens' RequestLaunchTemplateData [Text]
rltdSecurityGroups = lens _rltdSecurityGroups (\ s a -> s{_rltdSecurityGroups = a}) . _Default . _Coerce

-- | The market (purchasing) option for the instances.
rltdInstanceMarketOptions :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateInstanceMarketOptionsRequest)
rltdInstanceMarketOptions = lens _rltdInstanceMarketOptions (\ s a -> s{_rltdInstanceMarketOptions = a})

-- | If set to @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API. To change this attribute to @false@ after launch, use 'ModifyInstanceAttribute' .
rltdDisableAPITermination :: Lens' RequestLaunchTemplateData (Maybe Bool)
rltdDisableAPITermination = lens _rltdDisableAPITermination (\ s a -> s{_rltdDisableAPITermination = a})

-- | The name of the key pair. You can create a key pair using 'CreateKeyPair' or 'ImportKeyPair' . /Important:/ If you do not specify a key pair, you can't connect to the instance unless you choose an AMI that is configured to allow users another way to log in.
rltdKeyName :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdKeyName = lens _rltdKeyName (\ s a -> s{_rltdKeyName = a})

-- | One or more network interfaces.
rltdNetworkInterfaces :: Lens' RequestLaunchTemplateData [LaunchTemplateInstanceNetworkInterfaceSpecificationRequest]
rltdNetworkInterfaces = lens _rltdNetworkInterfaces (\ s a -> s{_rltdNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdRamDiskId :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdRamDiskId = lens _rltdRamDiskId (\ s a -> s{_rltdRamDiskId = a})

-- | The ID of the kernel. /Important:/ We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedkernels.html User Provided Kernels> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdKernelId :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdKernelId = lens _rltdKernelId (\ s a -> s{_rltdKernelId = a})

-- | An elastic GPU to associate with the instance.
rltdElasticGpuSpecifications :: Lens' RequestLaunchTemplateData [ElasticGpuSpecification]
rltdElasticGpuSpecifications = lens _rltdElasticGpuSpecifications (\ s a -> s{_rltdElasticGpuSpecifications = a}) . _Default . _Coerce

-- | The instance type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
rltdInstanceType :: Lens' RequestLaunchTemplateData (Maybe InstanceType)
rltdInstanceType = lens _rltdInstanceType (\ s a -> s{_rltdInstanceType = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal Amazon EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance.
rltdEBSOptimized :: Lens' RequestLaunchTemplateData (Maybe Bool)
rltdEBSOptimized = lens _rltdEBSOptimized (\ s a -> s{_rltdEBSOptimized = a})

-- | The Base64-encoded user data to make available to the instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html Running Commands on Your Linux Instance at Launch> (Linux) and <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html#instancedata-add-user-data Adding User Data> (Windows).
rltdUserData :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdUserData = lens _rltdUserData (\ s a -> s{_rltdUserData = a})

-- | The monitoring for the instance.
rltdMonitoring :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplatesMonitoringRequest)
rltdMonitoring = lens _rltdMonitoring (\ s a -> s{_rltdMonitoring = a})

-- | The tags to apply to the resources during launch. You can tag instances and volumes. The specified tags are applied to all instances or volumes that are created during launch.
rltdTagSpecifications :: Lens' RequestLaunchTemplateData [LaunchTemplateTagSpecificationRequest]
rltdTagSpecifications = lens _rltdTagSpecifications (\ s a -> s{_rltdTagSpecifications = a}) . _Default . _Coerce

-- | The IAM instance profile.
rltdIAMInstanceProfile :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplateIAMInstanceProfileSpecificationRequest)
rltdIAMInstanceProfile = lens _rltdIAMInstanceProfile (\ s a -> s{_rltdIAMInstanceProfile = a})

-- | The ID of the AMI, which you can get by using 'DescribeImages' .
rltdImageId :: Lens' RequestLaunchTemplateData (Maybe Text)
rltdImageId = lens _rltdImageId (\ s a -> s{_rltdImageId = a})

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown). Default: @stop@
rltdInstanceInitiatedShutdownBehavior :: Lens' RequestLaunchTemplateData (Maybe ShutdownBehavior)
rltdInstanceInitiatedShutdownBehavior = lens _rltdInstanceInitiatedShutdownBehavior (\ s a -> s{_rltdInstanceInitiatedShutdownBehavior = a})

-- | The credit option for CPU usage of the instance. Valid for T2 instances only.
rltdCreditSpecification :: Lens' RequestLaunchTemplateData (Maybe CreditSpecificationRequest)
rltdCreditSpecification = lens _rltdCreditSpecification (\ s a -> s{_rltdCreditSpecification = a})

-- | The block device mapping. /Important:/ Supplying both a snapshot ID and an encryption value as arguments for block-device mapping results in an error. This is because only blank volumes can be encrypted on start, and these are not created from a snapshot. If a snapshot is the basis for the volume, it contains data by definition and its encryption status cannot be changed using this action.
rltdBlockDeviceMappings :: Lens' RequestLaunchTemplateData [LaunchTemplateBlockDeviceMappingRequest]
rltdBlockDeviceMappings = lens _rltdBlockDeviceMappings (\ s a -> s{_rltdBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement for the instance.
rltdPlacement :: Lens' RequestLaunchTemplateData (Maybe LaunchTemplatePlacementRequest)
rltdPlacement = lens _rltdPlacement (\ s a -> s{_rltdPlacement = a})

instance NFData RequestLaunchTemplateData where

instance ToQuery RequestLaunchTemplateData where
        toQuery RequestLaunchTemplateData'{..}
          = mconcat
              [toQuery
                 (toQueryList "SecurityGroupId" <$>
                    _rltdSecurityGroupIds),
               toQuery
                 (toQueryList "SecurityGroup" <$>
                    _rltdSecurityGroups),
               "InstanceMarketOptions" =:
                 _rltdInstanceMarketOptions,
               "DisableApiTermination" =:
                 _rltdDisableAPITermination,
               "KeyName" =: _rltdKeyName,
               toQuery
                 (toQueryList "NetworkInterface" <$>
                    _rltdNetworkInterfaces),
               "RamDiskId" =: _rltdRamDiskId,
               "KernelId" =: _rltdKernelId,
               toQuery
                 (toQueryList "ElasticGpuSpecification" <$>
                    _rltdElasticGpuSpecifications),
               "InstanceType" =: _rltdInstanceType,
               "EbsOptimized" =: _rltdEBSOptimized,
               "UserData" =: _rltdUserData,
               "Monitoring" =: _rltdMonitoring,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _rltdTagSpecifications),
               "IamInstanceProfile" =: _rltdIAMInstanceProfile,
               "ImageId" =: _rltdImageId,
               "InstanceInitiatedShutdownBehavior" =:
                 _rltdInstanceInitiatedShutdownBehavior,
               "CreditSpecification" =: _rltdCreditSpecification,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _rltdBlockDeviceMappings),
               "Placement" =: _rltdPlacement]

-- | Describes the launch specification for an instance.
--
--
--
-- /See:/ 'requestSpotLaunchSpecification' smart constructor.
data RequestSpotLaunchSpecification = RequestSpotLaunchSpecification'
  { _rslsSecurityGroupIds    :: !(Maybe [Text])
  , _rslsSecurityGroups      :: !(Maybe [Text])
  , _rslsKeyName             :: !(Maybe Text)
  , _rslsNetworkInterfaces   :: !(Maybe [InstanceNetworkInterfaceSpecification])
  , _rslsRAMDiskId           :: !(Maybe Text)
  , _rslsSubnetId            :: !(Maybe Text)
  , _rslsKernelId            :: !(Maybe Text)
  , _rslsInstanceType        :: !(Maybe InstanceType)
  , _rslsEBSOptimized        :: !(Maybe Bool)
  , _rslsUserData            :: !(Maybe Text)
  , _rslsMonitoring          :: !(Maybe RunInstancesMonitoringEnabled)
  , _rslsIAMInstanceProfile  :: !(Maybe IAMInstanceProfileSpecification)
  , _rslsImageId             :: !(Maybe Text)
  , _rslsAddressingType      :: !(Maybe Text)
  , _rslsBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  , _rslsPlacement           :: !(Maybe SpotPlacement)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestSpotLaunchSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rslsSecurityGroupIds' - One or more security group IDs.
--
-- * 'rslsSecurityGroups' - One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- * 'rslsKeyName' - The name of the key pair.
--
-- * 'rslsNetworkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- * 'rslsRAMDiskId' - The ID of the RAM disk.
--
-- * 'rslsSubnetId' - The ID of the subnet in which to launch the instance.
--
-- * 'rslsKernelId' - The ID of the kernel.
--
-- * 'rslsInstanceType' - The instance type.
--
-- * 'rslsEBSOptimized' - Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
--
-- * 'rslsUserData' - The Base64-encoded user data for the instance.
--
-- * 'rslsMonitoring' - Indicates whether basic or detailed monitoring is enabled for the instance. Default: Disabled
--
-- * 'rslsIAMInstanceProfile' - The IAM instance profile.
--
-- * 'rslsImageId' - The ID of the AMI.
--
-- * 'rslsAddressingType' - Deprecated.
--
-- * 'rslsBlockDeviceMappings' - One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- * 'rslsPlacement' - The placement information for the instance.
requestSpotLaunchSpecification
    :: RequestSpotLaunchSpecification
requestSpotLaunchSpecification =
  RequestSpotLaunchSpecification'
    { _rslsSecurityGroupIds = Nothing
    , _rslsSecurityGroups = Nothing
    , _rslsKeyName = Nothing
    , _rslsNetworkInterfaces = Nothing
    , _rslsRAMDiskId = Nothing
    , _rslsSubnetId = Nothing
    , _rslsKernelId = Nothing
    , _rslsInstanceType = Nothing
    , _rslsEBSOptimized = Nothing
    , _rslsUserData = Nothing
    , _rslsMonitoring = Nothing
    , _rslsIAMInstanceProfile = Nothing
    , _rslsImageId = Nothing
    , _rslsAddressingType = Nothing
    , _rslsBlockDeviceMappings = Nothing
    , _rslsPlacement = Nothing
    }


-- | One or more security group IDs.
rslsSecurityGroupIds :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroupIds = lens _rslsSecurityGroupIds (\ s a -> s{_rslsSecurityGroupIds = a}) . _Default . _Coerce

-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
rslsSecurityGroups :: Lens' RequestSpotLaunchSpecification [Text]
rslsSecurityGroups = lens _rslsSecurityGroups (\ s a -> s{_rslsSecurityGroups = a}) . _Default . _Coerce

-- | The name of the key pair.
rslsKeyName :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKeyName = lens _rslsKeyName (\ s a -> s{_rslsKeyName = a})

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
rslsNetworkInterfaces :: Lens' RequestSpotLaunchSpecification [InstanceNetworkInterfaceSpecification]
rslsNetworkInterfaces = lens _rslsNetworkInterfaces (\ s a -> s{_rslsNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk.
rslsRAMDiskId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsRAMDiskId = lens _rslsRAMDiskId (\ s a -> s{_rslsRAMDiskId = a})

-- | The ID of the subnet in which to launch the instance.
rslsSubnetId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsSubnetId = lens _rslsSubnetId (\ s a -> s{_rslsSubnetId = a})

-- | The ID of the kernel.
rslsKernelId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsKernelId = lens _rslsKernelId (\ s a -> s{_rslsKernelId = a})

-- | The instance type.
rslsInstanceType :: Lens' RequestSpotLaunchSpecification (Maybe InstanceType)
rslsInstanceType = lens _rslsInstanceType (\ s a -> s{_rslsInstanceType = a})

-- | Indicates whether the instance is optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
rslsEBSOptimized :: Lens' RequestSpotLaunchSpecification (Maybe Bool)
rslsEBSOptimized = lens _rslsEBSOptimized (\ s a -> s{_rslsEBSOptimized = a})

-- | The Base64-encoded user data for the instance.
rslsUserData :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsUserData = lens _rslsUserData (\ s a -> s{_rslsUserData = a})

-- | Indicates whether basic or detailed monitoring is enabled for the instance. Default: Disabled
rslsMonitoring :: Lens' RequestSpotLaunchSpecification (Maybe RunInstancesMonitoringEnabled)
rslsMonitoring = lens _rslsMonitoring (\ s a -> s{_rslsMonitoring = a})

-- | The IAM instance profile.
rslsIAMInstanceProfile :: Lens' RequestSpotLaunchSpecification (Maybe IAMInstanceProfileSpecification)
rslsIAMInstanceProfile = lens _rslsIAMInstanceProfile (\ s a -> s{_rslsIAMInstanceProfile = a})

-- | The ID of the AMI.
rslsImageId :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsImageId = lens _rslsImageId (\ s a -> s{_rslsImageId = a})

-- | Deprecated.
rslsAddressingType :: Lens' RequestSpotLaunchSpecification (Maybe Text)
rslsAddressingType = lens _rslsAddressingType (\ s a -> s{_rslsAddressingType = a})

-- | One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
rslsBlockDeviceMappings :: Lens' RequestSpotLaunchSpecification [BlockDeviceMapping]
rslsBlockDeviceMappings = lens _rslsBlockDeviceMappings (\ s a -> s{_rslsBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement information for the instance.
rslsPlacement :: Lens' RequestSpotLaunchSpecification (Maybe SpotPlacement)
rslsPlacement = lens _rslsPlacement (\ s a -> s{_rslsPlacement = a})

instance NFData RequestSpotLaunchSpecification where

instance ToQuery RequestSpotLaunchSpecification where
        toQuery RequestSpotLaunchSpecification'{..}
          = mconcat
              [toQuery
                 (toQueryList "SecurityGroupId" <$>
                    _rslsSecurityGroupIds),
               toQuery
                 (toQueryList "SecurityGroup" <$>
                    _rslsSecurityGroups),
               "KeyName" =: _rslsKeyName,
               toQuery
                 (toQueryList "NetworkInterface" <$>
                    _rslsNetworkInterfaces),
               "RamdiskId" =: _rslsRAMDiskId,
               "SubnetId" =: _rslsSubnetId,
               "KernelId" =: _rslsKernelId,
               "InstanceType" =: _rslsInstanceType,
               "EbsOptimized" =: _rslsEBSOptimized,
               "UserData" =: _rslsUserData,
               "Monitoring" =: _rslsMonitoring,
               "IamInstanceProfile" =: _rslsIAMInstanceProfile,
               "ImageId" =: _rslsImageId,
               "AddressingType" =: _rslsAddressingType,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _rslsBlockDeviceMappings),
               "Placement" =: _rslsPlacement]

-- | Describes a reservation.
--
--
--
-- /See:/ 'reservation' smart constructor.
data Reservation = Reservation'
  { _rGroups        :: !(Maybe [GroupIdentifier])
  , _rInstances     :: !(Maybe [Instance])
  , _rRequesterId   :: !(Maybe Text)
  , _rReservationId :: !Text
  , _rOwnerId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Reservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rGroups' - [EC2-Classic only] One or more security groups.
--
-- * 'rInstances' - One or more instances.
--
-- * 'rRequesterId' - The ID of the requester that launched the instances on your behalf (for example, AWS Management Console or Auto Scaling).
--
-- * 'rReservationId' - The ID of the reservation.
--
-- * 'rOwnerId' - The ID of the AWS account that owns the reservation.
reservation
    :: Text -- ^ 'rReservationId'
    -> Text -- ^ 'rOwnerId'
    -> Reservation
reservation pReservationId_ pOwnerId_ =
  Reservation'
    { _rGroups = Nothing
    , _rInstances = Nothing
    , _rRequesterId = Nothing
    , _rReservationId = pReservationId_
    , _rOwnerId = pOwnerId_
    }


-- | [EC2-Classic only] One or more security groups.
rGroups :: Lens' Reservation [GroupIdentifier]
rGroups = lens _rGroups (\ s a -> s{_rGroups = a}) . _Default . _Coerce

-- | One or more instances.
rInstances :: Lens' Reservation [Instance]
rInstances = lens _rInstances (\ s a -> s{_rInstances = a}) . _Default . _Coerce

-- | The ID of the requester that launched the instances on your behalf (for example, AWS Management Console or Auto Scaling).
rRequesterId :: Lens' Reservation (Maybe Text)
rRequesterId = lens _rRequesterId (\ s a -> s{_rRequesterId = a})

-- | The ID of the reservation.
rReservationId :: Lens' Reservation Text
rReservationId = lens _rReservationId (\ s a -> s{_rReservationId = a})

-- | The ID of the AWS account that owns the reservation.
rOwnerId :: Lens' Reservation Text
rOwnerId = lens _rOwnerId (\ s a -> s{_rOwnerId = a})

instance FromXML Reservation where
        parseXML x
          = Reservation' <$>
              (x .@? "groupSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*>
                (x .@? "instancesSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "requesterId")
                <*> (x .@ "reservationId")
                <*> (x .@ "ownerId")

instance NFData Reservation where

-- | The cost associated with the Reserved Instance.
--
--
--
-- /See:/ 'reservationValue' smart constructor.
data ReservationValue = ReservationValue'
  { _rvHourlyPrice           :: !(Maybe Text)
  , _rvRemainingTotalValue   :: !(Maybe Text)
  , _rvRemainingUpfrontValue :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rvHourlyPrice' - The hourly rate of the reservation.
--
-- * 'rvRemainingTotalValue' - The balance of the total value (the sum of remainingUpfrontValue + hourlyPrice * number of hours remaining).
--
-- * 'rvRemainingUpfrontValue' - The remaining upfront cost of the reservation.
reservationValue
    :: ReservationValue
reservationValue =
  ReservationValue'
    { _rvHourlyPrice = Nothing
    , _rvRemainingTotalValue = Nothing
    , _rvRemainingUpfrontValue = Nothing
    }


-- | The hourly rate of the reservation.
rvHourlyPrice :: Lens' ReservationValue (Maybe Text)
rvHourlyPrice = lens _rvHourlyPrice (\ s a -> s{_rvHourlyPrice = a})

-- | The balance of the total value (the sum of remainingUpfrontValue + hourlyPrice * number of hours remaining).
rvRemainingTotalValue :: Lens' ReservationValue (Maybe Text)
rvRemainingTotalValue = lens _rvRemainingTotalValue (\ s a -> s{_rvRemainingTotalValue = a})

-- | The remaining upfront cost of the reservation.
rvRemainingUpfrontValue :: Lens' ReservationValue (Maybe Text)
rvRemainingUpfrontValue = lens _rvRemainingUpfrontValue (\ s a -> s{_rvRemainingUpfrontValue = a})

instance FromXML ReservationValue where
        parseXML x
          = ReservationValue' <$>
              (x .@? "hourlyPrice") <*>
                (x .@? "remainingTotalValue")
                <*> (x .@? "remainingUpfrontValue")

instance NFData ReservationValue where

-- | Describes the limit price of a Reserved Instance offering.
--
--
--
-- /See:/ 'reservedInstanceLimitPrice' smart constructor.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice'
  { _rilpAmount       :: !(Maybe Double)
  , _rilpCurrencyCode :: !(Maybe CurrencyCodeValues)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstanceLimitPrice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rilpAmount' - Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
--
-- * 'rilpCurrencyCode' - The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
reservedInstanceLimitPrice
    :: ReservedInstanceLimitPrice
reservedInstanceLimitPrice =
  ReservedInstanceLimitPrice'
    {_rilpAmount = Nothing, _rilpCurrencyCode = Nothing}


-- | Used for Reserved Instance Marketplace offerings. Specifies the limit price on the total order (instanceCount * price).
rilpAmount :: Lens' ReservedInstanceLimitPrice (Maybe Double)
rilpAmount = lens _rilpAmount (\ s a -> s{_rilpAmount = a})

-- | The currency in which the @limitPrice@ amount is specified. At this time, the only supported currency is @USD@ .
rilpCurrencyCode :: Lens' ReservedInstanceLimitPrice (Maybe CurrencyCodeValues)
rilpCurrencyCode = lens _rilpCurrencyCode (\ s a -> s{_rilpCurrencyCode = a})

instance NFData ReservedInstanceLimitPrice where

instance ToQuery ReservedInstanceLimitPrice where
        toQuery ReservedInstanceLimitPrice'{..}
          = mconcat
              ["Amount" =: _rilpAmount,
               "CurrencyCode" =: _rilpCurrencyCode]

-- | The total value of the Convertible Reserved Instance.
--
--
--
-- /See:/ 'reservedInstanceReservationValue' smart constructor.
data ReservedInstanceReservationValue = ReservedInstanceReservationValue'
  { _rirvReservationValue   :: !(Maybe ReservationValue)
  , _rirvReservedInstanceId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstanceReservationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirvReservationValue' - The total value of the Convertible Reserved Instance that you are exchanging.
--
-- * 'rirvReservedInstanceId' - The ID of the Convertible Reserved Instance that you are exchanging.
reservedInstanceReservationValue
    :: ReservedInstanceReservationValue
reservedInstanceReservationValue =
  ReservedInstanceReservationValue'
    {_rirvReservationValue = Nothing, _rirvReservedInstanceId = Nothing}


-- | The total value of the Convertible Reserved Instance that you are exchanging.
rirvReservationValue :: Lens' ReservedInstanceReservationValue (Maybe ReservationValue)
rirvReservationValue = lens _rirvReservationValue (\ s a -> s{_rirvReservationValue = a})

-- | The ID of the Convertible Reserved Instance that you are exchanging.
rirvReservedInstanceId :: Lens' ReservedInstanceReservationValue (Maybe Text)
rirvReservedInstanceId = lens _rirvReservedInstanceId (\ s a -> s{_rirvReservedInstanceId = a})

instance FromXML ReservedInstanceReservationValue
         where
        parseXML x
          = ReservedInstanceReservationValue' <$>
              (x .@? "reservationValue") <*>
                (x .@? "reservedInstanceId")

instance NFData ReservedInstanceReservationValue
         where

-- | Describes a Reserved Instance.
--
--
--
-- /See:/ 'reservedInstances' smart constructor.
data ReservedInstances = ReservedInstances'
  { _riState               :: !(Maybe ReservedInstanceState)
  , _riCurrencyCode        :: !(Maybe CurrencyCodeValues)
  , _riInstanceCount       :: !(Maybe Int)
  , _riProductDescription  :: !(Maybe RIProductDescription)
  , _riStart               :: !(Maybe ISO8601)
  , _riInstanceType        :: !(Maybe InstanceType)
  , _riEnd                 :: !(Maybe ISO8601)
  , _riAvailabilityZone    :: !(Maybe Text)
  , _riScope               :: !(Maybe Scope)
  , _riRecurringCharges    :: !(Maybe [RecurringCharge])
  , _riOfferingType        :: !(Maybe OfferingTypeValues)
  , _riUsagePrice          :: !(Maybe Double)
  , _riFixedPrice          :: !(Maybe Double)
  , _riReservedInstancesId :: !(Maybe Text)
  , _riInstanceTenancy     :: !(Maybe Tenancy)
  , _riOfferingClass       :: !(Maybe OfferingClassType)
  , _riDuration            :: !(Maybe Integer)
  , _riTags                :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riState' - The state of the Reserved Instance purchase.
--
-- * 'riCurrencyCode' - The currency of the Reserved Instance. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
--
-- * 'riInstanceCount' - The number of reservations purchased.
--
-- * 'riProductDescription' - The Reserved Instance product platform description.
--
-- * 'riStart' - The date and time the Reserved Instance started.
--
-- * 'riInstanceType' - The instance type on which the Reserved Instance can be used.
--
-- * 'riEnd' - The time when the Reserved Instance expires.
--
-- * 'riAvailabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- * 'riScope' - The scope of the Reserved Instance.
--
-- * 'riRecurringCharges' - The recurring charge tag assigned to the resource.
--
-- * 'riOfferingType' - The Reserved Instance offering type.
--
-- * 'riUsagePrice' - The usage price of the Reserved Instance, per hour.
--
-- * 'riFixedPrice' - The purchase price of the Reserved Instance.
--
-- * 'riReservedInstancesId' - The ID of the Reserved Instance.
--
-- * 'riInstanceTenancy' - The tenancy of the instance.
--
-- * 'riOfferingClass' - The offering class of the Reserved Instance.
--
-- * 'riDuration' - The duration of the Reserved Instance, in seconds.
--
-- * 'riTags' - Any tags assigned to the resource.
reservedInstances
    :: ReservedInstances
reservedInstances =
  ReservedInstances'
    { _riState = Nothing
    , _riCurrencyCode = Nothing
    , _riInstanceCount = Nothing
    , _riProductDescription = Nothing
    , _riStart = Nothing
    , _riInstanceType = Nothing
    , _riEnd = Nothing
    , _riAvailabilityZone = Nothing
    , _riScope = Nothing
    , _riRecurringCharges = Nothing
    , _riOfferingType = Nothing
    , _riUsagePrice = Nothing
    , _riFixedPrice = Nothing
    , _riReservedInstancesId = Nothing
    , _riInstanceTenancy = Nothing
    , _riOfferingClass = Nothing
    , _riDuration = Nothing
    , _riTags = Nothing
    }


-- | The state of the Reserved Instance purchase.
riState :: Lens' ReservedInstances (Maybe ReservedInstanceState)
riState = lens _riState (\ s a -> s{_riState = a})

-- | The currency of the Reserved Instance. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
riCurrencyCode :: Lens' ReservedInstances (Maybe CurrencyCodeValues)
riCurrencyCode = lens _riCurrencyCode (\ s a -> s{_riCurrencyCode = a})

-- | The number of reservations purchased.
riInstanceCount :: Lens' ReservedInstances (Maybe Int)
riInstanceCount = lens _riInstanceCount (\ s a -> s{_riInstanceCount = a})

-- | The Reserved Instance product platform description.
riProductDescription :: Lens' ReservedInstances (Maybe RIProductDescription)
riProductDescription = lens _riProductDescription (\ s a -> s{_riProductDescription = a})

-- | The date and time the Reserved Instance started.
riStart :: Lens' ReservedInstances (Maybe UTCTime)
riStart = lens _riStart (\ s a -> s{_riStart = a}) . mapping _Time

-- | The instance type on which the Reserved Instance can be used.
riInstanceType :: Lens' ReservedInstances (Maybe InstanceType)
riInstanceType = lens _riInstanceType (\ s a -> s{_riInstanceType = a})

-- | The time when the Reserved Instance expires.
riEnd :: Lens' ReservedInstances (Maybe UTCTime)
riEnd = lens _riEnd (\ s a -> s{_riEnd = a}) . mapping _Time

-- | The Availability Zone in which the Reserved Instance can be used.
riAvailabilityZone :: Lens' ReservedInstances (Maybe Text)
riAvailabilityZone = lens _riAvailabilityZone (\ s a -> s{_riAvailabilityZone = a})

-- | The scope of the Reserved Instance.
riScope :: Lens' ReservedInstances (Maybe Scope)
riScope = lens _riScope (\ s a -> s{_riScope = a})

-- | The recurring charge tag assigned to the resource.
riRecurringCharges :: Lens' ReservedInstances [RecurringCharge]
riRecurringCharges = lens _riRecurringCharges (\ s a -> s{_riRecurringCharges = a}) . _Default . _Coerce

-- | The Reserved Instance offering type.
riOfferingType :: Lens' ReservedInstances (Maybe OfferingTypeValues)
riOfferingType = lens _riOfferingType (\ s a -> s{_riOfferingType = a})

-- | The usage price of the Reserved Instance, per hour.
riUsagePrice :: Lens' ReservedInstances (Maybe Double)
riUsagePrice = lens _riUsagePrice (\ s a -> s{_riUsagePrice = a})

-- | The purchase price of the Reserved Instance.
riFixedPrice :: Lens' ReservedInstances (Maybe Double)
riFixedPrice = lens _riFixedPrice (\ s a -> s{_riFixedPrice = a})

-- | The ID of the Reserved Instance.
riReservedInstancesId :: Lens' ReservedInstances (Maybe Text)
riReservedInstancesId = lens _riReservedInstancesId (\ s a -> s{_riReservedInstancesId = a})

-- | The tenancy of the instance.
riInstanceTenancy :: Lens' ReservedInstances (Maybe Tenancy)
riInstanceTenancy = lens _riInstanceTenancy (\ s a -> s{_riInstanceTenancy = a})

-- | The offering class of the Reserved Instance.
riOfferingClass :: Lens' ReservedInstances (Maybe OfferingClassType)
riOfferingClass = lens _riOfferingClass (\ s a -> s{_riOfferingClass = a})

-- | The duration of the Reserved Instance, in seconds.
riDuration :: Lens' ReservedInstances (Maybe Integer)
riDuration = lens _riDuration (\ s a -> s{_riDuration = a})

-- | Any tags assigned to the resource.
riTags :: Lens' ReservedInstances [Tag]
riTags = lens _riTags (\ s a -> s{_riTags = a}) . _Default . _Coerce

instance FromXML ReservedInstances where
        parseXML x
          = ReservedInstances' <$>
              (x .@? "state") <*> (x .@? "currencyCode") <*>
                (x .@? "instanceCount")
                <*> (x .@? "productDescription")
                <*> (x .@? "start")
                <*> (x .@? "instanceType")
                <*> (x .@? "end")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "scope")
                <*>
                (x .@? "recurringCharges" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "offeringType")
                <*> (x .@? "usagePrice")
                <*> (x .@? "fixedPrice")
                <*> (x .@? "reservedInstancesId")
                <*> (x .@? "instanceTenancy")
                <*> (x .@? "offeringClass")
                <*> (x .@? "duration")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData ReservedInstances where

-- | Describes the configuration settings for the modified Reserved Instances.
--
--
--
-- /See:/ 'reservedInstancesConfiguration' smart constructor.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration'
  { _ricPlatform         :: !(Maybe Text)
  , _ricInstanceCount    :: !(Maybe Int)
  , _ricInstanceType     :: !(Maybe InstanceType)
  , _ricAvailabilityZone :: !(Maybe Text)
  , _ricScope            :: !(Maybe Scope)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstancesConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ricPlatform' - The network platform of the modified Reserved Instances, which is either EC2-Classic or EC2-VPC.
--
-- * 'ricInstanceCount' - The number of modified Reserved Instances.
--
-- * 'ricInstanceType' - The instance type for the modified Reserved Instances.
--
-- * 'ricAvailabilityZone' - The Availability Zone for the modified Reserved Instances.
--
-- * 'ricScope' - Whether the Reserved Instance is applied to instances in a region or instances in a specific Availability Zone.
reservedInstancesConfiguration
    :: ReservedInstancesConfiguration
reservedInstancesConfiguration =
  ReservedInstancesConfiguration'
    { _ricPlatform = Nothing
    , _ricInstanceCount = Nothing
    , _ricInstanceType = Nothing
    , _ricAvailabilityZone = Nothing
    , _ricScope = Nothing
    }


-- | The network platform of the modified Reserved Instances, which is either EC2-Classic or EC2-VPC.
ricPlatform :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricPlatform = lens _ricPlatform (\ s a -> s{_ricPlatform = a})

-- | The number of modified Reserved Instances.
ricInstanceCount :: Lens' ReservedInstancesConfiguration (Maybe Int)
ricInstanceCount = lens _ricInstanceCount (\ s a -> s{_ricInstanceCount = a})

-- | The instance type for the modified Reserved Instances.
ricInstanceType :: Lens' ReservedInstancesConfiguration (Maybe InstanceType)
ricInstanceType = lens _ricInstanceType (\ s a -> s{_ricInstanceType = a})

-- | The Availability Zone for the modified Reserved Instances.
ricAvailabilityZone :: Lens' ReservedInstancesConfiguration (Maybe Text)
ricAvailabilityZone = lens _ricAvailabilityZone (\ s a -> s{_ricAvailabilityZone = a})

-- | Whether the Reserved Instance is applied to instances in a region or instances in a specific Availability Zone.
ricScope :: Lens' ReservedInstancesConfiguration (Maybe Scope)
ricScope = lens _ricScope (\ s a -> s{_ricScope = a})

instance FromXML ReservedInstancesConfiguration where
        parseXML x
          = ReservedInstancesConfiguration' <$>
              (x .@? "platform") <*> (x .@? "instanceCount") <*>
                (x .@? "instanceType")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "scope")

instance NFData ReservedInstancesConfiguration where

instance ToQuery ReservedInstancesConfiguration where
        toQuery ReservedInstancesConfiguration'{..}
          = mconcat
              ["Platform" =: _ricPlatform,
               "InstanceCount" =: _ricInstanceCount,
               "InstanceType" =: _ricInstanceType,
               "AvailabilityZone" =: _ricAvailabilityZone,
               "Scope" =: _ricScope]

-- | Describes the ID of a Reserved Instance.
--
--
--
-- /See:/ 'reservedInstancesId' smart constructor.
newtype ReservedInstancesId = ReservedInstancesId'
  { _riiReservedInstancesId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstancesId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riiReservedInstancesId' - The ID of the Reserved Instance.
reservedInstancesId
    :: ReservedInstancesId
reservedInstancesId = ReservedInstancesId' {_riiReservedInstancesId = Nothing}


-- | The ID of the Reserved Instance.
riiReservedInstancesId :: Lens' ReservedInstancesId (Maybe Text)
riiReservedInstancesId = lens _riiReservedInstancesId (\ s a -> s{_riiReservedInstancesId = a})

instance FromXML ReservedInstancesId where
        parseXML x
          = ReservedInstancesId' <$>
              (x .@? "reservedInstancesId")

instance NFData ReservedInstancesId where

-- | Describes a Reserved Instance listing.
--
--
--
-- /See:/ 'reservedInstancesListing' smart constructor.
data ReservedInstancesListing = ReservedInstancesListing'
  { _rilStatus                     :: !(Maybe ListingStatus)
  , _rilClientToken                :: !(Maybe Text)
  , _rilUpdateDate                 :: !(Maybe ISO8601)
  , _rilCreateDate                 :: !(Maybe ISO8601)
  , _rilPriceSchedules             :: !(Maybe [PriceSchedule])
  , _rilStatusMessage              :: !(Maybe Text)
  , _rilReservedInstancesId        :: !(Maybe Text)
  , _rilTags                       :: !(Maybe [Tag])
  , _rilInstanceCounts             :: !(Maybe [InstanceCount])
  , _rilReservedInstancesListingId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstancesListing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rilStatus' - The status of the Reserved Instance listing.
--
-- * 'rilClientToken' - A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'rilUpdateDate' - The last modified timestamp of the listing.
--
-- * 'rilCreateDate' - The time the listing was created.
--
-- * 'rilPriceSchedules' - The price of the Reserved Instance listing.
--
-- * 'rilStatusMessage' - The reason for the current status of the Reserved Instance listing. The response can be blank.
--
-- * 'rilReservedInstancesId' - The ID of the Reserved Instance.
--
-- * 'rilTags' - Any tags assigned to the resource.
--
-- * 'rilInstanceCounts' - The number of instances in this state.
--
-- * 'rilReservedInstancesListingId' - The ID of the Reserved Instance listing.
reservedInstancesListing
    :: ReservedInstancesListing
reservedInstancesListing =
  ReservedInstancesListing'
    { _rilStatus = Nothing
    , _rilClientToken = Nothing
    , _rilUpdateDate = Nothing
    , _rilCreateDate = Nothing
    , _rilPriceSchedules = Nothing
    , _rilStatusMessage = Nothing
    , _rilReservedInstancesId = Nothing
    , _rilTags = Nothing
    , _rilInstanceCounts = Nothing
    , _rilReservedInstancesListingId = Nothing
    }


-- | The status of the Reserved Instance listing.
rilStatus :: Lens' ReservedInstancesListing (Maybe ListingStatus)
rilStatus = lens _rilStatus (\ s a -> s{_rilStatus = a})

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
rilClientToken :: Lens' ReservedInstancesListing (Maybe Text)
rilClientToken = lens _rilClientToken (\ s a -> s{_rilClientToken = a})

-- | The last modified timestamp of the listing.
rilUpdateDate :: Lens' ReservedInstancesListing (Maybe UTCTime)
rilUpdateDate = lens _rilUpdateDate (\ s a -> s{_rilUpdateDate = a}) . mapping _Time

-- | The time the listing was created.
rilCreateDate :: Lens' ReservedInstancesListing (Maybe UTCTime)
rilCreateDate = lens _rilCreateDate (\ s a -> s{_rilCreateDate = a}) . mapping _Time

-- | The price of the Reserved Instance listing.
rilPriceSchedules :: Lens' ReservedInstancesListing [PriceSchedule]
rilPriceSchedules = lens _rilPriceSchedules (\ s a -> s{_rilPriceSchedules = a}) . _Default . _Coerce

-- | The reason for the current status of the Reserved Instance listing. The response can be blank.
rilStatusMessage :: Lens' ReservedInstancesListing (Maybe Text)
rilStatusMessage = lens _rilStatusMessage (\ s a -> s{_rilStatusMessage = a})

-- | The ID of the Reserved Instance.
rilReservedInstancesId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesId = lens _rilReservedInstancesId (\ s a -> s{_rilReservedInstancesId = a})

-- | Any tags assigned to the resource.
rilTags :: Lens' ReservedInstancesListing [Tag]
rilTags = lens _rilTags (\ s a -> s{_rilTags = a}) . _Default . _Coerce

-- | The number of instances in this state.
rilInstanceCounts :: Lens' ReservedInstancesListing [InstanceCount]
rilInstanceCounts = lens _rilInstanceCounts (\ s a -> s{_rilInstanceCounts = a}) . _Default . _Coerce

-- | The ID of the Reserved Instance listing.
rilReservedInstancesListingId :: Lens' ReservedInstancesListing (Maybe Text)
rilReservedInstancesListingId = lens _rilReservedInstancesListingId (\ s a -> s{_rilReservedInstancesListingId = a})

instance FromXML ReservedInstancesListing where
        parseXML x
          = ReservedInstancesListing' <$>
              (x .@? "status") <*> (x .@? "clientToken") <*>
                (x .@? "updateDate")
                <*> (x .@? "createDate")
                <*>
                (x .@? "priceSchedules" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "statusMessage")
                <*> (x .@? "reservedInstancesId")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "instanceCounts" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "reservedInstancesListingId")

instance NFData ReservedInstancesListing where

-- | Describes a Reserved Instance modification.
--
--
--
-- /See:/ 'reservedInstancesModification' smart constructor.
data ReservedInstancesModification = ReservedInstancesModification'
  { _rimModificationResults :: !(Maybe [ReservedInstancesModificationResult])
  , _rimStatus :: !(Maybe Text)
  , _rimClientToken :: !(Maybe Text)
  , _rimUpdateDate :: !(Maybe ISO8601)
  , _rimCreateDate :: !(Maybe ISO8601)
  , _rimEffectiveDate :: !(Maybe ISO8601)
  , _rimStatusMessage :: !(Maybe Text)
  , _rimReservedInstancesModificationId :: !(Maybe Text)
  , _rimReservedInstancesIds :: !(Maybe [ReservedInstancesId])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstancesModification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rimModificationResults' - Contains target configurations along with their corresponding new Reserved Instance IDs.
--
-- * 'rimStatus' - The status of the Reserved Instances modification request.
--
-- * 'rimClientToken' - A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'rimUpdateDate' - The time when the modification request was last updated.
--
-- * 'rimCreateDate' - The time when the modification request was created.
--
-- * 'rimEffectiveDate' - The time for the modification to become effective.
--
-- * 'rimStatusMessage' - The reason for the status.
--
-- * 'rimReservedInstancesModificationId' - A unique ID for the Reserved Instance modification.
--
-- * 'rimReservedInstancesIds' - The IDs of one or more Reserved Instances.
reservedInstancesModification
    :: ReservedInstancesModification
reservedInstancesModification =
  ReservedInstancesModification'
    { _rimModificationResults = Nothing
    , _rimStatus = Nothing
    , _rimClientToken = Nothing
    , _rimUpdateDate = Nothing
    , _rimCreateDate = Nothing
    , _rimEffectiveDate = Nothing
    , _rimStatusMessage = Nothing
    , _rimReservedInstancesModificationId = Nothing
    , _rimReservedInstancesIds = Nothing
    }


-- | Contains target configurations along with their corresponding new Reserved Instance IDs.
rimModificationResults :: Lens' ReservedInstancesModification [ReservedInstancesModificationResult]
rimModificationResults = lens _rimModificationResults (\ s a -> s{_rimModificationResults = a}) . _Default . _Coerce

-- | The status of the Reserved Instances modification request.
rimStatus :: Lens' ReservedInstancesModification (Maybe Text)
rimStatus = lens _rimStatus (\ s a -> s{_rimStatus = a})

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
rimClientToken :: Lens' ReservedInstancesModification (Maybe Text)
rimClientToken = lens _rimClientToken (\ s a -> s{_rimClientToken = a})

-- | The time when the modification request was last updated.
rimUpdateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimUpdateDate = lens _rimUpdateDate (\ s a -> s{_rimUpdateDate = a}) . mapping _Time

-- | The time when the modification request was created.
rimCreateDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimCreateDate = lens _rimCreateDate (\ s a -> s{_rimCreateDate = a}) . mapping _Time

-- | The time for the modification to become effective.
rimEffectiveDate :: Lens' ReservedInstancesModification (Maybe UTCTime)
rimEffectiveDate = lens _rimEffectiveDate (\ s a -> s{_rimEffectiveDate = a}) . mapping _Time

-- | The reason for the status.
rimStatusMessage :: Lens' ReservedInstancesModification (Maybe Text)
rimStatusMessage = lens _rimStatusMessage (\ s a -> s{_rimStatusMessage = a})

-- | A unique ID for the Reserved Instance modification.
rimReservedInstancesModificationId :: Lens' ReservedInstancesModification (Maybe Text)
rimReservedInstancesModificationId = lens _rimReservedInstancesModificationId (\ s a -> s{_rimReservedInstancesModificationId = a})

-- | The IDs of one or more Reserved Instances.
rimReservedInstancesIds :: Lens' ReservedInstancesModification [ReservedInstancesId]
rimReservedInstancesIds = lens _rimReservedInstancesIds (\ s a -> s{_rimReservedInstancesIds = a}) . _Default . _Coerce

instance FromXML ReservedInstancesModification where
        parseXML x
          = ReservedInstancesModification' <$>
              (x .@? "modificationResultSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "status")
                <*> (x .@? "clientToken")
                <*> (x .@? "updateDate")
                <*> (x .@? "createDate")
                <*> (x .@? "effectiveDate")
                <*> (x .@? "statusMessage")
                <*> (x .@? "reservedInstancesModificationId")
                <*>
                (x .@? "reservedInstancesSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData ReservedInstancesModification where

-- | Describes the modification request/s.
--
--
--
-- /See:/ 'reservedInstancesModificationResult' smart constructor.
data ReservedInstancesModificationResult = ReservedInstancesModificationResult'
  { _rimrReservedInstancesId :: !(Maybe Text)
  , _rimrTargetConfiguration :: !(Maybe ReservedInstancesConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstancesModificationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rimrReservedInstancesId' - The ID for the Reserved Instances that were created as part of the modification request. This field is only available when the modification is fulfilled.
--
-- * 'rimrTargetConfiguration' - The target Reserved Instances configurations supplied as part of the modification request.
reservedInstancesModificationResult
    :: ReservedInstancesModificationResult
reservedInstancesModificationResult =
  ReservedInstancesModificationResult'
    {_rimrReservedInstancesId = Nothing, _rimrTargetConfiguration = Nothing}


-- | The ID for the Reserved Instances that were created as part of the modification request. This field is only available when the modification is fulfilled.
rimrReservedInstancesId :: Lens' ReservedInstancesModificationResult (Maybe Text)
rimrReservedInstancesId = lens _rimrReservedInstancesId (\ s a -> s{_rimrReservedInstancesId = a})

-- | The target Reserved Instances configurations supplied as part of the modification request.
rimrTargetConfiguration :: Lens' ReservedInstancesModificationResult (Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration = lens _rimrTargetConfiguration (\ s a -> s{_rimrTargetConfiguration = a})

instance FromXML ReservedInstancesModificationResult
         where
        parseXML x
          = ReservedInstancesModificationResult' <$>
              (x .@? "reservedInstancesId") <*>
                (x .@? "targetConfiguration")

instance NFData ReservedInstancesModificationResult
         where

-- | Describes a Reserved Instance offering.
--
--
--
-- /See:/ 'reservedInstancesOffering' smart constructor.
data ReservedInstancesOffering = ReservedInstancesOffering'
  { _rioMarketplace                 :: !(Maybe Bool)
  , _rioCurrencyCode                :: !(Maybe CurrencyCodeValues)
  , _rioProductDescription          :: !(Maybe RIProductDescription)
  , _rioInstanceType                :: !(Maybe InstanceType)
  , _rioAvailabilityZone            :: !(Maybe Text)
  , _rioPricingDetails              :: !(Maybe [PricingDetail])
  , _rioScope                       :: !(Maybe Scope)
  , _rioRecurringCharges            :: !(Maybe [RecurringCharge])
  , _rioOfferingType                :: !(Maybe OfferingTypeValues)
  , _rioUsagePrice                  :: !(Maybe Double)
  , _rioFixedPrice                  :: !(Maybe Double)
  , _rioInstanceTenancy             :: !(Maybe Tenancy)
  , _rioReservedInstancesOfferingId :: !(Maybe Text)
  , _rioOfferingClass               :: !(Maybe OfferingClassType)
  , _rioDuration                    :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedInstancesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rioMarketplace' - Indicates whether the offering is available through the Reserved Instance Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace offering, this is @true@ .
--
-- * 'rioCurrencyCode' - The currency of the Reserved Instance offering you are purchasing. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
--
-- * 'rioProductDescription' - The Reserved Instance product platform description.
--
-- * 'rioInstanceType' - The instance type on which the Reserved Instance can be used.
--
-- * 'rioAvailabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- * 'rioPricingDetails' - The pricing details of the Reserved Instance offering.
--
-- * 'rioScope' - Whether the Reserved Instance is applied to instances in a region or an Availability Zone.
--
-- * 'rioRecurringCharges' - The recurring charge tag assigned to the resource.
--
-- * 'rioOfferingType' - The Reserved Instance offering type.
--
-- * 'rioUsagePrice' - The usage price of the Reserved Instance, per hour.
--
-- * 'rioFixedPrice' - The purchase price of the Reserved Instance.
--
-- * 'rioInstanceTenancy' - The tenancy of the instance.
--
-- * 'rioReservedInstancesOfferingId' - The ID of the Reserved Instance offering. This is the offering ID used in 'GetReservedInstancesExchangeQuote' to confirm that an exchange can be made.
--
-- * 'rioOfferingClass' - If @convertible@ it can be exchanged for Reserved Instances of the same or higher monetary value, with different configurations. If @standard@ , it is not possible to perform an exchange.
--
-- * 'rioDuration' - The duration of the Reserved Instance, in seconds.
reservedInstancesOffering
    :: ReservedInstancesOffering
reservedInstancesOffering =
  ReservedInstancesOffering'
    { _rioMarketplace = Nothing
    , _rioCurrencyCode = Nothing
    , _rioProductDescription = Nothing
    , _rioInstanceType = Nothing
    , _rioAvailabilityZone = Nothing
    , _rioPricingDetails = Nothing
    , _rioScope = Nothing
    , _rioRecurringCharges = Nothing
    , _rioOfferingType = Nothing
    , _rioUsagePrice = Nothing
    , _rioFixedPrice = Nothing
    , _rioInstanceTenancy = Nothing
    , _rioReservedInstancesOfferingId = Nothing
    , _rioOfferingClass = Nothing
    , _rioDuration = Nothing
    }


-- | Indicates whether the offering is available through the Reserved Instance Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace offering, this is @true@ .
rioMarketplace :: Lens' ReservedInstancesOffering (Maybe Bool)
rioMarketplace = lens _rioMarketplace (\ s a -> s{_rioMarketplace = a})

-- | The currency of the Reserved Instance offering you are purchasing. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
rioCurrencyCode :: Lens' ReservedInstancesOffering (Maybe CurrencyCodeValues)
rioCurrencyCode = lens _rioCurrencyCode (\ s a -> s{_rioCurrencyCode = a})

-- | The Reserved Instance product platform description.
rioProductDescription :: Lens' ReservedInstancesOffering (Maybe RIProductDescription)
rioProductDescription = lens _rioProductDescription (\ s a -> s{_rioProductDescription = a})

-- | The instance type on which the Reserved Instance can be used.
rioInstanceType :: Lens' ReservedInstancesOffering (Maybe InstanceType)
rioInstanceType = lens _rioInstanceType (\ s a -> s{_rioInstanceType = a})

-- | The Availability Zone in which the Reserved Instance can be used.
rioAvailabilityZone :: Lens' ReservedInstancesOffering (Maybe Text)
rioAvailabilityZone = lens _rioAvailabilityZone (\ s a -> s{_rioAvailabilityZone = a})

-- | The pricing details of the Reserved Instance offering.
rioPricingDetails :: Lens' ReservedInstancesOffering [PricingDetail]
rioPricingDetails = lens _rioPricingDetails (\ s a -> s{_rioPricingDetails = a}) . _Default . _Coerce

-- | Whether the Reserved Instance is applied to instances in a region or an Availability Zone.
rioScope :: Lens' ReservedInstancesOffering (Maybe Scope)
rioScope = lens _rioScope (\ s a -> s{_rioScope = a})

-- | The recurring charge tag assigned to the resource.
rioRecurringCharges :: Lens' ReservedInstancesOffering [RecurringCharge]
rioRecurringCharges = lens _rioRecurringCharges (\ s a -> s{_rioRecurringCharges = a}) . _Default . _Coerce

-- | The Reserved Instance offering type.
rioOfferingType :: Lens' ReservedInstancesOffering (Maybe OfferingTypeValues)
rioOfferingType = lens _rioOfferingType (\ s a -> s{_rioOfferingType = a})

-- | The usage price of the Reserved Instance, per hour.
rioUsagePrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioUsagePrice = lens _rioUsagePrice (\ s a -> s{_rioUsagePrice = a})

-- | The purchase price of the Reserved Instance.
rioFixedPrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioFixedPrice = lens _rioFixedPrice (\ s a -> s{_rioFixedPrice = a})

-- | The tenancy of the instance.
rioInstanceTenancy :: Lens' ReservedInstancesOffering (Maybe Tenancy)
rioInstanceTenancy = lens _rioInstanceTenancy (\ s a -> s{_rioInstanceTenancy = a})

-- | The ID of the Reserved Instance offering. This is the offering ID used in 'GetReservedInstancesExchangeQuote' to confirm that an exchange can be made.
rioReservedInstancesOfferingId :: Lens' ReservedInstancesOffering (Maybe Text)
rioReservedInstancesOfferingId = lens _rioReservedInstancesOfferingId (\ s a -> s{_rioReservedInstancesOfferingId = a})

-- | If @convertible@ it can be exchanged for Reserved Instances of the same or higher monetary value, with different configurations. If @standard@ , it is not possible to perform an exchange.
rioOfferingClass :: Lens' ReservedInstancesOffering (Maybe OfferingClassType)
rioOfferingClass = lens _rioOfferingClass (\ s a -> s{_rioOfferingClass = a})

-- | The duration of the Reserved Instance, in seconds.
rioDuration :: Lens' ReservedInstancesOffering (Maybe Integer)
rioDuration = lens _rioDuration (\ s a -> s{_rioDuration = a})

instance FromXML ReservedInstancesOffering where
        parseXML x
          = ReservedInstancesOffering' <$>
              (x .@? "marketplace") <*> (x .@? "currencyCode") <*>
                (x .@? "productDescription")
                <*> (x .@? "instanceType")
                <*> (x .@? "availabilityZone")
                <*>
                (x .@? "pricingDetailsSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "scope")
                <*>
                (x .@? "recurringCharges" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "offeringType")
                <*> (x .@? "usagePrice")
                <*> (x .@? "fixedPrice")
                <*> (x .@? "instanceTenancy")
                <*> (x .@? "reservedInstancesOfferingId")
                <*> (x .@? "offeringClass")
                <*> (x .@? "duration")

instance NFData ReservedInstancesOffering where

-- | Describes the error that's returned when you cannot delete a launch template version.
--
--
--
-- /See:/ 'responseError' smart constructor.
data ResponseError = ResponseError'
  { _reCode    :: !(Maybe LaunchTemplateErrorCode)
  , _reMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResponseError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reCode' - The error code.
--
-- * 'reMessage' - The error message, if applicable.
responseError
    :: ResponseError
responseError = ResponseError' {_reCode = Nothing, _reMessage = Nothing}


-- | The error code.
reCode :: Lens' ResponseError (Maybe LaunchTemplateErrorCode)
reCode = lens _reCode (\ s a -> s{_reCode = a})

-- | The error message, if applicable.
reMessage :: Lens' ResponseError (Maybe Text)
reMessage = lens _reMessage (\ s a -> s{_reMessage = a})

instance FromXML ResponseError where
        parseXML x
          = ResponseError' <$>
              (x .@? "code") <*> (x .@? "message")

instance NFData ResponseError where

-- | The information for a launch template.
--
--
--
-- /See:/ 'responseLaunchTemplateData' smart constructor.
data ResponseLaunchTemplateData = ResponseLaunchTemplateData'
  { _rSecurityGroupIds :: !(Maybe [Text])
  , _rSecurityGroups :: !(Maybe [Text])
  , _rInstanceMarketOptions :: !(Maybe LaunchTemplateInstanceMarketOptions)
  , _rDisableAPITermination :: !(Maybe Bool)
  , _rKeyName :: !(Maybe Text)
  , _rNetworkInterfaces :: !(Maybe [LaunchTemplateInstanceNetworkInterfaceSpecification])
  , _rRamDiskId :: !(Maybe Text)
  , _rKernelId :: !(Maybe Text)
  , _rElasticGpuSpecifications :: !(Maybe [ElasticGpuSpecificationResponse])
  , _rInstanceType :: !(Maybe InstanceType)
  , _rEBSOptimized :: !(Maybe Bool)
  , _rUserData :: !(Maybe Text)
  , _rMonitoring :: !(Maybe LaunchTemplatesMonitoring)
  , _rTagSpecifications :: !(Maybe [LaunchTemplateTagSpecification])
  , _rIAMInstanceProfile :: !(Maybe LaunchTemplateIAMInstanceProfileSpecification)
  , _rImageId :: !(Maybe Text)
  , _rInstanceInitiatedShutdownBehavior :: !(Maybe ShutdownBehavior)
  , _rCreditSpecification :: !(Maybe CreditSpecification)
  , _rBlockDeviceMappings :: !(Maybe [LaunchTemplateBlockDeviceMapping])
  , _rPlacement :: !(Maybe LaunchTemplatePlacement)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResponseLaunchTemplateData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rSecurityGroupIds' - The security group IDs.
--
-- * 'rSecurityGroups' - The security group names.
--
-- * 'rInstanceMarketOptions' - The market (purchasing) option for the instances.
--
-- * 'rDisableAPITermination' - If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
--
-- * 'rKeyName' - The name of the key pair.
--
-- * 'rNetworkInterfaces' - The network interfaces.
--
-- * 'rRamDiskId' - The ID of the RAM disk, if applicable.
--
-- * 'rKernelId' - The ID of the kernel, if applicable.
--
-- * 'rElasticGpuSpecifications' - The elastic GPU specification.
--
-- * 'rInstanceType' - The instance type.
--
-- * 'rEBSOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O.
--
-- * 'rUserData' - The user data for the instance.
--
-- * 'rMonitoring' - The monitoring for the instance.
--
-- * 'rTagSpecifications' - The tags.
--
-- * 'rIAMInstanceProfile' - The IAM instance profile.
--
-- * 'rImageId' - The ID of the AMI that was used to launch the instance.
--
-- * 'rInstanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- * 'rCreditSpecification' - The credit option for CPU usage of the instance.
--
-- * 'rBlockDeviceMappings' - The block device mappings.
--
-- * 'rPlacement' - The placement of the instance.
responseLaunchTemplateData
    :: ResponseLaunchTemplateData
responseLaunchTemplateData =
  ResponseLaunchTemplateData'
    { _rSecurityGroupIds = Nothing
    , _rSecurityGroups = Nothing
    , _rInstanceMarketOptions = Nothing
    , _rDisableAPITermination = Nothing
    , _rKeyName = Nothing
    , _rNetworkInterfaces = Nothing
    , _rRamDiskId = Nothing
    , _rKernelId = Nothing
    , _rElasticGpuSpecifications = Nothing
    , _rInstanceType = Nothing
    , _rEBSOptimized = Nothing
    , _rUserData = Nothing
    , _rMonitoring = Nothing
    , _rTagSpecifications = Nothing
    , _rIAMInstanceProfile = Nothing
    , _rImageId = Nothing
    , _rInstanceInitiatedShutdownBehavior = Nothing
    , _rCreditSpecification = Nothing
    , _rBlockDeviceMappings = Nothing
    , _rPlacement = Nothing
    }


-- | The security group IDs.
rSecurityGroupIds :: Lens' ResponseLaunchTemplateData [Text]
rSecurityGroupIds = lens _rSecurityGroupIds (\ s a -> s{_rSecurityGroupIds = a}) . _Default . _Coerce

-- | The security group names.
rSecurityGroups :: Lens' ResponseLaunchTemplateData [Text]
rSecurityGroups = lens _rSecurityGroups (\ s a -> s{_rSecurityGroups = a}) . _Default . _Coerce

-- | The market (purchasing) option for the instances.
rInstanceMarketOptions :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateInstanceMarketOptions)
rInstanceMarketOptions = lens _rInstanceMarketOptions (\ s a -> s{_rInstanceMarketOptions = a})

-- | If set to @true@ , indicates that the instance cannot be terminated using the Amazon EC2 console, command line tool, or API.
rDisableAPITermination :: Lens' ResponseLaunchTemplateData (Maybe Bool)
rDisableAPITermination = lens _rDisableAPITermination (\ s a -> s{_rDisableAPITermination = a})

-- | The name of the key pair.
rKeyName :: Lens' ResponseLaunchTemplateData (Maybe Text)
rKeyName = lens _rKeyName (\ s a -> s{_rKeyName = a})

-- | The network interfaces.
rNetworkInterfaces :: Lens' ResponseLaunchTemplateData [LaunchTemplateInstanceNetworkInterfaceSpecification]
rNetworkInterfaces = lens _rNetworkInterfaces (\ s a -> s{_rNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk, if applicable.
rRamDiskId :: Lens' ResponseLaunchTemplateData (Maybe Text)
rRamDiskId = lens _rRamDiskId (\ s a -> s{_rRamDiskId = a})

-- | The ID of the kernel, if applicable.
rKernelId :: Lens' ResponseLaunchTemplateData (Maybe Text)
rKernelId = lens _rKernelId (\ s a -> s{_rKernelId = a})

-- | The elastic GPU specification.
rElasticGpuSpecifications :: Lens' ResponseLaunchTemplateData [ElasticGpuSpecificationResponse]
rElasticGpuSpecifications = lens _rElasticGpuSpecifications (\ s a -> s{_rElasticGpuSpecifications = a}) . _Default . _Coerce

-- | The instance type.
rInstanceType :: Lens' ResponseLaunchTemplateData (Maybe InstanceType)
rInstanceType = lens _rInstanceType (\ s a -> s{_rInstanceType = a})

-- | Indicates whether the instance is optimized for Amazon EBS I/O.
rEBSOptimized :: Lens' ResponseLaunchTemplateData (Maybe Bool)
rEBSOptimized = lens _rEBSOptimized (\ s a -> s{_rEBSOptimized = a})

-- | The user data for the instance.
rUserData :: Lens' ResponseLaunchTemplateData (Maybe Text)
rUserData = lens _rUserData (\ s a -> s{_rUserData = a})

-- | The monitoring for the instance.
rMonitoring :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplatesMonitoring)
rMonitoring = lens _rMonitoring (\ s a -> s{_rMonitoring = a})

-- | The tags.
rTagSpecifications :: Lens' ResponseLaunchTemplateData [LaunchTemplateTagSpecification]
rTagSpecifications = lens _rTagSpecifications (\ s a -> s{_rTagSpecifications = a}) . _Default . _Coerce

-- | The IAM instance profile.
rIAMInstanceProfile :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplateIAMInstanceProfileSpecification)
rIAMInstanceProfile = lens _rIAMInstanceProfile (\ s a -> s{_rIAMInstanceProfile = a})

-- | The ID of the AMI that was used to launch the instance.
rImageId :: Lens' ResponseLaunchTemplateData (Maybe Text)
rImageId = lens _rImageId (\ s a -> s{_rImageId = a})

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
rInstanceInitiatedShutdownBehavior :: Lens' ResponseLaunchTemplateData (Maybe ShutdownBehavior)
rInstanceInitiatedShutdownBehavior = lens _rInstanceInitiatedShutdownBehavior (\ s a -> s{_rInstanceInitiatedShutdownBehavior = a})

-- | The credit option for CPU usage of the instance.
rCreditSpecification :: Lens' ResponseLaunchTemplateData (Maybe CreditSpecification)
rCreditSpecification = lens _rCreditSpecification (\ s a -> s{_rCreditSpecification = a})

-- | The block device mappings.
rBlockDeviceMappings :: Lens' ResponseLaunchTemplateData [LaunchTemplateBlockDeviceMapping]
rBlockDeviceMappings = lens _rBlockDeviceMappings (\ s a -> s{_rBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement of the instance.
rPlacement :: Lens' ResponseLaunchTemplateData (Maybe LaunchTemplatePlacement)
rPlacement = lens _rPlacement (\ s a -> s{_rPlacement = a})

instance FromXML ResponseLaunchTemplateData where
        parseXML x
          = ResponseLaunchTemplateData' <$>
              (x .@? "securityGroupIdSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*>
                (x .@? "securityGroupSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "instanceMarketOptions")
                <*> (x .@? "disableApiTermination")
                <*> (x .@? "keyName")
                <*>
                (x .@? "networkInterfaceSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "ramDiskId")
                <*> (x .@? "kernelId")
                <*>
                (x .@? "elasticGpuSpecificationSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "instanceType")
                <*> (x .@? "ebsOptimized")
                <*> (x .@? "userData")
                <*> (x .@? "monitoring")
                <*>
                (x .@? "tagSpecificationSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "iamInstanceProfile")
                <*> (x .@? "imageId")
                <*> (x .@? "instanceInitiatedShutdownBehavior")
                <*> (x .@? "creditSpecification")
                <*>
                (x .@? "blockDeviceMappingSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "placement")

instance NFData ResponseLaunchTemplateData where

-- | Describes a route in a route table.
--
--
--
-- /See:/ 'route' smart constructor.
data Route = Route'
  { _rVPCPeeringConnectionId      :: !(Maybe Text)
  , _rInstanceId                  :: !(Maybe Text)
  , _rOrigin                      :: !(Maybe RouteOrigin)
  , _rState                       :: !(Maybe RouteState)
  , _rEgressOnlyInternetGatewayId :: !(Maybe Text)
  , _rDestinationIPv6CidrBlock    :: !(Maybe Text)
  , _rNatGatewayId                :: !(Maybe Text)
  , _rNetworkInterfaceId          :: !(Maybe Text)
  , _rGatewayId                   :: !(Maybe Text)
  , _rInstanceOwnerId             :: !(Maybe Text)
  , _rDestinationPrefixListId     :: !(Maybe Text)
  , _rDestinationCidrBlock        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Route' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rVPCPeeringConnectionId' - The ID of the VPC peering connection.
--
-- * 'rInstanceId' - The ID of a NAT instance in your VPC.
--
-- * 'rOrigin' - Describes how the route was created.     * @CreateRouteTable@ - The route was automatically created when the route table was created.     * @CreateRoute@ - The route was manually added to the route table.     * @EnableVgwRoutePropagation@ - The route was propagated by route propagation.
--
-- * 'rState' - The state of the route. The @blackhole@ state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, or the specified NAT instance has been terminated).
--
-- * 'rEgressOnlyInternetGatewayId' - The ID of the egress-only Internet gateway.
--
-- * 'rDestinationIPv6CidrBlock' - The IPv6 CIDR block used for the destination match.
--
-- * 'rNatGatewayId' - The ID of a NAT gateway.
--
-- * 'rNetworkInterfaceId' - The ID of the network interface.
--
-- * 'rGatewayId' - The ID of a gateway attached to your VPC.
--
-- * 'rInstanceOwnerId' - The AWS account ID of the owner of the instance.
--
-- * 'rDestinationPrefixListId' - The prefix of the AWS service.
--
-- * 'rDestinationCidrBlock' - The IPv4 CIDR block used for the destination match.
route
    :: Route
route =
  Route'
    { _rVPCPeeringConnectionId = Nothing
    , _rInstanceId = Nothing
    , _rOrigin = Nothing
    , _rState = Nothing
    , _rEgressOnlyInternetGatewayId = Nothing
    , _rDestinationIPv6CidrBlock = Nothing
    , _rNatGatewayId = Nothing
    , _rNetworkInterfaceId = Nothing
    , _rGatewayId = Nothing
    , _rInstanceOwnerId = Nothing
    , _rDestinationPrefixListId = Nothing
    , _rDestinationCidrBlock = Nothing
    }


-- | The ID of the VPC peering connection.
rVPCPeeringConnectionId :: Lens' Route (Maybe Text)
rVPCPeeringConnectionId = lens _rVPCPeeringConnectionId (\ s a -> s{_rVPCPeeringConnectionId = a})

-- | The ID of a NAT instance in your VPC.
rInstanceId :: Lens' Route (Maybe Text)
rInstanceId = lens _rInstanceId (\ s a -> s{_rInstanceId = a})

-- | Describes how the route was created.     * @CreateRouteTable@ - The route was automatically created when the route table was created.     * @CreateRoute@ - The route was manually added to the route table.     * @EnableVgwRoutePropagation@ - The route was propagated by route propagation.
rOrigin :: Lens' Route (Maybe RouteOrigin)
rOrigin = lens _rOrigin (\ s a -> s{_rOrigin = a})

-- | The state of the route. The @blackhole@ state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, or the specified NAT instance has been terminated).
rState :: Lens' Route (Maybe RouteState)
rState = lens _rState (\ s a -> s{_rState = a})

-- | The ID of the egress-only Internet gateway.
rEgressOnlyInternetGatewayId :: Lens' Route (Maybe Text)
rEgressOnlyInternetGatewayId = lens _rEgressOnlyInternetGatewayId (\ s a -> s{_rEgressOnlyInternetGatewayId = a})

-- | The IPv6 CIDR block used for the destination match.
rDestinationIPv6CidrBlock :: Lens' Route (Maybe Text)
rDestinationIPv6CidrBlock = lens _rDestinationIPv6CidrBlock (\ s a -> s{_rDestinationIPv6CidrBlock = a})

-- | The ID of a NAT gateway.
rNatGatewayId :: Lens' Route (Maybe Text)
rNatGatewayId = lens _rNatGatewayId (\ s a -> s{_rNatGatewayId = a})

-- | The ID of the network interface.
rNetworkInterfaceId :: Lens' Route (Maybe Text)
rNetworkInterfaceId = lens _rNetworkInterfaceId (\ s a -> s{_rNetworkInterfaceId = a})

-- | The ID of a gateway attached to your VPC.
rGatewayId :: Lens' Route (Maybe Text)
rGatewayId = lens _rGatewayId (\ s a -> s{_rGatewayId = a})

-- | The AWS account ID of the owner of the instance.
rInstanceOwnerId :: Lens' Route (Maybe Text)
rInstanceOwnerId = lens _rInstanceOwnerId (\ s a -> s{_rInstanceOwnerId = a})

-- | The prefix of the AWS service.
rDestinationPrefixListId :: Lens' Route (Maybe Text)
rDestinationPrefixListId = lens _rDestinationPrefixListId (\ s a -> s{_rDestinationPrefixListId = a})

-- | The IPv4 CIDR block used for the destination match.
rDestinationCidrBlock :: Lens' Route (Maybe Text)
rDestinationCidrBlock = lens _rDestinationCidrBlock (\ s a -> s{_rDestinationCidrBlock = a})

instance FromXML Route where
        parseXML x
          = Route' <$>
              (x .@? "vpcPeeringConnectionId") <*>
                (x .@? "instanceId")
                <*> (x .@? "origin")
                <*> (x .@? "state")
                <*> (x .@? "egressOnlyInternetGatewayId")
                <*> (x .@? "destinationIpv6CidrBlock")
                <*> (x .@? "natGatewayId")
                <*> (x .@? "networkInterfaceId")
                <*> (x .@? "gatewayId")
                <*> (x .@? "instanceOwnerId")
                <*> (x .@? "destinationPrefixListId")
                <*> (x .@? "destinationCidrBlock")

instance NFData Route where

-- | Describes a route table.
--
--
--
-- /See:/ 'routeTable' smart constructor.
data RouteTable = RouteTable'
  { _rtRouteTableId    :: !(Maybe Text)
  , _rtRoutes          :: !(Maybe [Route])
  , _rtVPCId           :: !(Maybe Text)
  , _rtPropagatingVGWs :: !(Maybe [PropagatingVGW])
  , _rtAssociations    :: !(Maybe [RouteTableAssociation])
  , _rtTags            :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtRouteTableId' - The ID of the route table.
--
-- * 'rtRoutes' - The routes in the route table.
--
-- * 'rtVPCId' - The ID of the VPC.
--
-- * 'rtPropagatingVGWs' - Any virtual private gateway (VGW) propagating routes.
--
-- * 'rtAssociations' - The associations between the route table and one or more subnets.
--
-- * 'rtTags' - Any tags assigned to the route table.
routeTable
    :: RouteTable
routeTable =
  RouteTable'
    { _rtRouteTableId = Nothing
    , _rtRoutes = Nothing
    , _rtVPCId = Nothing
    , _rtPropagatingVGWs = Nothing
    , _rtAssociations = Nothing
    , _rtTags = Nothing
    }


-- | The ID of the route table.
rtRouteTableId :: Lens' RouteTable (Maybe Text)
rtRouteTableId = lens _rtRouteTableId (\ s a -> s{_rtRouteTableId = a})

-- | The routes in the route table.
rtRoutes :: Lens' RouteTable [Route]
rtRoutes = lens _rtRoutes (\ s a -> s{_rtRoutes = a}) . _Default . _Coerce

-- | The ID of the VPC.
rtVPCId :: Lens' RouteTable (Maybe Text)
rtVPCId = lens _rtVPCId (\ s a -> s{_rtVPCId = a})

-- | Any virtual private gateway (VGW) propagating routes.
rtPropagatingVGWs :: Lens' RouteTable [PropagatingVGW]
rtPropagatingVGWs = lens _rtPropagatingVGWs (\ s a -> s{_rtPropagatingVGWs = a}) . _Default . _Coerce

-- | The associations between the route table and one or more subnets.
rtAssociations :: Lens' RouteTable [RouteTableAssociation]
rtAssociations = lens _rtAssociations (\ s a -> s{_rtAssociations = a}) . _Default . _Coerce

-- | Any tags assigned to the route table.
rtTags :: Lens' RouteTable [Tag]
rtTags = lens _rtTags (\ s a -> s{_rtTags = a}) . _Default . _Coerce

instance FromXML RouteTable where
        parseXML x
          = RouteTable' <$>
              (x .@? "routeTableId") <*>
                (x .@? "routeSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "vpcId")
                <*>
                (x .@? "propagatingVgwSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "associationSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData RouteTable where

-- | Describes an association between a route table and a subnet.
--
--
--
-- /See:/ 'routeTableAssociation' smart constructor.
data RouteTableAssociation = RouteTableAssociation'
  { _rtaRouteTableId            :: !(Maybe Text)
  , _rtaRouteTableAssociationId :: !(Maybe Text)
  , _rtaMain                    :: !(Maybe Bool)
  , _rtaSubnetId                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RouteTableAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtaRouteTableId' - The ID of the route table.
--
-- * 'rtaRouteTableAssociationId' - The ID of the association between a route table and a subnet.
--
-- * 'rtaMain' - Indicates whether this is the main route table.
--
-- * 'rtaSubnetId' - The ID of the subnet. A subnet ID is not returned for an implicit association.
routeTableAssociation
    :: RouteTableAssociation
routeTableAssociation =
  RouteTableAssociation'
    { _rtaRouteTableId = Nothing
    , _rtaRouteTableAssociationId = Nothing
    , _rtaMain = Nothing
    , _rtaSubnetId = Nothing
    }


-- | The ID of the route table.
rtaRouteTableId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableId = lens _rtaRouteTableId (\ s a -> s{_rtaRouteTableId = a})

-- | The ID of the association between a route table and a subnet.
rtaRouteTableAssociationId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableAssociationId = lens _rtaRouteTableAssociationId (\ s a -> s{_rtaRouteTableAssociationId = a})

-- | Indicates whether this is the main route table.
rtaMain :: Lens' RouteTableAssociation (Maybe Bool)
rtaMain = lens _rtaMain (\ s a -> s{_rtaMain = a})

-- | The ID of the subnet. A subnet ID is not returned for an implicit association.
rtaSubnetId :: Lens' RouteTableAssociation (Maybe Text)
rtaSubnetId = lens _rtaSubnetId (\ s a -> s{_rtaSubnetId = a})

instance FromXML RouteTableAssociation where
        parseXML x
          = RouteTableAssociation' <$>
              (x .@? "routeTableId") <*>
                (x .@? "routeTableAssociationId")
                <*> (x .@? "main")
                <*> (x .@? "subnetId")

instance NFData RouteTableAssociation where

-- | Describes the monitoring of an instance.
--
--
--
-- /See:/ 'runInstancesMonitoringEnabled' smart constructor.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled'
  { _rimeEnabled :: Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunInstancesMonitoringEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rimeEnabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
runInstancesMonitoringEnabled
    :: Bool -- ^ 'rimeEnabled'
    -> RunInstancesMonitoringEnabled
runInstancesMonitoringEnabled pEnabled_ =
  RunInstancesMonitoringEnabled' {_rimeEnabled = pEnabled_}


-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
rimeEnabled :: Lens' RunInstancesMonitoringEnabled Bool
rimeEnabled = lens _rimeEnabled (\ s a -> s{_rimeEnabled = a})

instance FromXML RunInstancesMonitoringEnabled where
        parseXML x
          = RunInstancesMonitoringEnabled' <$> (x .@ "enabled")

instance NFData RunInstancesMonitoringEnabled where

instance ToQuery RunInstancesMonitoringEnabled where
        toQuery RunInstancesMonitoringEnabled'{..}
          = mconcat ["Enabled" =: _rimeEnabled]

-- | Describes the storage parameters for S3 and S3 buckets for an instance store-backed AMI.
--
--
--
-- /See:/ 's3Storage' smart constructor.
data S3Storage = S3Storage'
  { _ssPrefix                :: !(Maybe Text)
  , _ssUploadPolicy          :: !(Maybe Base64)
  , _ssBucket                :: !(Maybe Text)
  , _ssUploadPolicySignature :: !(Maybe Text)
  , _ssAWSAccessKeyId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Storage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssPrefix' - The beginning of the file name of the AMI.
--
-- * 'ssUploadPolicy' - An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'ssBucket' - The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
--
-- * 'ssUploadPolicySignature' - The signature of the JSON document.
--
-- * 'ssAWSAccessKeyId' - The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
s3Storage
    :: S3Storage
s3Storage =
  S3Storage'
    { _ssPrefix = Nothing
    , _ssUploadPolicy = Nothing
    , _ssBucket = Nothing
    , _ssUploadPolicySignature = Nothing
    , _ssAWSAccessKeyId = Nothing
    }


-- | The beginning of the file name of the AMI.
ssPrefix :: Lens' S3Storage (Maybe Text)
ssPrefix = lens _ssPrefix (\ s a -> s{_ssPrefix = a})

-- | An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
ssUploadPolicy :: Lens' S3Storage (Maybe ByteString)
ssUploadPolicy = lens _ssUploadPolicy (\ s a -> s{_ssUploadPolicy = a}) . mapping _Base64

-- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
ssBucket :: Lens' S3Storage (Maybe Text)
ssBucket = lens _ssBucket (\ s a -> s{_ssBucket = a})

-- | The signature of the JSON document.
ssUploadPolicySignature :: Lens' S3Storage (Maybe Text)
ssUploadPolicySignature = lens _ssUploadPolicySignature (\ s a -> s{_ssUploadPolicySignature = a})

-- | The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
ssAWSAccessKeyId :: Lens' S3Storage (Maybe Text)
ssAWSAccessKeyId = lens _ssAWSAccessKeyId (\ s a -> s{_ssAWSAccessKeyId = a})

instance FromXML S3Storage where
        parseXML x
          = S3Storage' <$>
              (x .@? "prefix") <*> (x .@? "uploadPolicy") <*>
                (x .@? "bucket")
                <*> (x .@? "uploadPolicySignature")
                <*> (x .@? "AWSAccessKeyId")

instance NFData S3Storage where

instance ToQuery S3Storage where
        toQuery S3Storage'{..}
          = mconcat
              ["Prefix" =: _ssPrefix,
               "UploadPolicy" =: _ssUploadPolicy,
               "Bucket" =: _ssBucket,
               "UploadPolicySignature" =: _ssUploadPolicySignature,
               "AWSAccessKeyId" =: _ssAWSAccessKeyId]

-- | Describes a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstance' smart constructor.
data ScheduledInstance = ScheduledInstance'
  { _siPreviousSlotEndTime         :: !(Maybe ISO8601)
  , _siPlatform                    :: !(Maybe Text)
  , _siTermStartDate               :: !(Maybe ISO8601)
  , _siInstanceCount               :: !(Maybe Int)
  , _siScheduledInstanceId         :: !(Maybe Text)
  , _siHourlyPrice                 :: !(Maybe Text)
  , _siCreateDate                  :: !(Maybe ISO8601)
  , _siSlotDurationInHours         :: !(Maybe Int)
  , _siTotalScheduledInstanceHours :: !(Maybe Int)
  , _siInstanceType                :: !(Maybe Text)
  , _siRecurrence                  :: !(Maybe ScheduledInstanceRecurrence)
  , _siAvailabilityZone            :: !(Maybe Text)
  , _siTermEndDate                 :: !(Maybe ISO8601)
  , _siNextSlotStartTime           :: !(Maybe ISO8601)
  , _siNetworkPlatform             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siPreviousSlotEndTime' - The time that the previous schedule ended or will end.
--
-- * 'siPlatform' - The platform (@Linux/UNIX@ or @Windows@ ).
--
-- * 'siTermStartDate' - The start date for the Scheduled Instance.
--
-- * 'siInstanceCount' - The number of instances.
--
-- * 'siScheduledInstanceId' - The Scheduled Instance ID.
--
-- * 'siHourlyPrice' - The hourly price for a single instance.
--
-- * 'siCreateDate' - The date when the Scheduled Instance was purchased.
--
-- * 'siSlotDurationInHours' - The number of hours in the schedule.
--
-- * 'siTotalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
--
-- * 'siInstanceType' - The instance type.
--
-- * 'siRecurrence' - The schedule recurrence.
--
-- * 'siAvailabilityZone' - The Availability Zone.
--
-- * 'siTermEndDate' - The end date for the Scheduled Instance.
--
-- * 'siNextSlotStartTime' - The time for the next schedule to start.
--
-- * 'siNetworkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
scheduledInstance
    :: ScheduledInstance
scheduledInstance =
  ScheduledInstance'
    { _siPreviousSlotEndTime = Nothing
    , _siPlatform = Nothing
    , _siTermStartDate = Nothing
    , _siInstanceCount = Nothing
    , _siScheduledInstanceId = Nothing
    , _siHourlyPrice = Nothing
    , _siCreateDate = Nothing
    , _siSlotDurationInHours = Nothing
    , _siTotalScheduledInstanceHours = Nothing
    , _siInstanceType = Nothing
    , _siRecurrence = Nothing
    , _siAvailabilityZone = Nothing
    , _siTermEndDate = Nothing
    , _siNextSlotStartTime = Nothing
    , _siNetworkPlatform = Nothing
    }


-- | The time that the previous schedule ended or will end.
siPreviousSlotEndTime :: Lens' ScheduledInstance (Maybe UTCTime)
siPreviousSlotEndTime = lens _siPreviousSlotEndTime (\ s a -> s{_siPreviousSlotEndTime = a}) . mapping _Time

-- | The platform (@Linux/UNIX@ or @Windows@ ).
siPlatform :: Lens' ScheduledInstance (Maybe Text)
siPlatform = lens _siPlatform (\ s a -> s{_siPlatform = a})

-- | The start date for the Scheduled Instance.
siTermStartDate :: Lens' ScheduledInstance (Maybe UTCTime)
siTermStartDate = lens _siTermStartDate (\ s a -> s{_siTermStartDate = a}) . mapping _Time

-- | The number of instances.
siInstanceCount :: Lens' ScheduledInstance (Maybe Int)
siInstanceCount = lens _siInstanceCount (\ s a -> s{_siInstanceCount = a})

-- | The Scheduled Instance ID.
siScheduledInstanceId :: Lens' ScheduledInstance (Maybe Text)
siScheduledInstanceId = lens _siScheduledInstanceId (\ s a -> s{_siScheduledInstanceId = a})

-- | The hourly price for a single instance.
siHourlyPrice :: Lens' ScheduledInstance (Maybe Text)
siHourlyPrice = lens _siHourlyPrice (\ s a -> s{_siHourlyPrice = a})

-- | The date when the Scheduled Instance was purchased.
siCreateDate :: Lens' ScheduledInstance (Maybe UTCTime)
siCreateDate = lens _siCreateDate (\ s a -> s{_siCreateDate = a}) . mapping _Time

-- | The number of hours in the schedule.
siSlotDurationInHours :: Lens' ScheduledInstance (Maybe Int)
siSlotDurationInHours = lens _siSlotDurationInHours (\ s a -> s{_siSlotDurationInHours = a})

-- | The total number of hours for a single instance for the entire term.
siTotalScheduledInstanceHours :: Lens' ScheduledInstance (Maybe Int)
siTotalScheduledInstanceHours = lens _siTotalScheduledInstanceHours (\ s a -> s{_siTotalScheduledInstanceHours = a})

-- | The instance type.
siInstanceType :: Lens' ScheduledInstance (Maybe Text)
siInstanceType = lens _siInstanceType (\ s a -> s{_siInstanceType = a})

-- | The schedule recurrence.
siRecurrence :: Lens' ScheduledInstance (Maybe ScheduledInstanceRecurrence)
siRecurrence = lens _siRecurrence (\ s a -> s{_siRecurrence = a})

-- | The Availability Zone.
siAvailabilityZone :: Lens' ScheduledInstance (Maybe Text)
siAvailabilityZone = lens _siAvailabilityZone (\ s a -> s{_siAvailabilityZone = a})

-- | The end date for the Scheduled Instance.
siTermEndDate :: Lens' ScheduledInstance (Maybe UTCTime)
siTermEndDate = lens _siTermEndDate (\ s a -> s{_siTermEndDate = a}) . mapping _Time

-- | The time for the next schedule to start.
siNextSlotStartTime :: Lens' ScheduledInstance (Maybe UTCTime)
siNextSlotStartTime = lens _siNextSlotStartTime (\ s a -> s{_siNextSlotStartTime = a}) . mapping _Time

-- | The network platform (@EC2-Classic@ or @EC2-VPC@ ).
siNetworkPlatform :: Lens' ScheduledInstance (Maybe Text)
siNetworkPlatform = lens _siNetworkPlatform (\ s a -> s{_siNetworkPlatform = a})

instance FromXML ScheduledInstance where
        parseXML x
          = ScheduledInstance' <$>
              (x .@? "previousSlotEndTime") <*> (x .@? "platform")
                <*> (x .@? "termStartDate")
                <*> (x .@? "instanceCount")
                <*> (x .@? "scheduledInstanceId")
                <*> (x .@? "hourlyPrice")
                <*> (x .@? "createDate")
                <*> (x .@? "slotDurationInHours")
                <*> (x .@? "totalScheduledInstanceHours")
                <*> (x .@? "instanceType")
                <*> (x .@? "recurrence")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "termEndDate")
                <*> (x .@? "nextSlotStartTime")
                <*> (x .@? "networkPlatform")

instance NFData ScheduledInstance where

-- | Describes a schedule that is available for your Scheduled Instances.
--
--
--
-- /See:/ 'scheduledInstanceAvailability' smart constructor.
data ScheduledInstanceAvailability = ScheduledInstanceAvailability'
  { _siaMaxTermDurationInDays       :: !(Maybe Int)
  , _siaPlatform                    :: !(Maybe Text)
  , _siaPurchaseToken               :: !(Maybe Text)
  , _siaHourlyPrice                 :: !(Maybe Text)
  , _siaAvailableInstanceCount      :: !(Maybe Int)
  , _siaSlotDurationInHours         :: !(Maybe Int)
  , _siaTotalScheduledInstanceHours :: !(Maybe Int)
  , _siaInstanceType                :: !(Maybe Text)
  , _siaRecurrence                  :: !(Maybe ScheduledInstanceRecurrence)
  , _siaAvailabilityZone            :: !(Maybe Text)
  , _siaMinTermDurationInDays       :: !(Maybe Int)
  , _siaFirstSlotStartTime          :: !(Maybe ISO8601)
  , _siaNetworkPlatform             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstanceAvailability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siaMaxTermDurationInDays' - The maximum term. The only possible value is 365 days.
--
-- * 'siaPlatform' - The platform (@Linux/UNIX@ or @Windows@ ).
--
-- * 'siaPurchaseToken' - The purchase token. This token expires in two hours.
--
-- * 'siaHourlyPrice' - The hourly price for a single instance.
--
-- * 'siaAvailableInstanceCount' - The number of available instances.
--
-- * 'siaSlotDurationInHours' - The number of hours in the schedule.
--
-- * 'siaTotalScheduledInstanceHours' - The total number of hours for a single instance for the entire term.
--
-- * 'siaInstanceType' - The instance type. You can specify one of the C3, C4, M4, or R3 instance types.
--
-- * 'siaRecurrence' - The schedule recurrence.
--
-- * 'siaAvailabilityZone' - The Availability Zone.
--
-- * 'siaMinTermDurationInDays' - The minimum term. The only possible value is 365 days.
--
-- * 'siaFirstSlotStartTime' - The time period for the first schedule to start.
--
-- * 'siaNetworkPlatform' - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
scheduledInstanceAvailability
    :: ScheduledInstanceAvailability
scheduledInstanceAvailability =
  ScheduledInstanceAvailability'
    { _siaMaxTermDurationInDays = Nothing
    , _siaPlatform = Nothing
    , _siaPurchaseToken = Nothing
    , _siaHourlyPrice = Nothing
    , _siaAvailableInstanceCount = Nothing
    , _siaSlotDurationInHours = Nothing
    , _siaTotalScheduledInstanceHours = Nothing
    , _siaInstanceType = Nothing
    , _siaRecurrence = Nothing
    , _siaAvailabilityZone = Nothing
    , _siaMinTermDurationInDays = Nothing
    , _siaFirstSlotStartTime = Nothing
    , _siaNetworkPlatform = Nothing
    }


-- | The maximum term. The only possible value is 365 days.
siaMaxTermDurationInDays :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaMaxTermDurationInDays = lens _siaMaxTermDurationInDays (\ s a -> s{_siaMaxTermDurationInDays = a})

-- | The platform (@Linux/UNIX@ or @Windows@ ).
siaPlatform :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaPlatform = lens _siaPlatform (\ s a -> s{_siaPlatform = a})

-- | The purchase token. This token expires in two hours.
siaPurchaseToken :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaPurchaseToken = lens _siaPurchaseToken (\ s a -> s{_siaPurchaseToken = a})

-- | The hourly price for a single instance.
siaHourlyPrice :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaHourlyPrice = lens _siaHourlyPrice (\ s a -> s{_siaHourlyPrice = a})

-- | The number of available instances.
siaAvailableInstanceCount :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaAvailableInstanceCount = lens _siaAvailableInstanceCount (\ s a -> s{_siaAvailableInstanceCount = a})

-- | The number of hours in the schedule.
siaSlotDurationInHours :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaSlotDurationInHours = lens _siaSlotDurationInHours (\ s a -> s{_siaSlotDurationInHours = a})

-- | The total number of hours for a single instance for the entire term.
siaTotalScheduledInstanceHours :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaTotalScheduledInstanceHours = lens _siaTotalScheduledInstanceHours (\ s a -> s{_siaTotalScheduledInstanceHours = a})

-- | The instance type. You can specify one of the C3, C4, M4, or R3 instance types.
siaInstanceType :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaInstanceType = lens _siaInstanceType (\ s a -> s{_siaInstanceType = a})

-- | The schedule recurrence.
siaRecurrence :: Lens' ScheduledInstanceAvailability (Maybe ScheduledInstanceRecurrence)
siaRecurrence = lens _siaRecurrence (\ s a -> s{_siaRecurrence = a})

-- | The Availability Zone.
siaAvailabilityZone :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaAvailabilityZone = lens _siaAvailabilityZone (\ s a -> s{_siaAvailabilityZone = a})

-- | The minimum term. The only possible value is 365 days.
siaMinTermDurationInDays :: Lens' ScheduledInstanceAvailability (Maybe Int)
siaMinTermDurationInDays = lens _siaMinTermDurationInDays (\ s a -> s{_siaMinTermDurationInDays = a})

-- | The time period for the first schedule to start.
siaFirstSlotStartTime :: Lens' ScheduledInstanceAvailability (Maybe UTCTime)
siaFirstSlotStartTime = lens _siaFirstSlotStartTime (\ s a -> s{_siaFirstSlotStartTime = a}) . mapping _Time

-- | The network platform (@EC2-Classic@ or @EC2-VPC@ ).
siaNetworkPlatform :: Lens' ScheduledInstanceAvailability (Maybe Text)
siaNetworkPlatform = lens _siaNetworkPlatform (\ s a -> s{_siaNetworkPlatform = a})

instance FromXML ScheduledInstanceAvailability where
        parseXML x
          = ScheduledInstanceAvailability' <$>
              (x .@? "maxTermDurationInDays") <*>
                (x .@? "platform")
                <*> (x .@? "purchaseToken")
                <*> (x .@? "hourlyPrice")
                <*> (x .@? "availableInstanceCount")
                <*> (x .@? "slotDurationInHours")
                <*> (x .@? "totalScheduledInstanceHours")
                <*> (x .@? "instanceType")
                <*> (x .@? "recurrence")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "minTermDurationInDays")
                <*> (x .@? "firstSlotStartTime")
                <*> (x .@? "networkPlatform")

instance NFData ScheduledInstanceAvailability where

-- | Describes the recurring schedule for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstanceRecurrence' smart constructor.
data ScheduledInstanceRecurrence = ScheduledInstanceRecurrence'
  { _sirFrequency               :: !(Maybe Text)
  , _sirOccurrenceRelativeToEnd :: !(Maybe Bool)
  , _sirOccurrenceUnit          :: !(Maybe Text)
  , _sirInterval                :: !(Maybe Int)
  , _sirOccurrenceDaySet        :: !(Maybe [Int])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstanceRecurrence' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirFrequency' - The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- * 'sirOccurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified week or month.
--
-- * 'sirOccurrenceUnit' - The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
--
-- * 'sirInterval' - The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
--
-- * 'sirOccurrenceDaySet' - The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
scheduledInstanceRecurrence
    :: ScheduledInstanceRecurrence
scheduledInstanceRecurrence =
  ScheduledInstanceRecurrence'
    { _sirFrequency = Nothing
    , _sirOccurrenceRelativeToEnd = Nothing
    , _sirOccurrenceUnit = Nothing
    , _sirInterval = Nothing
    , _sirOccurrenceDaySet = Nothing
    }


-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
sirFrequency :: Lens' ScheduledInstanceRecurrence (Maybe Text)
sirFrequency = lens _sirFrequency (\ s a -> s{_sirFrequency = a})

-- | Indicates whether the occurrence is relative to the end of the specified week or month.
sirOccurrenceRelativeToEnd :: Lens' ScheduledInstanceRecurrence (Maybe Bool)
sirOccurrenceRelativeToEnd = lens _sirOccurrenceRelativeToEnd (\ s a -> s{_sirOccurrenceRelativeToEnd = a})

-- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
sirOccurrenceUnit :: Lens' ScheduledInstanceRecurrence (Maybe Text)
sirOccurrenceUnit = lens _sirOccurrenceUnit (\ s a -> s{_sirOccurrenceUnit = a})

-- | The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
sirInterval :: Lens' ScheduledInstanceRecurrence (Maybe Int)
sirInterval = lens _sirInterval (\ s a -> s{_sirInterval = a})

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
sirOccurrenceDaySet :: Lens' ScheduledInstanceRecurrence [Int]
sirOccurrenceDaySet = lens _sirOccurrenceDaySet (\ s a -> s{_sirOccurrenceDaySet = a}) . _Default . _Coerce

instance FromXML ScheduledInstanceRecurrence where
        parseXML x
          = ScheduledInstanceRecurrence' <$>
              (x .@? "frequency") <*>
                (x .@? "occurrenceRelativeToEnd")
                <*> (x .@? "occurrenceUnit")
                <*> (x .@? "interval")
                <*>
                (x .@? "occurrenceDaySet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData ScheduledInstanceRecurrence where

-- | Describes the recurring schedule for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstanceRecurrenceRequest' smart constructor.
data ScheduledInstanceRecurrenceRequest = ScheduledInstanceRecurrenceRequest'
  { _sirrFrequency               :: !(Maybe Text)
  , _sirrOccurrenceRelativeToEnd :: !(Maybe Bool)
  , _sirrOccurrenceDays          :: !(Maybe [Int])
  , _sirrOccurrenceUnit          :: !(Maybe Text)
  , _sirrInterval                :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstanceRecurrenceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirrFrequency' - The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- * 'sirrOccurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
--
-- * 'sirrOccurrenceDays' - The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
--
-- * 'sirrOccurrenceUnit' - The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
--
-- * 'sirrInterval' - The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
scheduledInstanceRecurrenceRequest
    :: ScheduledInstanceRecurrenceRequest
scheduledInstanceRecurrenceRequest =
  ScheduledInstanceRecurrenceRequest'
    { _sirrFrequency = Nothing
    , _sirrOccurrenceRelativeToEnd = Nothing
    , _sirrOccurrenceDays = Nothing
    , _sirrOccurrenceUnit = Nothing
    , _sirrInterval = Nothing
    }


-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
sirrFrequency :: Lens' ScheduledInstanceRecurrenceRequest (Maybe Text)
sirrFrequency = lens _sirrFrequency (\ s a -> s{_sirrFrequency = a})

-- | Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
sirrOccurrenceRelativeToEnd :: Lens' ScheduledInstanceRecurrenceRequest (Maybe Bool)
sirrOccurrenceRelativeToEnd = lens _sirrOccurrenceRelativeToEnd (\ s a -> s{_sirrOccurrenceRelativeToEnd = a})

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
sirrOccurrenceDays :: Lens' ScheduledInstanceRecurrenceRequest [Int]
sirrOccurrenceDays = lens _sirrOccurrenceDays (\ s a -> s{_sirrOccurrenceDays = a}) . _Default . _Coerce

-- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
sirrOccurrenceUnit :: Lens' ScheduledInstanceRecurrenceRequest (Maybe Text)
sirrOccurrenceUnit = lens _sirrOccurrenceUnit (\ s a -> s{_sirrOccurrenceUnit = a})

-- | The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
sirrInterval :: Lens' ScheduledInstanceRecurrenceRequest (Maybe Int)
sirrInterval = lens _sirrInterval (\ s a -> s{_sirrInterval = a})

instance NFData ScheduledInstanceRecurrenceRequest
         where

instance ToQuery ScheduledInstanceRecurrenceRequest
         where
        toQuery ScheduledInstanceRecurrenceRequest'{..}
          = mconcat
              ["Frequency" =: _sirrFrequency,
               "OccurrenceRelativeToEnd" =:
                 _sirrOccurrenceRelativeToEnd,
               toQuery
                 (toQueryList "OccurrenceDay" <$>
                    _sirrOccurrenceDays),
               "OccurrenceUnit" =: _sirrOccurrenceUnit,
               "Interval" =: _sirrInterval]

-- | Describes a block device mapping for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesBlockDeviceMapping' smart constructor.
data ScheduledInstancesBlockDeviceMapping = ScheduledInstancesBlockDeviceMapping'
  { _sibdmVirtualName :: !(Maybe Text)
  , _sibdmNoDevice    :: !(Maybe Text)
  , _sibdmEBS         :: !(Maybe ScheduledInstancesEBS)
  , _sibdmDeviceName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesBlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sibdmVirtualName' - The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with two available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume. Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
--
-- * 'sibdmNoDevice' - Suppresses the specified device included in the block device mapping of the AMI.
--
-- * 'sibdmEBS' - Parameters used to set up EBS volumes automatically when the instance is launched.
--
-- * 'sibdmDeviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
scheduledInstancesBlockDeviceMapping
    :: ScheduledInstancesBlockDeviceMapping
scheduledInstancesBlockDeviceMapping =
  ScheduledInstancesBlockDeviceMapping'
    { _sibdmVirtualName = Nothing
    , _sibdmNoDevice = Nothing
    , _sibdmEBS = Nothing
    , _sibdmDeviceName = Nothing
    }


-- | The virtual device name (@ephemeral@ N). Instance store volumes are numbered starting from 0. An instance type with two available instance store volumes can specify mappings for @ephemeral0@ and @ephemeral1@ . The number of available instance store volumes depends on the instance type. After you connect to the instance, you must mount the volume. Constraints: For M3 instances, you must specify instance store volumes in the block device mapping for the instance. When you launch an M3 instance, we ignore any instance store volumes specified in the block device mapping for the AMI.
sibdmVirtualName :: Lens' ScheduledInstancesBlockDeviceMapping (Maybe Text)
sibdmVirtualName = lens _sibdmVirtualName (\ s a -> s{_sibdmVirtualName = a})

-- | Suppresses the specified device included in the block device mapping of the AMI.
sibdmNoDevice :: Lens' ScheduledInstancesBlockDeviceMapping (Maybe Text)
sibdmNoDevice = lens _sibdmNoDevice (\ s a -> s{_sibdmNoDevice = a})

-- | Parameters used to set up EBS volumes automatically when the instance is launched.
sibdmEBS :: Lens' ScheduledInstancesBlockDeviceMapping (Maybe ScheduledInstancesEBS)
sibdmEBS = lens _sibdmEBS (\ s a -> s{_sibdmEBS = a})

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
sibdmDeviceName :: Lens' ScheduledInstancesBlockDeviceMapping (Maybe Text)
sibdmDeviceName = lens _sibdmDeviceName (\ s a -> s{_sibdmDeviceName = a})

instance NFData ScheduledInstancesBlockDeviceMapping
         where

instance ToQuery ScheduledInstancesBlockDeviceMapping
         where
        toQuery ScheduledInstancesBlockDeviceMapping'{..}
          = mconcat
              ["VirtualName" =: _sibdmVirtualName,
               "NoDevice" =: _sibdmNoDevice, "Ebs" =: _sibdmEBS,
               "DeviceName" =: _sibdmDeviceName]

-- | Describes an EBS volume for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesEBS' smart constructor.
data ScheduledInstancesEBS = ScheduledInstancesEBS'
  { _sieDeleteOnTermination :: !(Maybe Bool)
  , _sieVolumeSize          :: !(Maybe Int)
  , _sieIOPS                :: !(Maybe Int)
  , _sieEncrypted           :: !(Maybe Bool)
  , _sieVolumeType          :: !(Maybe Text)
  , _sieSnapshotId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesEBS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sieDeleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- * 'sieVolumeSize' - The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'sieIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For io1 volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about @gp2@ baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraint: Range is 100-20000 IOPS for @io1@ volumes and 100-10000 IOPS for @gp2@ volumes. Condition: This parameter is required for requests to create @io1@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- * 'sieEncrypted' - Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
--
-- * 'sieVolumeType' - The volume type. @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic. Default: @standard@
--
-- * 'sieSnapshotId' - The ID of the snapshot.
scheduledInstancesEBS
    :: ScheduledInstancesEBS
scheduledInstancesEBS =
  ScheduledInstancesEBS'
    { _sieDeleteOnTermination = Nothing
    , _sieVolumeSize = Nothing
    , _sieIOPS = Nothing
    , _sieEncrypted = Nothing
    , _sieVolumeType = Nothing
    , _sieSnapshotId = Nothing
    }


-- | Indicates whether the volume is deleted on instance termination.
sieDeleteOnTermination :: Lens' ScheduledInstancesEBS (Maybe Bool)
sieDeleteOnTermination = lens _sieDeleteOnTermination (\ s a -> s{_sieDeleteOnTermination = a})

-- | The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
sieVolumeSize :: Lens' ScheduledInstancesEBS (Maybe Int)
sieVolumeSize = lens _sieVolumeSize (\ s a -> s{_sieVolumeSize = a})

-- | The number of I/O operations per second (IOPS) that the volume supports. For io1 volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about @gp2@ baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraint: Range is 100-20000 IOPS for @io1@ volumes and 100-10000 IOPS for @gp2@ volumes. Condition: This parameter is required for requests to create @io1@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
sieIOPS :: Lens' ScheduledInstancesEBS (Maybe Int)
sieIOPS = lens _sieIOPS (\ s a -> s{_sieIOPS = a})

-- | Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
sieEncrypted :: Lens' ScheduledInstancesEBS (Maybe Bool)
sieEncrypted = lens _sieEncrypted (\ s a -> s{_sieEncrypted = a})

-- | The volume type. @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic. Default: @standard@
sieVolumeType :: Lens' ScheduledInstancesEBS (Maybe Text)
sieVolumeType = lens _sieVolumeType (\ s a -> s{_sieVolumeType = a})

-- | The ID of the snapshot.
sieSnapshotId :: Lens' ScheduledInstancesEBS (Maybe Text)
sieSnapshotId = lens _sieSnapshotId (\ s a -> s{_sieSnapshotId = a})

instance NFData ScheduledInstancesEBS where

instance ToQuery ScheduledInstancesEBS where
        toQuery ScheduledInstancesEBS'{..}
          = mconcat
              ["DeleteOnTermination" =: _sieDeleteOnTermination,
               "VolumeSize" =: _sieVolumeSize, "Iops" =: _sieIOPS,
               "Encrypted" =: _sieEncrypted,
               "VolumeType" =: _sieVolumeType,
               "SnapshotId" =: _sieSnapshotId]

-- | Describes an IAM instance profile for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesIAMInstanceProfile' smart constructor.
data ScheduledInstancesIAMInstanceProfile = ScheduledInstancesIAMInstanceProfile'
  { _siiapARN  :: !(Maybe Text)
  , _siiapName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesIAMInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siiapARN' - The Amazon Resource Name (ARN).
--
-- * 'siiapName' - The name.
scheduledInstancesIAMInstanceProfile
    :: ScheduledInstancesIAMInstanceProfile
scheduledInstancesIAMInstanceProfile =
  ScheduledInstancesIAMInstanceProfile'
    {_siiapARN = Nothing, _siiapName = Nothing}


-- | The Amazon Resource Name (ARN).
siiapARN :: Lens' ScheduledInstancesIAMInstanceProfile (Maybe Text)
siiapARN = lens _siiapARN (\ s a -> s{_siiapARN = a})

-- | The name.
siiapName :: Lens' ScheduledInstancesIAMInstanceProfile (Maybe Text)
siiapName = lens _siiapName (\ s a -> s{_siiapName = a})

instance NFData ScheduledInstancesIAMInstanceProfile
         where

instance ToQuery ScheduledInstancesIAMInstanceProfile
         where
        toQuery ScheduledInstancesIAMInstanceProfile'{..}
          = mconcat ["Arn" =: _siiapARN, "Name" =: _siiapName]

-- | Describes an IPv6 address.
--
--
--
-- /See:/ 'scheduledInstancesIPv6Address' smart constructor.
newtype ScheduledInstancesIPv6Address = ScheduledInstancesIPv6Address'
  { _siiaIPv6Address :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesIPv6Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siiaIPv6Address' - The IPv6 address.
scheduledInstancesIPv6Address
    :: ScheduledInstancesIPv6Address
scheduledInstancesIPv6Address =
  ScheduledInstancesIPv6Address' {_siiaIPv6Address = Nothing}


-- | The IPv6 address.
siiaIPv6Address :: Lens' ScheduledInstancesIPv6Address (Maybe Text)
siiaIPv6Address = lens _siiaIPv6Address (\ s a -> s{_siiaIPv6Address = a})

instance NFData ScheduledInstancesIPv6Address where

instance ToQuery ScheduledInstancesIPv6Address where
        toQuery ScheduledInstancesIPv6Address'{..}
          = mconcat ["Ipv6Address" =: _siiaIPv6Address]

-- | Describes the launch specification for a Scheduled Instance.
--
--
-- If you are launching the Scheduled Instance in EC2-VPC, you must specify the ID of the subnet. You can specify the subnet using either @SubnetId@ or @NetworkInterface@ .
--
--
-- /See:/ 'scheduledInstancesLaunchSpecification' smart constructor.
data ScheduledInstancesLaunchSpecification = ScheduledInstancesLaunchSpecification'
  { _silsSecurityGroupIds    :: !(Maybe [Text])
  , _silsKeyName             :: !(Maybe Text)
  , _silsNetworkInterfaces   :: !(Maybe [ScheduledInstancesNetworkInterface])
  , _silsRAMDiskId           :: !(Maybe Text)
  , _silsSubnetId            :: !(Maybe Text)
  , _silsKernelId            :: !(Maybe Text)
  , _silsInstanceType        :: !(Maybe Text)
  , _silsEBSOptimized        :: !(Maybe Bool)
  , _silsUserData            :: !(Maybe Text)
  , _silsMonitoring          :: !(Maybe ScheduledInstancesMonitoring)
  , _silsIAMInstanceProfile  :: !(Maybe ScheduledInstancesIAMInstanceProfile)
  , _silsBlockDeviceMappings :: !(Maybe [ScheduledInstancesBlockDeviceMapping])
  , _silsPlacement           :: !(Maybe ScheduledInstancesPlacement)
  , _silsImageId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesLaunchSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'silsSecurityGroupIds' - The IDs of one or more security groups.
--
-- * 'silsKeyName' - The name of the key pair.
--
-- * 'silsNetworkInterfaces' - One or more network interfaces.
--
-- * 'silsRAMDiskId' - The ID of the RAM disk.
--
-- * 'silsSubnetId' - The ID of the subnet in which to launch the instances.
--
-- * 'silsKernelId' - The ID of the kernel.
--
-- * 'silsInstanceType' - The instance type.
--
-- * 'silsEBSOptimized' - Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance. Default: @false@
--
-- * 'silsUserData' - The base64-encoded MIME user data.
--
-- * 'silsMonitoring' - Enable or disable monitoring for the instances.
--
-- * 'silsIAMInstanceProfile' - The IAM instance profile.
--
-- * 'silsBlockDeviceMappings' - One or more block device mapping entries.
--
-- * 'silsPlacement' - The placement information.
--
-- * 'silsImageId' - The ID of the Amazon Machine Image (AMI).
scheduledInstancesLaunchSpecification
    :: Text -- ^ 'silsImageId'
    -> ScheduledInstancesLaunchSpecification
scheduledInstancesLaunchSpecification pImageId_ =
  ScheduledInstancesLaunchSpecification'
    { _silsSecurityGroupIds = Nothing
    , _silsKeyName = Nothing
    , _silsNetworkInterfaces = Nothing
    , _silsRAMDiskId = Nothing
    , _silsSubnetId = Nothing
    , _silsKernelId = Nothing
    , _silsInstanceType = Nothing
    , _silsEBSOptimized = Nothing
    , _silsUserData = Nothing
    , _silsMonitoring = Nothing
    , _silsIAMInstanceProfile = Nothing
    , _silsBlockDeviceMappings = Nothing
    , _silsPlacement = Nothing
    , _silsImageId = pImageId_
    }


-- | The IDs of one or more security groups.
silsSecurityGroupIds :: Lens' ScheduledInstancesLaunchSpecification [Text]
silsSecurityGroupIds = lens _silsSecurityGroupIds (\ s a -> s{_silsSecurityGroupIds = a}) . _Default . _Coerce

-- | The name of the key pair.
silsKeyName :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsKeyName = lens _silsKeyName (\ s a -> s{_silsKeyName = a})

-- | One or more network interfaces.
silsNetworkInterfaces :: Lens' ScheduledInstancesLaunchSpecification [ScheduledInstancesNetworkInterface]
silsNetworkInterfaces = lens _silsNetworkInterfaces (\ s a -> s{_silsNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk.
silsRAMDiskId :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsRAMDiskId = lens _silsRAMDiskId (\ s a -> s{_silsRAMDiskId = a})

-- | The ID of the subnet in which to launch the instances.
silsSubnetId :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsSubnetId = lens _silsSubnetId (\ s a -> s{_silsSubnetId = a})

-- | The ID of the kernel.
silsKernelId :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsKernelId = lens _silsKernelId (\ s a -> s{_silsKernelId = a})

-- | The instance type.
silsInstanceType :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsInstanceType = lens _silsInstanceType (\ s a -> s{_silsInstanceType = a})

-- | Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS-optimized instance. Default: @false@
silsEBSOptimized :: Lens' ScheduledInstancesLaunchSpecification (Maybe Bool)
silsEBSOptimized = lens _silsEBSOptimized (\ s a -> s{_silsEBSOptimized = a})

-- | The base64-encoded MIME user data.
silsUserData :: Lens' ScheduledInstancesLaunchSpecification (Maybe Text)
silsUserData = lens _silsUserData (\ s a -> s{_silsUserData = a})

-- | Enable or disable monitoring for the instances.
silsMonitoring :: Lens' ScheduledInstancesLaunchSpecification (Maybe ScheduledInstancesMonitoring)
silsMonitoring = lens _silsMonitoring (\ s a -> s{_silsMonitoring = a})

-- | The IAM instance profile.
silsIAMInstanceProfile :: Lens' ScheduledInstancesLaunchSpecification (Maybe ScheduledInstancesIAMInstanceProfile)
silsIAMInstanceProfile = lens _silsIAMInstanceProfile (\ s a -> s{_silsIAMInstanceProfile = a})

-- | One or more block device mapping entries.
silsBlockDeviceMappings :: Lens' ScheduledInstancesLaunchSpecification [ScheduledInstancesBlockDeviceMapping]
silsBlockDeviceMappings = lens _silsBlockDeviceMappings (\ s a -> s{_silsBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement information.
silsPlacement :: Lens' ScheduledInstancesLaunchSpecification (Maybe ScheduledInstancesPlacement)
silsPlacement = lens _silsPlacement (\ s a -> s{_silsPlacement = a})

-- | The ID of the Amazon Machine Image (AMI).
silsImageId :: Lens' ScheduledInstancesLaunchSpecification Text
silsImageId = lens _silsImageId (\ s a -> s{_silsImageId = a})

instance NFData ScheduledInstancesLaunchSpecification
         where

instance ToQuery
           ScheduledInstancesLaunchSpecification
         where
        toQuery ScheduledInstancesLaunchSpecification'{..}
          = mconcat
              [toQuery
                 (toQueryList "SecurityGroupId" <$>
                    _silsSecurityGroupIds),
               "KeyName" =: _silsKeyName,
               toQuery
                 (toQueryList "NetworkInterface" <$>
                    _silsNetworkInterfaces),
               "RamdiskId" =: _silsRAMDiskId,
               "SubnetId" =: _silsSubnetId,
               "KernelId" =: _silsKernelId,
               "InstanceType" =: _silsInstanceType,
               "EbsOptimized" =: _silsEBSOptimized,
               "UserData" =: _silsUserData,
               "Monitoring" =: _silsMonitoring,
               "IamInstanceProfile" =: _silsIAMInstanceProfile,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _silsBlockDeviceMappings),
               "Placement" =: _silsPlacement,
               "ImageId" =: _silsImageId]

-- | Describes whether monitoring is enabled for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesMonitoring' smart constructor.
newtype ScheduledInstancesMonitoring = ScheduledInstancesMonitoring'
  { _simEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simEnabled' - Indicates whether monitoring is enabled.
scheduledInstancesMonitoring
    :: ScheduledInstancesMonitoring
scheduledInstancesMonitoring =
  ScheduledInstancesMonitoring' {_simEnabled = Nothing}


-- | Indicates whether monitoring is enabled.
simEnabled :: Lens' ScheduledInstancesMonitoring (Maybe Bool)
simEnabled = lens _simEnabled (\ s a -> s{_simEnabled = a})

instance NFData ScheduledInstancesMonitoring where

instance ToQuery ScheduledInstancesMonitoring where
        toQuery ScheduledInstancesMonitoring'{..}
          = mconcat ["Enabled" =: _simEnabled]

-- | Describes a network interface for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesNetworkInterface' smart constructor.
data ScheduledInstancesNetworkInterface = ScheduledInstancesNetworkInterface'
  { _siniGroups :: !(Maybe [Text])
  , _siniDeleteOnTermination :: !(Maybe Bool)
  , _siniAssociatePublicIPAddress :: !(Maybe Bool)
  , _siniPrivateIPAddressConfigs :: !(Maybe [ScheduledInstancesPrivateIPAddressConfig])
  , _siniNetworkInterfaceId :: !(Maybe Text)
  , _siniSubnetId :: !(Maybe Text)
  , _siniIPv6AddressCount :: !(Maybe Int)
  , _siniPrivateIPAddress :: !(Maybe Text)
  , _siniSecondaryPrivateIPAddressCount :: !(Maybe Int)
  , _siniDescription :: !(Maybe Text)
  , _siniDeviceIndex :: !(Maybe Int)
  , _siniIPv6Addresses :: !(Maybe [ScheduledInstancesIPv6Address])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesNetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siniGroups' - The IDs of one or more security groups.
--
-- * 'siniDeleteOnTermination' - Indicates whether to delete the interface when the instance is terminated.
--
-- * 'siniAssociatePublicIPAddress' - Indicates whether to assign a public IPv4 address to instances launched in a VPC. The public IPv4 address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
--
-- * 'siniPrivateIPAddressConfigs' - The private IPv4 addresses.
--
-- * 'siniNetworkInterfaceId' - The ID of the network interface.
--
-- * 'siniSubnetId' - The ID of the subnet.
--
-- * 'siniIPv6AddressCount' - The number of IPv6 addresses to assign to the network interface. The IPv6 addresses are automatically selected from the subnet range.
--
-- * 'siniPrivateIPAddress' - The IPv4 address of the network interface within the subnet.
--
-- * 'siniSecondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses.
--
-- * 'siniDescription' - The description.
--
-- * 'siniDeviceIndex' - The index of the device for the network interface attachment.
--
-- * 'siniIPv6Addresses' - One or more specific IPv6 addresses from the subnet range.
scheduledInstancesNetworkInterface
    :: ScheduledInstancesNetworkInterface
scheduledInstancesNetworkInterface =
  ScheduledInstancesNetworkInterface'
    { _siniGroups = Nothing
    , _siniDeleteOnTermination = Nothing
    , _siniAssociatePublicIPAddress = Nothing
    , _siniPrivateIPAddressConfigs = Nothing
    , _siniNetworkInterfaceId = Nothing
    , _siniSubnetId = Nothing
    , _siniIPv6AddressCount = Nothing
    , _siniPrivateIPAddress = Nothing
    , _siniSecondaryPrivateIPAddressCount = Nothing
    , _siniDescription = Nothing
    , _siniDeviceIndex = Nothing
    , _siniIPv6Addresses = Nothing
    }


-- | The IDs of one or more security groups.
siniGroups :: Lens' ScheduledInstancesNetworkInterface [Text]
siniGroups = lens _siniGroups (\ s a -> s{_siniGroups = a}) . _Default . _Coerce

-- | Indicates whether to delete the interface when the instance is terminated.
siniDeleteOnTermination :: Lens' ScheduledInstancesNetworkInterface (Maybe Bool)
siniDeleteOnTermination = lens _siniDeleteOnTermination (\ s a -> s{_siniDeleteOnTermination = a})

-- | Indicates whether to assign a public IPv4 address to instances launched in a VPC. The public IPv4 address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
siniAssociatePublicIPAddress :: Lens' ScheduledInstancesNetworkInterface (Maybe Bool)
siniAssociatePublicIPAddress = lens _siniAssociatePublicIPAddress (\ s a -> s{_siniAssociatePublicIPAddress = a})

-- | The private IPv4 addresses.
siniPrivateIPAddressConfigs :: Lens' ScheduledInstancesNetworkInterface [ScheduledInstancesPrivateIPAddressConfig]
siniPrivateIPAddressConfigs = lens _siniPrivateIPAddressConfigs (\ s a -> s{_siniPrivateIPAddressConfigs = a}) . _Default . _Coerce

-- | The ID of the network interface.
siniNetworkInterfaceId :: Lens' ScheduledInstancesNetworkInterface (Maybe Text)
siniNetworkInterfaceId = lens _siniNetworkInterfaceId (\ s a -> s{_siniNetworkInterfaceId = a})

-- | The ID of the subnet.
siniSubnetId :: Lens' ScheduledInstancesNetworkInterface (Maybe Text)
siniSubnetId = lens _siniSubnetId (\ s a -> s{_siniSubnetId = a})

-- | The number of IPv6 addresses to assign to the network interface. The IPv6 addresses are automatically selected from the subnet range.
siniIPv6AddressCount :: Lens' ScheduledInstancesNetworkInterface (Maybe Int)
siniIPv6AddressCount = lens _siniIPv6AddressCount (\ s a -> s{_siniIPv6AddressCount = a})

-- | The IPv4 address of the network interface within the subnet.
siniPrivateIPAddress :: Lens' ScheduledInstancesNetworkInterface (Maybe Text)
siniPrivateIPAddress = lens _siniPrivateIPAddress (\ s a -> s{_siniPrivateIPAddress = a})

-- | The number of secondary private IPv4 addresses.
siniSecondaryPrivateIPAddressCount :: Lens' ScheduledInstancesNetworkInterface (Maybe Int)
siniSecondaryPrivateIPAddressCount = lens _siniSecondaryPrivateIPAddressCount (\ s a -> s{_siniSecondaryPrivateIPAddressCount = a})

-- | The description.
siniDescription :: Lens' ScheduledInstancesNetworkInterface (Maybe Text)
siniDescription = lens _siniDescription (\ s a -> s{_siniDescription = a})

-- | The index of the device for the network interface attachment.
siniDeviceIndex :: Lens' ScheduledInstancesNetworkInterface (Maybe Int)
siniDeviceIndex = lens _siniDeviceIndex (\ s a -> s{_siniDeviceIndex = a})

-- | One or more specific IPv6 addresses from the subnet range.
siniIPv6Addresses :: Lens' ScheduledInstancesNetworkInterface [ScheduledInstancesIPv6Address]
siniIPv6Addresses = lens _siniIPv6Addresses (\ s a -> s{_siniIPv6Addresses = a}) . _Default . _Coerce

instance NFData ScheduledInstancesNetworkInterface
         where

instance ToQuery ScheduledInstancesNetworkInterface
         where
        toQuery ScheduledInstancesNetworkInterface'{..}
          = mconcat
              [toQuery (toQueryList "Group" <$> _siniGroups),
               "DeleteOnTermination" =: _siniDeleteOnTermination,
               "AssociatePublicIpAddress" =:
                 _siniAssociatePublicIPAddress,
               toQuery
                 (toQueryList "PrivateIpAddressConfig" <$>
                    _siniPrivateIPAddressConfigs),
               "NetworkInterfaceId" =: _siniNetworkInterfaceId,
               "SubnetId" =: _siniSubnetId,
               "Ipv6AddressCount" =: _siniIPv6AddressCount,
               "PrivateIpAddress" =: _siniPrivateIPAddress,
               "SecondaryPrivateIpAddressCount" =:
                 _siniSecondaryPrivateIPAddressCount,
               "Description" =: _siniDescription,
               "DeviceIndex" =: _siniDeviceIndex,
               toQuery
                 (toQueryList "Ipv6Address" <$> _siniIPv6Addresses)]

-- | Describes the placement for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesPlacement' smart constructor.
data ScheduledInstancesPlacement = ScheduledInstancesPlacement'
  { _sipAvailabilityZone :: !(Maybe Text)
  , _sipGroupName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sipAvailabilityZone' - The Availability Zone.
--
-- * 'sipGroupName' - The name of the placement group.
scheduledInstancesPlacement
    :: ScheduledInstancesPlacement
scheduledInstancesPlacement =
  ScheduledInstancesPlacement'
    {_sipAvailabilityZone = Nothing, _sipGroupName = Nothing}


-- | The Availability Zone.
sipAvailabilityZone :: Lens' ScheduledInstancesPlacement (Maybe Text)
sipAvailabilityZone = lens _sipAvailabilityZone (\ s a -> s{_sipAvailabilityZone = a})

-- | The name of the placement group.
sipGroupName :: Lens' ScheduledInstancesPlacement (Maybe Text)
sipGroupName = lens _sipGroupName (\ s a -> s{_sipGroupName = a})

instance NFData ScheduledInstancesPlacement where

instance ToQuery ScheduledInstancesPlacement where
        toQuery ScheduledInstancesPlacement'{..}
          = mconcat
              ["AvailabilityZone" =: _sipAvailabilityZone,
               "GroupName" =: _sipGroupName]

-- | Describes a private IPv4 address for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesPrivateIPAddressConfig' smart constructor.
data ScheduledInstancesPrivateIPAddressConfig = ScheduledInstancesPrivateIPAddressConfig'
  { _sipiacPrimary          :: !(Maybe Bool)
  , _sipiacPrivateIPAddress :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledInstancesPrivateIPAddressConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sipiacPrimary' - Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
--
-- * 'sipiacPrivateIPAddress' - The IPv4 address.
scheduledInstancesPrivateIPAddressConfig
    :: ScheduledInstancesPrivateIPAddressConfig
scheduledInstancesPrivateIPAddressConfig =
  ScheduledInstancesPrivateIPAddressConfig'
    {_sipiacPrimary = Nothing, _sipiacPrivateIPAddress = Nothing}


-- | Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
sipiacPrimary :: Lens' ScheduledInstancesPrivateIPAddressConfig (Maybe Bool)
sipiacPrimary = lens _sipiacPrimary (\ s a -> s{_sipiacPrimary = a})

-- | The IPv4 address.
sipiacPrivateIPAddress :: Lens' ScheduledInstancesPrivateIPAddressConfig (Maybe Text)
sipiacPrivateIPAddress = lens _sipiacPrivateIPAddress (\ s a -> s{_sipiacPrivateIPAddress = a})

instance NFData
           ScheduledInstancesPrivateIPAddressConfig
         where

instance ToQuery
           ScheduledInstancesPrivateIPAddressConfig
         where
        toQuery ScheduledInstancesPrivateIPAddressConfig'{..}
          = mconcat
              ["Primary" =: _sipiacPrimary,
               "PrivateIpAddress" =: _sipiacPrivateIPAddress]

-- | Describes a security group
--
--
--
-- /See:/ 'securityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { _sgVPCId               :: !(Maybe Text)
  , _sgIPPermissions       :: !(Maybe [IPPermission])
  , _sgIPPermissionsEgress :: !(Maybe [IPPermission])
  , _sgTags                :: !(Maybe [Tag])
  , _sgOwnerId             :: !Text
  , _sgGroupId             :: !Text
  , _sgGroupName           :: !Text
  , _sgDescription         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgVPCId' - [EC2-VPC] The ID of the VPC for the security group.
--
-- * 'sgIPPermissions' - One or more inbound rules associated with the security group.
--
-- * 'sgIPPermissionsEgress' - [EC2-VPC] One or more outbound rules associated with the security group.
--
-- * 'sgTags' - Any tags assigned to the security group.
--
-- * 'sgOwnerId' - The AWS account ID of the owner of the security group.
--
-- * 'sgGroupId' - The ID of the security group.
--
-- * 'sgGroupName' - The name of the security group.
--
-- * 'sgDescription' - A description of the security group.
securityGroup
    :: Text -- ^ 'sgOwnerId'
    -> Text -- ^ 'sgGroupId'
    -> Text -- ^ 'sgGroupName'
    -> Text -- ^ 'sgDescription'
    -> SecurityGroup
securityGroup pOwnerId_ pGroupId_ pGroupName_ pDescription_ =
  SecurityGroup'
    { _sgVPCId = Nothing
    , _sgIPPermissions = Nothing
    , _sgIPPermissionsEgress = Nothing
    , _sgTags = Nothing
    , _sgOwnerId = pOwnerId_
    , _sgGroupId = pGroupId_
    , _sgGroupName = pGroupName_
    , _sgDescription = pDescription_
    }


-- | [EC2-VPC] The ID of the VPC for the security group.
sgVPCId :: Lens' SecurityGroup (Maybe Text)
sgVPCId = lens _sgVPCId (\ s a -> s{_sgVPCId = a})

-- | One or more inbound rules associated with the security group.
sgIPPermissions :: Lens' SecurityGroup [IPPermission]
sgIPPermissions = lens _sgIPPermissions (\ s a -> s{_sgIPPermissions = a}) . _Default . _Coerce

-- | [EC2-VPC] One or more outbound rules associated with the security group.
sgIPPermissionsEgress :: Lens' SecurityGroup [IPPermission]
sgIPPermissionsEgress = lens _sgIPPermissionsEgress (\ s a -> s{_sgIPPermissionsEgress = a}) . _Default . _Coerce

-- | Any tags assigned to the security group.
sgTags :: Lens' SecurityGroup [Tag]
sgTags = lens _sgTags (\ s a -> s{_sgTags = a}) . _Default . _Coerce

-- | The AWS account ID of the owner of the security group.
sgOwnerId :: Lens' SecurityGroup Text
sgOwnerId = lens _sgOwnerId (\ s a -> s{_sgOwnerId = a})

-- | The ID of the security group.
sgGroupId :: Lens' SecurityGroup Text
sgGroupId = lens _sgGroupId (\ s a -> s{_sgGroupId = a})

-- | The name of the security group.
sgGroupName :: Lens' SecurityGroup Text
sgGroupName = lens _sgGroupName (\ s a -> s{_sgGroupName = a})

-- | A description of the security group.
sgDescription :: Lens' SecurityGroup Text
sgDescription = lens _sgDescription (\ s a -> s{_sgDescription = a})

instance FromXML SecurityGroup where
        parseXML x
          = SecurityGroup' <$>
              (x .@? "vpcId") <*>
                (x .@? "ipPermissions" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "ipPermissionsEgress" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "ownerId")
                <*> (x .@ "groupId")
                <*> (x .@ "groupName")
                <*> (x .@ "groupDescription")

instance NFData SecurityGroup where

-- | Describes a security group.
--
--
--
-- /See:/ 'securityGroupIdentifier' smart constructor.
data SecurityGroupIdentifier = SecurityGroupIdentifier'
  { _sgiGroupId   :: !(Maybe Text)
  , _sgiGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecurityGroupIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgiGroupId' - The ID of the security group.
--
-- * 'sgiGroupName' - The name of the security group.
securityGroupIdentifier
    :: SecurityGroupIdentifier
securityGroupIdentifier =
  SecurityGroupIdentifier' {_sgiGroupId = Nothing, _sgiGroupName = Nothing}


-- | The ID of the security group.
sgiGroupId :: Lens' SecurityGroupIdentifier (Maybe Text)
sgiGroupId = lens _sgiGroupId (\ s a -> s{_sgiGroupId = a})

-- | The name of the security group.
sgiGroupName :: Lens' SecurityGroupIdentifier (Maybe Text)
sgiGroupName = lens _sgiGroupName (\ s a -> s{_sgiGroupName = a})

instance FromXML SecurityGroupIdentifier where
        parseXML x
          = SecurityGroupIdentifier' <$>
              (x .@? "groupId") <*> (x .@? "groupName")

instance NFData SecurityGroupIdentifier where

-- | Describes a VPC with a security group that references your security group.
--
--
--
-- /See:/ 'securityGroupReference' smart constructor.
data SecurityGroupReference = SecurityGroupReference'
  { _sgrVPCPeeringConnectionId :: !(Maybe Text)
  , _sgrGroupId                :: !Text
  , _sgrReferencingVPCId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecurityGroupReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgrVPCPeeringConnectionId' - The ID of the VPC peering connection.
--
-- * 'sgrGroupId' - The ID of your security group.
--
-- * 'sgrReferencingVPCId' - The ID of the VPC with the referencing security group.
securityGroupReference
    :: Text -- ^ 'sgrGroupId'
    -> Text -- ^ 'sgrReferencingVPCId'
    -> SecurityGroupReference
securityGroupReference pGroupId_ pReferencingVPCId_ =
  SecurityGroupReference'
    { _sgrVPCPeeringConnectionId = Nothing
    , _sgrGroupId = pGroupId_
    , _sgrReferencingVPCId = pReferencingVPCId_
    }


-- | The ID of the VPC peering connection.
sgrVPCPeeringConnectionId :: Lens' SecurityGroupReference (Maybe Text)
sgrVPCPeeringConnectionId = lens _sgrVPCPeeringConnectionId (\ s a -> s{_sgrVPCPeeringConnectionId = a})

-- | The ID of your security group.
sgrGroupId :: Lens' SecurityGroupReference Text
sgrGroupId = lens _sgrGroupId (\ s a -> s{_sgrGroupId = a})

-- | The ID of the VPC with the referencing security group.
sgrReferencingVPCId :: Lens' SecurityGroupReference Text
sgrReferencingVPCId = lens _sgrReferencingVPCId (\ s a -> s{_sgrReferencingVPCId = a})

instance FromXML SecurityGroupReference where
        parseXML x
          = SecurityGroupReference' <$>
              (x .@? "vpcPeeringConnectionId") <*> (x .@ "groupId")
                <*> (x .@ "referencingVpcId")

instance NFData SecurityGroupReference where

-- | Describes a service configuration for a VPC endpoint service.
--
--
--
-- /See:/ 'serviceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { _scNetworkLoadBalancerARNs :: !(Maybe [Text])
  , _scBaseEndpointDNSNames    :: !(Maybe [Text])
  , _scAvailabilityZones       :: !(Maybe [Text])
  , _scServiceName             :: !(Maybe Text)
  , _scServiceState            :: !(Maybe ServiceState)
  , _scServiceType             :: !(Maybe [ServiceTypeDetail])
  , _scAcceptanceRequired      :: !(Maybe Bool)
  , _scServiceId               :: !(Maybe Text)
  , _scPrivateDNSName          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scNetworkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
--
-- * 'scBaseEndpointDNSNames' - The DNS names for the service.
--
-- * 'scAvailabilityZones' - In the Availability Zones in which the service is available.
--
-- * 'scServiceName' - The name of the service.
--
-- * 'scServiceState' - The service state.
--
-- * 'scServiceType' - The type of service.
--
-- * 'scAcceptanceRequired' - Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
--
-- * 'scServiceId' - The ID of the service.
--
-- * 'scPrivateDNSName' - The private DNS name for the service.
serviceConfiguration
    :: ServiceConfiguration
serviceConfiguration =
  ServiceConfiguration'
    { _scNetworkLoadBalancerARNs = Nothing
    , _scBaseEndpointDNSNames = Nothing
    , _scAvailabilityZones = Nothing
    , _scServiceName = Nothing
    , _scServiceState = Nothing
    , _scServiceType = Nothing
    , _scAcceptanceRequired = Nothing
    , _scServiceId = Nothing
    , _scPrivateDNSName = Nothing
    }


-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
scNetworkLoadBalancerARNs :: Lens' ServiceConfiguration [Text]
scNetworkLoadBalancerARNs = lens _scNetworkLoadBalancerARNs (\ s a -> s{_scNetworkLoadBalancerARNs = a}) . _Default . _Coerce

-- | The DNS names for the service.
scBaseEndpointDNSNames :: Lens' ServiceConfiguration [Text]
scBaseEndpointDNSNames = lens _scBaseEndpointDNSNames (\ s a -> s{_scBaseEndpointDNSNames = a}) . _Default . _Coerce

-- | In the Availability Zones in which the service is available.
scAvailabilityZones :: Lens' ServiceConfiguration [Text]
scAvailabilityZones = lens _scAvailabilityZones (\ s a -> s{_scAvailabilityZones = a}) . _Default . _Coerce

-- | The name of the service.
scServiceName :: Lens' ServiceConfiguration (Maybe Text)
scServiceName = lens _scServiceName (\ s a -> s{_scServiceName = a})

-- | The service state.
scServiceState :: Lens' ServiceConfiguration (Maybe ServiceState)
scServiceState = lens _scServiceState (\ s a -> s{_scServiceState = a})

-- | The type of service.
scServiceType :: Lens' ServiceConfiguration [ServiceTypeDetail]
scServiceType = lens _scServiceType (\ s a -> s{_scServiceType = a}) . _Default . _Coerce

-- | Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
scAcceptanceRequired :: Lens' ServiceConfiguration (Maybe Bool)
scAcceptanceRequired = lens _scAcceptanceRequired (\ s a -> s{_scAcceptanceRequired = a})

-- | The ID of the service.
scServiceId :: Lens' ServiceConfiguration (Maybe Text)
scServiceId = lens _scServiceId (\ s a -> s{_scServiceId = a})

-- | The private DNS name for the service.
scPrivateDNSName :: Lens' ServiceConfiguration (Maybe Text)
scPrivateDNSName = lens _scPrivateDNSName (\ s a -> s{_scPrivateDNSName = a})

instance FromXML ServiceConfiguration where
        parseXML x
          = ServiceConfiguration' <$>
              (x .@? "networkLoadBalancerArnSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*>
                (x .@? "baseEndpointDnsNameSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "availabilityZoneSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "serviceName")
                <*> (x .@? "serviceState")
                <*>
                (x .@? "serviceType" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "acceptanceRequired")
                <*> (x .@? "serviceId")
                <*> (x .@? "privateDnsName")

instance NFData ServiceConfiguration where

-- | Describes a VPC endpoint service.
--
--
--
-- /See:/ 'serviceDetail' smart constructor.
data ServiceDetail = ServiceDetail'
  { _sdVPCEndpointPolicySupported :: !(Maybe Bool)
  , _sdBaseEndpointDNSNames       :: !(Maybe [Text])
  , _sdOwner                      :: !(Maybe Text)
  , _sdAvailabilityZones          :: !(Maybe [Text])
  , _sdServiceName                :: !(Maybe Text)
  , _sdServiceType                :: !(Maybe [ServiceTypeDetail])
  , _sdAcceptanceRequired         :: !(Maybe Bool)
  , _sdPrivateDNSName             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdVPCEndpointPolicySupported' - Indicates whether the service supports endpoint policies.
--
-- * 'sdBaseEndpointDNSNames' - The DNS names for the service.
--
-- * 'sdOwner' - The AWS account ID of the service owner.
--
-- * 'sdAvailabilityZones' - The Availability Zones in which the service is available.
--
-- * 'sdServiceName' - The Amazon Resource Name (ARN) of the service.
--
-- * 'sdServiceType' - The type of service.
--
-- * 'sdAcceptanceRequired' - Indicates whether VPC endpoint connection requests to the service must be accepted by the service owner.
--
-- * 'sdPrivateDNSName' - The private DNS name for the service.
serviceDetail
    :: ServiceDetail
serviceDetail =
  ServiceDetail'
    { _sdVPCEndpointPolicySupported = Nothing
    , _sdBaseEndpointDNSNames = Nothing
    , _sdOwner = Nothing
    , _sdAvailabilityZones = Nothing
    , _sdServiceName = Nothing
    , _sdServiceType = Nothing
    , _sdAcceptanceRequired = Nothing
    , _sdPrivateDNSName = Nothing
    }


-- | Indicates whether the service supports endpoint policies.
sdVPCEndpointPolicySupported :: Lens' ServiceDetail (Maybe Bool)
sdVPCEndpointPolicySupported = lens _sdVPCEndpointPolicySupported (\ s a -> s{_sdVPCEndpointPolicySupported = a})

-- | The DNS names for the service.
sdBaseEndpointDNSNames :: Lens' ServiceDetail [Text]
sdBaseEndpointDNSNames = lens _sdBaseEndpointDNSNames (\ s a -> s{_sdBaseEndpointDNSNames = a}) . _Default . _Coerce

-- | The AWS account ID of the service owner.
sdOwner :: Lens' ServiceDetail (Maybe Text)
sdOwner = lens _sdOwner (\ s a -> s{_sdOwner = a})

-- | The Availability Zones in which the service is available.
sdAvailabilityZones :: Lens' ServiceDetail [Text]
sdAvailabilityZones = lens _sdAvailabilityZones (\ s a -> s{_sdAvailabilityZones = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the service.
sdServiceName :: Lens' ServiceDetail (Maybe Text)
sdServiceName = lens _sdServiceName (\ s a -> s{_sdServiceName = a})

-- | The type of service.
sdServiceType :: Lens' ServiceDetail [ServiceTypeDetail]
sdServiceType = lens _sdServiceType (\ s a -> s{_sdServiceType = a}) . _Default . _Coerce

-- | Indicates whether VPC endpoint connection requests to the service must be accepted by the service owner.
sdAcceptanceRequired :: Lens' ServiceDetail (Maybe Bool)
sdAcceptanceRequired = lens _sdAcceptanceRequired (\ s a -> s{_sdAcceptanceRequired = a})

-- | The private DNS name for the service.
sdPrivateDNSName :: Lens' ServiceDetail (Maybe Text)
sdPrivateDNSName = lens _sdPrivateDNSName (\ s a -> s{_sdPrivateDNSName = a})

instance FromXML ServiceDetail where
        parseXML x
          = ServiceDetail' <$>
              (x .@? "vpcEndpointPolicySupported") <*>
                (x .@? "baseEndpointDnsNameSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "owner")
                <*>
                (x .@? "availabilityZoneSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "serviceName")
                <*>
                (x .@? "serviceType" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "acceptanceRequired")
                <*> (x .@? "privateDnsName")

instance NFData ServiceDetail where

-- | Describes the type of service for a VPC endpoint.
--
--
--
-- /See:/ 'serviceTypeDetail' smart constructor.
newtype ServiceTypeDetail = ServiceTypeDetail'
  { _stdServiceType :: Maybe ServiceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceTypeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdServiceType' - The type of service.
serviceTypeDetail
    :: ServiceTypeDetail
serviceTypeDetail = ServiceTypeDetail' {_stdServiceType = Nothing}


-- | The type of service.
stdServiceType :: Lens' ServiceTypeDetail (Maybe ServiceType)
stdServiceType = lens _stdServiceType (\ s a -> s{_stdServiceType = a})

instance FromXML ServiceTypeDetail where
        parseXML x
          = ServiceTypeDetail' <$> (x .@? "serviceType")

instance NFData ServiceTypeDetail where

-- | Describes the time period for a Scheduled Instance to start its first schedule. The time period must span less than one day.
--
--
--
-- /See:/ 'slotDateTimeRangeRequest' smart constructor.
data SlotDateTimeRangeRequest = SlotDateTimeRangeRequest'
  { _sdtrrEarliestTime :: !ISO8601
  , _sdtrrLatestTime   :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SlotDateTimeRangeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdtrrEarliestTime' - The earliest date and time, in UTC, for the Scheduled Instance to start.
--
-- * 'sdtrrLatestTime' - The latest date and time, in UTC, for the Scheduled Instance to start. This value must be later than or equal to the earliest date and at most three months in the future.
slotDateTimeRangeRequest
    :: UTCTime -- ^ 'sdtrrEarliestTime'
    -> UTCTime -- ^ 'sdtrrLatestTime'
    -> SlotDateTimeRangeRequest
slotDateTimeRangeRequest pEarliestTime_ pLatestTime_ =
  SlotDateTimeRangeRequest'
    { _sdtrrEarliestTime = _Time # pEarliestTime_
    , _sdtrrLatestTime = _Time # pLatestTime_
    }


-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
sdtrrEarliestTime :: Lens' SlotDateTimeRangeRequest UTCTime
sdtrrEarliestTime = lens _sdtrrEarliestTime (\ s a -> s{_sdtrrEarliestTime = a}) . _Time

-- | The latest date and time, in UTC, for the Scheduled Instance to start. This value must be later than or equal to the earliest date and at most three months in the future.
sdtrrLatestTime :: Lens' SlotDateTimeRangeRequest UTCTime
sdtrrLatestTime = lens _sdtrrLatestTime (\ s a -> s{_sdtrrLatestTime = a}) . _Time

instance NFData SlotDateTimeRangeRequest where

instance ToQuery SlotDateTimeRangeRequest where
        toQuery SlotDateTimeRangeRequest'{..}
          = mconcat
              ["EarliestTime" =: _sdtrrEarliestTime,
               "LatestTime" =: _sdtrrLatestTime]

-- | Describes the time period for a Scheduled Instance to start its first schedule.
--
--
--
-- /See:/ 'slotStartTimeRangeRequest' smart constructor.
data SlotStartTimeRangeRequest = SlotStartTimeRangeRequest'
  { _sstrrLatestTime   :: !(Maybe ISO8601)
  , _sstrrEarliestTime :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SlotStartTimeRangeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sstrrLatestTime' - The latest date and time, in UTC, for the Scheduled Instance to start.
--
-- * 'sstrrEarliestTime' - The earliest date and time, in UTC, for the Scheduled Instance to start.
slotStartTimeRangeRequest
    :: SlotStartTimeRangeRequest
slotStartTimeRangeRequest =
  SlotStartTimeRangeRequest'
    {_sstrrLatestTime = Nothing, _sstrrEarliestTime = Nothing}


-- | The latest date and time, in UTC, for the Scheduled Instance to start.
sstrrLatestTime :: Lens' SlotStartTimeRangeRequest (Maybe UTCTime)
sstrrLatestTime = lens _sstrrLatestTime (\ s a -> s{_sstrrLatestTime = a}) . mapping _Time

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
sstrrEarliestTime :: Lens' SlotStartTimeRangeRequest (Maybe UTCTime)
sstrrEarliestTime = lens _sstrrEarliestTime (\ s a -> s{_sstrrEarliestTime = a}) . mapping _Time

instance NFData SlotStartTimeRangeRequest where

instance ToQuery SlotStartTimeRangeRequest where
        toQuery SlotStartTimeRangeRequest'{..}
          = mconcat
              ["LatestTime" =: _sstrrLatestTime,
               "EarliestTime" =: _sstrrEarliestTime]

-- | Describes a snapshot.
--
--
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
  { _sStateMessage        :: !(Maybe Text)
  , _sOwnerAlias          :: !(Maybe Text)
  , _sDataEncryptionKeyId :: !(Maybe Text)
  , _sKMSKeyId            :: !(Maybe Text)
  , _sTags                :: !(Maybe [Tag])
  , _sSnapshotId          :: !Text
  , _sOwnerId             :: !Text
  , _sVolumeId            :: !Text
  , _sVolumeSize          :: !Int
  , _sDescription         :: !Text
  , _sStartTime           :: !ISO8601
  , _sProgress            :: !Text
  , _sState               :: !SnapshotState
  , _sEncrypted           :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStateMessage' - Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by the 'DescribeSnapshots' API operation.
--
-- * 'sOwnerAlias' - Value from an Amazon-maintained list (@amazon@ | @aws-marketplace@ | @microsoft@ ) of snapshot owners. Not to be confused with the user-configured AWS account alias, which is set from the IAM console.
--
-- * 'sDataEncryptionKeyId' - The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by the 'DescribeSnapshots' API operation.
--
-- * 'sKMSKeyId' - The full ARN of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
--
-- * 'sTags' - Any tags assigned to the snapshot.
--
-- * 'sSnapshotId' - The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
--
-- * 'sOwnerId' - The AWS account ID of the EBS snapshot owner.
--
-- * 'sVolumeId' - The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
--
-- * 'sVolumeSize' - The size of the volume, in GiB.
--
-- * 'sDescription' - The description for the snapshot.
--
-- * 'sStartTime' - The time stamp when the snapshot was initiated.
--
-- * 'sProgress' - The progress of the snapshot, as a percentage.
--
-- * 'sState' - The snapshot state.
--
-- * 'sEncrypted' - Indicates whether the snapshot is encrypted.
snapshot
    :: Text -- ^ 'sSnapshotId'
    -> Text -- ^ 'sOwnerId'
    -> Text -- ^ 'sVolumeId'
    -> Int -- ^ 'sVolumeSize'
    -> Text -- ^ 'sDescription'
    -> UTCTime -- ^ 'sStartTime'
    -> Text -- ^ 'sProgress'
    -> SnapshotState -- ^ 'sState'
    -> Bool -- ^ 'sEncrypted'
    -> Snapshot
snapshot pSnapshotId_ pOwnerId_ pVolumeId_ pVolumeSize_ pDescription_ pStartTime_ pProgress_ pState_ pEncrypted_ =
  Snapshot'
    { _sStateMessage = Nothing
    , _sOwnerAlias = Nothing
    , _sDataEncryptionKeyId = Nothing
    , _sKMSKeyId = Nothing
    , _sTags = Nothing
    , _sSnapshotId = pSnapshotId_
    , _sOwnerId = pOwnerId_
    , _sVolumeId = pVolumeId_
    , _sVolumeSize = pVolumeSize_
    , _sDescription = pDescription_
    , _sStartTime = _Time # pStartTime_
    , _sProgress = pProgress_
    , _sState = pState_
    , _sEncrypted = pEncrypted_
    }


-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by the 'DescribeSnapshots' API operation.
sStateMessage :: Lens' Snapshot (Maybe Text)
sStateMessage = lens _sStateMessage (\ s a -> s{_sStateMessage = a})

-- | Value from an Amazon-maintained list (@amazon@ | @aws-marketplace@ | @microsoft@ ) of snapshot owners. Not to be confused with the user-configured AWS account alias, which is set from the IAM console.
sOwnerAlias :: Lens' Snapshot (Maybe Text)
sOwnerAlias = lens _sOwnerAlias (\ s a -> s{_sOwnerAlias = a})

-- | The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by the 'DescribeSnapshots' API operation.
sDataEncryptionKeyId :: Lens' Snapshot (Maybe Text)
sDataEncryptionKeyId = lens _sDataEncryptionKeyId (\ s a -> s{_sDataEncryptionKeyId = a})

-- | The full ARN of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
sKMSKeyId :: Lens' Snapshot (Maybe Text)
sKMSKeyId = lens _sKMSKeyId (\ s a -> s{_sKMSKeyId = a})

-- | Any tags assigned to the snapshot.
sTags :: Lens' Snapshot [Tag]
sTags = lens _sTags (\ s a -> s{_sTags = a}) . _Default . _Coerce

-- | The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
sSnapshotId :: Lens' Snapshot Text
sSnapshotId = lens _sSnapshotId (\ s a -> s{_sSnapshotId = a})

-- | The AWS account ID of the EBS snapshot owner.
sOwnerId :: Lens' Snapshot Text
sOwnerId = lens _sOwnerId (\ s a -> s{_sOwnerId = a})

-- | The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
sVolumeId :: Lens' Snapshot Text
sVolumeId = lens _sVolumeId (\ s a -> s{_sVolumeId = a})

-- | The size of the volume, in GiB.
sVolumeSize :: Lens' Snapshot Int
sVolumeSize = lens _sVolumeSize (\ s a -> s{_sVolumeSize = a})

-- | The description for the snapshot.
sDescription :: Lens' Snapshot Text
sDescription = lens _sDescription (\ s a -> s{_sDescription = a})

-- | The time stamp when the snapshot was initiated.
sStartTime :: Lens' Snapshot UTCTime
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a}) . _Time

-- | The progress of the snapshot, as a percentage.
sProgress :: Lens' Snapshot Text
sProgress = lens _sProgress (\ s a -> s{_sProgress = a})

-- | The snapshot state.
sState :: Lens' Snapshot SnapshotState
sState = lens _sState (\ s a -> s{_sState = a})

-- | Indicates whether the snapshot is encrypted.
sEncrypted :: Lens' Snapshot Bool
sEncrypted = lens _sEncrypted (\ s a -> s{_sEncrypted = a})

instance FromXML Snapshot where
        parseXML x
          = Snapshot' <$>
              (x .@? "statusMessage") <*> (x .@? "ownerAlias") <*>
                (x .@? "dataEncryptionKeyId")
                <*> (x .@? "kmsKeyId")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "snapshotId")
                <*> (x .@ "ownerId")
                <*> (x .@ "volumeId")
                <*> (x .@ "volumeSize")
                <*> (x .@ "description")
                <*> (x .@ "startTime")
                <*> (x .@ "progress")
                <*> (x .@ "status")
                <*> (x .@ "encrypted")

instance NFData Snapshot where

-- | Describes the snapshot created from the imported disk.
--
--
--
-- /See:/ 'snapshotDetail' smart constructor.
data SnapshotDetail = SnapshotDetail'
  { _sdStatus        :: !(Maybe Text)
  , _sdProgress      :: !(Maybe Text)
  , _sdFormat        :: !(Maybe Text)
  , _sdURL           :: !(Maybe Text)
  , _sdDeviceName    :: !(Maybe Text)
  , _sdStatusMessage :: !(Maybe Text)
  , _sdUserBucket    :: !(Maybe UserBucketDetails)
  , _sdDiskImageSize :: !(Maybe Double)
  , _sdDescription   :: !(Maybe Text)
  , _sdSnapshotId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SnapshotDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdStatus' - A brief status of the snapshot creation.
--
-- * 'sdProgress' - The percentage of progress for the task.
--
-- * 'sdFormat' - The format of the disk image from which the snapshot is created.
--
-- * 'sdURL' - The URL used to access the disk image.
--
-- * 'sdDeviceName' - The block device mapping for the snapshot.
--
-- * 'sdStatusMessage' - A detailed status message for the snapshot creation.
--
-- * 'sdUserBucket' - The S3 bucket for the disk image.
--
-- * 'sdDiskImageSize' - The size of the disk in the snapshot, in GiB.
--
-- * 'sdDescription' - A description for the snapshot.
--
-- * 'sdSnapshotId' - The snapshot ID of the disk being imported.
snapshotDetail
    :: SnapshotDetail
snapshotDetail =
  SnapshotDetail'
    { _sdStatus = Nothing
    , _sdProgress = Nothing
    , _sdFormat = Nothing
    , _sdURL = Nothing
    , _sdDeviceName = Nothing
    , _sdStatusMessage = Nothing
    , _sdUserBucket = Nothing
    , _sdDiskImageSize = Nothing
    , _sdDescription = Nothing
    , _sdSnapshotId = Nothing
    }


-- | A brief status of the snapshot creation.
sdStatus :: Lens' SnapshotDetail (Maybe Text)
sdStatus = lens _sdStatus (\ s a -> s{_sdStatus = a})

-- | The percentage of progress for the task.
sdProgress :: Lens' SnapshotDetail (Maybe Text)
sdProgress = lens _sdProgress (\ s a -> s{_sdProgress = a})

-- | The format of the disk image from which the snapshot is created.
sdFormat :: Lens' SnapshotDetail (Maybe Text)
sdFormat = lens _sdFormat (\ s a -> s{_sdFormat = a})

-- | The URL used to access the disk image.
sdURL :: Lens' SnapshotDetail (Maybe Text)
sdURL = lens _sdURL (\ s a -> s{_sdURL = a})

-- | The block device mapping for the snapshot.
sdDeviceName :: Lens' SnapshotDetail (Maybe Text)
sdDeviceName = lens _sdDeviceName (\ s a -> s{_sdDeviceName = a})

-- | A detailed status message for the snapshot creation.
sdStatusMessage :: Lens' SnapshotDetail (Maybe Text)
sdStatusMessage = lens _sdStatusMessage (\ s a -> s{_sdStatusMessage = a})

-- | The S3 bucket for the disk image.
sdUserBucket :: Lens' SnapshotDetail (Maybe UserBucketDetails)
sdUserBucket = lens _sdUserBucket (\ s a -> s{_sdUserBucket = a})

-- | The size of the disk in the snapshot, in GiB.
sdDiskImageSize :: Lens' SnapshotDetail (Maybe Double)
sdDiskImageSize = lens _sdDiskImageSize (\ s a -> s{_sdDiskImageSize = a})

-- | A description for the snapshot.
sdDescription :: Lens' SnapshotDetail (Maybe Text)
sdDescription = lens _sdDescription (\ s a -> s{_sdDescription = a})

-- | The snapshot ID of the disk being imported.
sdSnapshotId :: Lens' SnapshotDetail (Maybe Text)
sdSnapshotId = lens _sdSnapshotId (\ s a -> s{_sdSnapshotId = a})

instance FromXML SnapshotDetail where
        parseXML x
          = SnapshotDetail' <$>
              (x .@? "status") <*> (x .@? "progress") <*>
                (x .@? "format")
                <*> (x .@? "url")
                <*> (x .@? "deviceName")
                <*> (x .@? "statusMessage")
                <*> (x .@? "userBucket")
                <*> (x .@? "diskImageSize")
                <*> (x .@? "description")
                <*> (x .@? "snapshotId")

instance NFData SnapshotDetail where

-- | The disk container object for the import snapshot request.
--
--
--
-- /See:/ 'snapshotDiskContainer' smart constructor.
data SnapshotDiskContainer = SnapshotDiskContainer'
  { _sdcFormat      :: !(Maybe Text)
  , _sdcURL         :: !(Maybe Text)
  , _sdcUserBucket  :: !(Maybe UserBucket)
  , _sdcDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SnapshotDiskContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcFormat' - The format of the disk image being imported. Valid values: @VHD@ | @VMDK@ | @OVA@
--
-- * 'sdcURL' - The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
--
-- * 'sdcUserBucket' - The S3 bucket for the disk image.
--
-- * 'sdcDescription' - The description of the disk image being imported.
snapshotDiskContainer
    :: SnapshotDiskContainer
snapshotDiskContainer =
  SnapshotDiskContainer'
    { _sdcFormat = Nothing
    , _sdcURL = Nothing
    , _sdcUserBucket = Nothing
    , _sdcDescription = Nothing
    }


-- | The format of the disk image being imported. Valid values: @VHD@ | @VMDK@ | @OVA@
sdcFormat :: Lens' SnapshotDiskContainer (Maybe Text)
sdcFormat = lens _sdcFormat (\ s a -> s{_sdcFormat = a})

-- | The URL to the Amazon S3-based disk image being imported. It can either be a https URL (https://..) or an Amazon S3 URL (s3://..).
sdcURL :: Lens' SnapshotDiskContainer (Maybe Text)
sdcURL = lens _sdcURL (\ s a -> s{_sdcURL = a})

-- | The S3 bucket for the disk image.
sdcUserBucket :: Lens' SnapshotDiskContainer (Maybe UserBucket)
sdcUserBucket = lens _sdcUserBucket (\ s a -> s{_sdcUserBucket = a})

-- | The description of the disk image being imported.
sdcDescription :: Lens' SnapshotDiskContainer (Maybe Text)
sdcDescription = lens _sdcDescription (\ s a -> s{_sdcDescription = a})

instance NFData SnapshotDiskContainer where

instance ToQuery SnapshotDiskContainer where
        toQuery SnapshotDiskContainer'{..}
          = mconcat
              ["Format" =: _sdcFormat, "Url" =: _sdcURL,
               "UserBucket" =: _sdcUserBucket,
               "Description" =: _sdcDescription]

-- | Details about the import snapshot task.
--
--
--
-- /See:/ 'snapshotTaskDetail' smart constructor.
data SnapshotTaskDetail = SnapshotTaskDetail'
  { _stdStatus        :: !(Maybe Text)
  , _stdProgress      :: !(Maybe Text)
  , _stdFormat        :: !(Maybe Text)
  , _stdURL           :: !(Maybe Text)
  , _stdStatusMessage :: !(Maybe Text)
  , _stdUserBucket    :: !(Maybe UserBucketDetails)
  , _stdDiskImageSize :: !(Maybe Double)
  , _stdDescription   :: !(Maybe Text)
  , _stdSnapshotId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SnapshotTaskDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdStatus' - A brief status for the import snapshot task.
--
-- * 'stdProgress' - The percentage of completion for the import snapshot task.
--
-- * 'stdFormat' - The format of the disk image from which the snapshot is created.
--
-- * 'stdURL' - The URL of the disk image from which the snapshot is created.
--
-- * 'stdStatusMessage' - A detailed status message for the import snapshot task.
--
-- * 'stdUserBucket' - The S3 bucket for the disk image.
--
-- * 'stdDiskImageSize' - The size of the disk in the snapshot, in GiB.
--
-- * 'stdDescription' - The description of the snapshot.
--
-- * 'stdSnapshotId' - The snapshot ID of the disk being imported.
snapshotTaskDetail
    :: SnapshotTaskDetail
snapshotTaskDetail =
  SnapshotTaskDetail'
    { _stdStatus = Nothing
    , _stdProgress = Nothing
    , _stdFormat = Nothing
    , _stdURL = Nothing
    , _stdStatusMessage = Nothing
    , _stdUserBucket = Nothing
    , _stdDiskImageSize = Nothing
    , _stdDescription = Nothing
    , _stdSnapshotId = Nothing
    }


-- | A brief status for the import snapshot task.
stdStatus :: Lens' SnapshotTaskDetail (Maybe Text)
stdStatus = lens _stdStatus (\ s a -> s{_stdStatus = a})

-- | The percentage of completion for the import snapshot task.
stdProgress :: Lens' SnapshotTaskDetail (Maybe Text)
stdProgress = lens _stdProgress (\ s a -> s{_stdProgress = a})

-- | The format of the disk image from which the snapshot is created.
stdFormat :: Lens' SnapshotTaskDetail (Maybe Text)
stdFormat = lens _stdFormat (\ s a -> s{_stdFormat = a})

-- | The URL of the disk image from which the snapshot is created.
stdURL :: Lens' SnapshotTaskDetail (Maybe Text)
stdURL = lens _stdURL (\ s a -> s{_stdURL = a})

-- | A detailed status message for the import snapshot task.
stdStatusMessage :: Lens' SnapshotTaskDetail (Maybe Text)
stdStatusMessage = lens _stdStatusMessage (\ s a -> s{_stdStatusMessage = a})

-- | The S3 bucket for the disk image.
stdUserBucket :: Lens' SnapshotTaskDetail (Maybe UserBucketDetails)
stdUserBucket = lens _stdUserBucket (\ s a -> s{_stdUserBucket = a})

-- | The size of the disk in the snapshot, in GiB.
stdDiskImageSize :: Lens' SnapshotTaskDetail (Maybe Double)
stdDiskImageSize = lens _stdDiskImageSize (\ s a -> s{_stdDiskImageSize = a})

-- | The description of the snapshot.
stdDescription :: Lens' SnapshotTaskDetail (Maybe Text)
stdDescription = lens _stdDescription (\ s a -> s{_stdDescription = a})

-- | The snapshot ID of the disk being imported.
stdSnapshotId :: Lens' SnapshotTaskDetail (Maybe Text)
stdSnapshotId = lens _stdSnapshotId (\ s a -> s{_stdSnapshotId = a})

instance FromXML SnapshotTaskDetail where
        parseXML x
          = SnapshotTaskDetail' <$>
              (x .@? "status") <*> (x .@? "progress") <*>
                (x .@? "format")
                <*> (x .@? "url")
                <*> (x .@? "statusMessage")
                <*> (x .@? "userBucket")
                <*> (x .@? "diskImageSize")
                <*> (x .@? "description")
                <*> (x .@? "snapshotId")

instance NFData SnapshotTaskDetail where

-- | Describes the data feed for a Spot Instance.
--
--
--
-- /See:/ 'spotDatafeedSubscription' smart constructor.
data SpotDatafeedSubscription = SpotDatafeedSubscription'
  { _sdsState   :: !(Maybe DatafeedSubscriptionState)
  , _sdsPrefix  :: !(Maybe Text)
  , _sdsBucket  :: !(Maybe Text)
  , _sdsOwnerId :: !(Maybe Text)
  , _sdsFault   :: !(Maybe SpotInstanceStateFault)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsState' - The state of the Spot Instance data feed subscription.
--
-- * 'sdsPrefix' - The prefix that is prepended to data feed files.
--
-- * 'sdsBucket' - The Amazon S3 bucket where the Spot Instance data feed is located.
--
-- * 'sdsOwnerId' - The AWS account ID of the account.
--
-- * 'sdsFault' - The fault codes for the Spot Instance request, if any.
spotDatafeedSubscription
    :: SpotDatafeedSubscription
spotDatafeedSubscription =
  SpotDatafeedSubscription'
    { _sdsState = Nothing
    , _sdsPrefix = Nothing
    , _sdsBucket = Nothing
    , _sdsOwnerId = Nothing
    , _sdsFault = Nothing
    }


-- | The state of the Spot Instance data feed subscription.
sdsState :: Lens' SpotDatafeedSubscription (Maybe DatafeedSubscriptionState)
sdsState = lens _sdsState (\ s a -> s{_sdsState = a})

-- | The prefix that is prepended to data feed files.
sdsPrefix :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsPrefix = lens _sdsPrefix (\ s a -> s{_sdsPrefix = a})

-- | The Amazon S3 bucket where the Spot Instance data feed is located.
sdsBucket :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsBucket = lens _sdsBucket (\ s a -> s{_sdsBucket = a})

-- | The AWS account ID of the account.
sdsOwnerId :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsOwnerId = lens _sdsOwnerId (\ s a -> s{_sdsOwnerId = a})

-- | The fault codes for the Spot Instance request, if any.
sdsFault :: Lens' SpotDatafeedSubscription (Maybe SpotInstanceStateFault)
sdsFault = lens _sdsFault (\ s a -> s{_sdsFault = a})

instance FromXML SpotDatafeedSubscription where
        parseXML x
          = SpotDatafeedSubscription' <$>
              (x .@? "state") <*> (x .@? "prefix") <*>
                (x .@? "bucket")
                <*> (x .@? "ownerId")
                <*> (x .@? "fault")

instance NFData SpotDatafeedSubscription where

-- | Describes the launch specification for one or more Spot Instances.
--
--
--
-- /See:/ 'spotFleetLaunchSpecification' smart constructor.
data SpotFleetLaunchSpecification = SpotFleetLaunchSpecification'
  { _sflsSecurityGroups      :: !(Maybe [GroupIdentifier])
  , _sflsSpotPrice           :: !(Maybe Text)
  , _sflsWeightedCapacity    :: !(Maybe Double)
  , _sflsKeyName             :: !(Maybe Text)
  , _sflsNetworkInterfaces   :: !(Maybe [InstanceNetworkInterfaceSpecification])
  , _sflsRAMDiskId           :: !(Maybe Text)
  , _sflsSubnetId            :: !(Maybe Text)
  , _sflsKernelId            :: !(Maybe Text)
  , _sflsInstanceType        :: !(Maybe InstanceType)
  , _sflsEBSOptimized        :: !(Maybe Bool)
  , _sflsUserData            :: !(Maybe Text)
  , _sflsMonitoring          :: !(Maybe SpotFleetMonitoring)
  , _sflsTagSpecifications   :: !(Maybe [SpotFleetTagSpecification])
  , _sflsIAMInstanceProfile  :: !(Maybe IAMInstanceProfileSpecification)
  , _sflsImageId             :: !(Maybe Text)
  , _sflsAddressingType      :: !(Maybe Text)
  , _sflsBlockDeviceMappings :: !(Maybe [BlockDeviceMapping])
  , _sflsPlacement           :: !(Maybe SpotPlacement)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotFleetLaunchSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sflsSecurityGroups' - One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
--
-- * 'sflsSpotPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance. If this value is not specified, the default is the Spot price specified for the fleet. To determine the Spot price per unit hour, divide the Spot price by the value of @WeightedCapacity@ .
--
-- * 'sflsWeightedCapacity' - The number of units provided by the specified instance type. These are the same units that you chose to set the target capacity in terms (instances or a performance characteristic such as vCPUs, memory, or I/O). If the target capacity divided by this value is not a whole number, we round the number of instances to the next whole number. If this value is not specified, the default is 1.
--
-- * 'sflsKeyName' - The name of the key pair.
--
-- * 'sflsNetworkInterfaces' - One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
--
-- * 'sflsRAMDiskId' - The ID of the RAM disk.
--
-- * 'sflsSubnetId' - The ID of the subnet in which to launch the instances. To specify multiple subnets, separate them using commas; for example, "subnet-a61dafcf, subnet-65ea5f08".
--
-- * 'sflsKernelId' - The ID of the kernel.
--
-- * 'sflsInstanceType' - The instance type.
--
-- * 'sflsEBSOptimized' - Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
--
-- * 'sflsUserData' - The Base64-encoded user data to make available to the instances.
--
-- * 'sflsMonitoring' - Enable or disable monitoring for the instances.
--
-- * 'sflsTagSpecifications' - The tags to apply during creation.
--
-- * 'sflsIAMInstanceProfile' - The IAM instance profile.
--
-- * 'sflsImageId' - The ID of the AMI.
--
-- * 'sflsAddressingType' - Deprecated.
--
-- * 'sflsBlockDeviceMappings' - One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
--
-- * 'sflsPlacement' - The placement information.
spotFleetLaunchSpecification
    :: SpotFleetLaunchSpecification
spotFleetLaunchSpecification =
  SpotFleetLaunchSpecification'
    { _sflsSecurityGroups = Nothing
    , _sflsSpotPrice = Nothing
    , _sflsWeightedCapacity = Nothing
    , _sflsKeyName = Nothing
    , _sflsNetworkInterfaces = Nothing
    , _sflsRAMDiskId = Nothing
    , _sflsSubnetId = Nothing
    , _sflsKernelId = Nothing
    , _sflsInstanceType = Nothing
    , _sflsEBSOptimized = Nothing
    , _sflsUserData = Nothing
    , _sflsMonitoring = Nothing
    , _sflsTagSpecifications = Nothing
    , _sflsIAMInstanceProfile = Nothing
    , _sflsImageId = Nothing
    , _sflsAddressingType = Nothing
    , _sflsBlockDeviceMappings = Nothing
    , _sflsPlacement = Nothing
    }


-- | One or more security groups. When requesting instances in a VPC, you must specify the IDs of the security groups. When requesting instances in EC2-Classic, you can specify the names or the IDs of the security groups.
sflsSecurityGroups :: Lens' SpotFleetLaunchSpecification [GroupIdentifier]
sflsSecurityGroups = lens _sflsSecurityGroups (\ s a -> s{_sflsSecurityGroups = a}) . _Default . _Coerce

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. If this value is not specified, the default is the Spot price specified for the fleet. To determine the Spot price per unit hour, divide the Spot price by the value of @WeightedCapacity@ .
sflsSpotPrice :: Lens' SpotFleetLaunchSpecification (Maybe Text)
sflsSpotPrice = lens _sflsSpotPrice (\ s a -> s{_sflsSpotPrice = a})

-- | The number of units provided by the specified instance type. These are the same units that you chose to set the target capacity in terms (instances or a performance characteristic such as vCPUs, memory, or I/O). If the target capacity divided by this value is not a whole number, we round the number of instances to the next whole number. If this value is not specified, the default is 1.
sflsWeightedCapacity :: Lens' SpotFleetLaunchSpecification (Maybe Double)
sflsWeightedCapacity = lens _sflsWeightedCapacity (\ s a -> s{_sflsWeightedCapacity = a})

-- | The name of the key pair.
sflsKeyName :: Lens' SpotFleetLaunchSpecification (Maybe Text)
sflsKeyName = lens _sflsKeyName (\ s a -> s{_sflsKeyName = a})

-- | One or more network interfaces. If you specify a network interface, you must specify subnet IDs and security group IDs using the network interface.
sflsNetworkInterfaces :: Lens' SpotFleetLaunchSpecification [InstanceNetworkInterfaceSpecification]
sflsNetworkInterfaces = lens _sflsNetworkInterfaces (\ s a -> s{_sflsNetworkInterfaces = a}) . _Default . _Coerce

-- | The ID of the RAM disk.
sflsRAMDiskId :: Lens' SpotFleetLaunchSpecification (Maybe Text)
sflsRAMDiskId = lens _sflsRAMDiskId (\ s a -> s{_sflsRAMDiskId = a})

-- | The ID of the subnet in which to launch the instances. To specify multiple subnets, separate them using commas; for example, "subnet-a61dafcf, subnet-65ea5f08".
sflsSubnetId :: Lens' SpotFleetLaunchSpecification (Maybe Text)
sflsSubnetId = lens _sflsSubnetId (\ s a -> s{_sflsSubnetId = a})

-- | The ID of the kernel.
sflsKernelId :: Lens' SpotFleetLaunchSpecification (Maybe Text)
sflsKernelId = lens _sflsKernelId (\ s a -> s{_sflsKernelId = a})

-- | The instance type.
sflsInstanceType :: Lens' SpotFleetLaunchSpecification (Maybe InstanceType)
sflsInstanceType = lens _sflsInstanceType (\ s a -> s{_sflsInstanceType = a})

-- | Indicates whether the instances are optimized for EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance. Default: @false@
sflsEBSOptimized :: Lens' SpotFleetLaunchSpecification (Maybe Bool)
sflsEBSOptimized = lens _sflsEBSOptimized (\ s a -> s{_sflsEBSOptimized = a})

-- | The Base64-encoded user data to make available to the instances.
sflsUserData :: Lens' SpotFleetLaunchSpecification (Maybe Text)
sflsUserData = lens _sflsUserData (\ s a -> s{_sflsUserData = a})

-- | Enable or disable monitoring for the instances.
sflsMonitoring :: Lens' SpotFleetLaunchSpecification (Maybe SpotFleetMonitoring)
sflsMonitoring = lens _sflsMonitoring (\ s a -> s{_sflsMonitoring = a})

-- | The tags to apply during creation.
sflsTagSpecifications :: Lens' SpotFleetLaunchSpecification [SpotFleetTagSpecification]
sflsTagSpecifications = lens _sflsTagSpecifications (\ s a -> s{_sflsTagSpecifications = a}) . _Default . _Coerce

-- | The IAM instance profile.
sflsIAMInstanceProfile :: Lens' SpotFleetLaunchSpecification (Maybe IAMInstanceProfileSpecification)
sflsIAMInstanceProfile = lens _sflsIAMInstanceProfile (\ s a -> s{_sflsIAMInstanceProfile = a})

-- | The ID of the AMI.
sflsImageId :: Lens' SpotFleetLaunchSpecification (Maybe Text)
sflsImageId = lens _sflsImageId (\ s a -> s{_sflsImageId = a})

-- | Deprecated.
sflsAddressingType :: Lens' SpotFleetLaunchSpecification (Maybe Text)
sflsAddressingType = lens _sflsAddressingType (\ s a -> s{_sflsAddressingType = a})

-- | One or more block device mapping entries. You can't specify both a snapshot ID and an encryption value. This is because only blank volumes can be encrypted on creation. If a snapshot is the basis for a volume, it is not blank and its encryption status is used for the volume encryption status.
sflsBlockDeviceMappings :: Lens' SpotFleetLaunchSpecification [BlockDeviceMapping]
sflsBlockDeviceMappings = lens _sflsBlockDeviceMappings (\ s a -> s{_sflsBlockDeviceMappings = a}) . _Default . _Coerce

-- | The placement information.
sflsPlacement :: Lens' SpotFleetLaunchSpecification (Maybe SpotPlacement)
sflsPlacement = lens _sflsPlacement (\ s a -> s{_sflsPlacement = a})

instance FromXML SpotFleetLaunchSpecification where
        parseXML x
          = SpotFleetLaunchSpecification' <$>
              (x .@? "groupSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "spotPrice")
                <*> (x .@? "weightedCapacity")
                <*> (x .@? "keyName")
                <*>
                (x .@? "networkInterfaceSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "ramdiskId")
                <*> (x .@? "subnetId")
                <*> (x .@? "kernelId")
                <*> (x .@? "instanceType")
                <*> (x .@? "ebsOptimized")
                <*> (x .@? "userData")
                <*> (x .@? "monitoring")
                <*>
                (x .@? "tagSpecificationSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "iamInstanceProfile")
                <*> (x .@? "imageId")
                <*> (x .@? "addressingType")
                <*>
                (x .@? "blockDeviceMapping" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "placement")

instance NFData SpotFleetLaunchSpecification where

instance ToQuery SpotFleetLaunchSpecification where
        toQuery SpotFleetLaunchSpecification'{..}
          = mconcat
              [toQuery
                 (toQueryList "GroupSet" <$> _sflsSecurityGroups),
               "SpotPrice" =: _sflsSpotPrice,
               "WeightedCapacity" =: _sflsWeightedCapacity,
               "KeyName" =: _sflsKeyName,
               toQuery
                 (toQueryList "NetworkInterfaceSet" <$>
                    _sflsNetworkInterfaces),
               "RamdiskId" =: _sflsRAMDiskId,
               "SubnetId" =: _sflsSubnetId,
               "KernelId" =: _sflsKernelId,
               "InstanceType" =: _sflsInstanceType,
               "EbsOptimized" =: _sflsEBSOptimized,
               "UserData" =: _sflsUserData,
               "Monitoring" =: _sflsMonitoring,
               toQuery
                 (toQueryList "TagSpecificationSet" <$>
                    _sflsTagSpecifications),
               "IamInstanceProfile" =: _sflsIAMInstanceProfile,
               "ImageId" =: _sflsImageId,
               "AddressingType" =: _sflsAddressingType,
               toQuery
                 (toQueryList "BlockDeviceMapping" <$>
                    _sflsBlockDeviceMappings),
               "Placement" =: _sflsPlacement]

-- | Describes whether monitoring is enabled.
--
--
--
-- /See:/ 'spotFleetMonitoring' smart constructor.
newtype SpotFleetMonitoring = SpotFleetMonitoring'
  { _sfmEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotFleetMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfmEnabled' - Enables monitoring for the instance. Default: @false@
spotFleetMonitoring
    :: SpotFleetMonitoring
spotFleetMonitoring = SpotFleetMonitoring' {_sfmEnabled = Nothing}


-- | Enables monitoring for the instance. Default: @false@
sfmEnabled :: Lens' SpotFleetMonitoring (Maybe Bool)
sfmEnabled = lens _sfmEnabled (\ s a -> s{_sfmEnabled = a})

instance FromXML SpotFleetMonitoring where
        parseXML x
          = SpotFleetMonitoring' <$> (x .@? "enabled")

instance NFData SpotFleetMonitoring where

instance ToQuery SpotFleetMonitoring where
        toQuery SpotFleetMonitoring'{..}
          = mconcat ["Enabled" =: _sfmEnabled]

-- | Describes a Spot Fleet request.
--
--
--
-- /See:/ 'spotFleetRequestConfig' smart constructor.
data SpotFleetRequestConfig = SpotFleetRequestConfig'
  { _sfrcActivityStatus         :: !(Maybe ActivityStatus)
  , _sfrcCreateTime             :: !ISO8601
  , _sfrcSpotFleetRequestConfig :: !SpotFleetRequestConfigData
  , _sfrcSpotFleetRequestId     :: !Text
  , _sfrcSpotFleetRequestState  :: !BatchState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotFleetRequestConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfrcActivityStatus' - The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
--
-- * 'sfrcCreateTime' - The creation date and time of the request.
--
-- * 'sfrcSpotFleetRequestConfig' - The configuration of the Spot Fleet request.
--
-- * 'sfrcSpotFleetRequestId' - The ID of the Spot Fleet request.
--
-- * 'sfrcSpotFleetRequestState' - The state of the Spot Fleet request.
spotFleetRequestConfig
    :: UTCTime -- ^ 'sfrcCreateTime'
    -> SpotFleetRequestConfigData -- ^ 'sfrcSpotFleetRequestConfig'
    -> Text -- ^ 'sfrcSpotFleetRequestId'
    -> BatchState -- ^ 'sfrcSpotFleetRequestState'
    -> SpotFleetRequestConfig
spotFleetRequestConfig pCreateTime_ pSpotFleetRequestConfig_ pSpotFleetRequestId_ pSpotFleetRequestState_ =
  SpotFleetRequestConfig'
    { _sfrcActivityStatus = Nothing
    , _sfrcCreateTime = _Time # pCreateTime_
    , _sfrcSpotFleetRequestConfig = pSpotFleetRequestConfig_
    , _sfrcSpotFleetRequestId = pSpotFleetRequestId_
    , _sfrcSpotFleetRequestState = pSpotFleetRequestState_
    }


-- | The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
sfrcActivityStatus :: Lens' SpotFleetRequestConfig (Maybe ActivityStatus)
sfrcActivityStatus = lens _sfrcActivityStatus (\ s a -> s{_sfrcActivityStatus = a})

-- | The creation date and time of the request.
sfrcCreateTime :: Lens' SpotFleetRequestConfig UTCTime
sfrcCreateTime = lens _sfrcCreateTime (\ s a -> s{_sfrcCreateTime = a}) . _Time

-- | The configuration of the Spot Fleet request.
sfrcSpotFleetRequestConfig :: Lens' SpotFleetRequestConfig SpotFleetRequestConfigData
sfrcSpotFleetRequestConfig = lens _sfrcSpotFleetRequestConfig (\ s a -> s{_sfrcSpotFleetRequestConfig = a})

-- | The ID of the Spot Fleet request.
sfrcSpotFleetRequestId :: Lens' SpotFleetRequestConfig Text
sfrcSpotFleetRequestId = lens _sfrcSpotFleetRequestId (\ s a -> s{_sfrcSpotFleetRequestId = a})

-- | The state of the Spot Fleet request.
sfrcSpotFleetRequestState :: Lens' SpotFleetRequestConfig BatchState
sfrcSpotFleetRequestState = lens _sfrcSpotFleetRequestState (\ s a -> s{_sfrcSpotFleetRequestState = a})

instance FromXML SpotFleetRequestConfig where
        parseXML x
          = SpotFleetRequestConfig' <$>
              (x .@? "activityStatus") <*> (x .@ "createTime") <*>
                (x .@ "spotFleetRequestConfig")
                <*> (x .@ "spotFleetRequestId")
                <*> (x .@ "spotFleetRequestState")

instance NFData SpotFleetRequestConfig where

-- | Describes the configuration of a Spot Fleet request.
--
--
--
-- /See:/ 'spotFleetRequestConfigData' smart constructor.
data SpotFleetRequestConfigData = SpotFleetRequestConfigData'
  { _sfrcdClientToken :: !(Maybe Text)
  , _sfrcdInstanceInterruptionBehavior :: !(Maybe InstanceInterruptionBehavior)
  , _sfrcdSpotPrice :: !(Maybe Text)
  , _sfrcdLoadBalancersConfig :: !(Maybe LoadBalancersConfig)
  , _sfrcdExcessCapacityTerminationPolicy :: !(Maybe ExcessCapacityTerminationPolicy)
  , _sfrcdOnDemandTargetCapacity :: !(Maybe Int)
  , _sfrcdLaunchTemplateConfigs :: !(Maybe [LaunchTemplateConfig])
  , _sfrcdValidUntil :: !(Maybe ISO8601)
  , _sfrcdTerminateInstancesWithExpiration :: !(Maybe Bool)
  , _sfrcdFulfilledCapacity :: !(Maybe Double)
  , _sfrcdType :: !(Maybe FleetType)
  , _sfrcdValidFrom :: !(Maybe ISO8601)
  , _sfrcdReplaceUnhealthyInstances :: !(Maybe Bool)
  , _sfrcdLaunchSpecifications :: !(Maybe [SpotFleetLaunchSpecification])
  , _sfrcdOnDemandFulfilledCapacity :: !(Maybe Double)
  , _sfrcdAllocationStrategy :: !(Maybe AllocationStrategy)
  , _sfrcdIAMFleetRole :: !Text
  , _sfrcdTargetCapacity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotFleetRequestConfigData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfrcdClientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of your listings. This helps to avoid duplicate listings. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'sfrcdInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'sfrcdSpotPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
--
-- * 'sfrcdLoadBalancersConfig' - One or more Classic Load Balancers and target groups to attach to the Spot Fleet request. Spot Fleet registers the running Spot Instances with the specified Classic Load Balancers and target groups. With Network Load Balancers, Spot Fleet cannot register instances that have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1.
--
-- * 'sfrcdExcessCapacityTerminationPolicy' - Indicates whether running Spot Instances should be terminated if the target capacity of the Spot Fleet request is decreased below the current size of the Spot Fleet.
--
-- * 'sfrcdOnDemandTargetCapacity' - The number of On-Demand units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- * 'sfrcdLaunchTemplateConfigs' - The launch template and overrides.
--
-- * 'sfrcdValidUntil' - The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new Spot Instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
--
-- * 'sfrcdTerminateInstancesWithExpiration' - Indicates whether running Spot Instances should be terminated when the Spot Fleet request expires.
--
-- * 'sfrcdFulfilledCapacity' - The number of units fulfilled by this request compared to the set target capacity.
--
-- * 'sfrcdType' - The type of request. Indicates whether the Spot Fleet only requests the target capacity or also attempts to maintain it. When this value is @request@ , the Spot Fleet only places the required requests. It does not attempt to replenish Spot Instances if capacity is diminished, nor does it submit requests in alternative Spot pools if capacity is not available. To maintain a certain target capacity, the Spot Fleet places the required requests to meet capacity and automatically replenishes any interrupted instances. Default: @maintain@ .
--
-- * 'sfrcdValidFrom' - The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
--
-- * 'sfrcdReplaceUnhealthyInstances' - Indicates whether Spot Fleet should replace unhealthy instances.
--
-- * 'sfrcdLaunchSpecifications' - The launch specifications for the Spot Fleet request.
--
-- * 'sfrcdOnDemandFulfilledCapacity' - The number of On-Demand units fulfilled by this request compared to the set target On-Demand capacity.
--
-- * 'sfrcdAllocationStrategy' - Indicates how to allocate the target capacity across the Spot pools specified by the Spot Fleet request. The default is @lowestPrice@ .
--
-- * 'sfrcdIAMFleetRole' - Grants the Spot Fleet permission to terminate Spot Instances on your behalf when you cancel its Spot Fleet request using 'CancelSpotFleetRequests' or when the Spot Fleet request expires, if you set @terminateInstancesWithExpiration@ .
--
-- * 'sfrcdTargetCapacity' - The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
spotFleetRequestConfigData
    :: Text -- ^ 'sfrcdIAMFleetRole'
    -> Int -- ^ 'sfrcdTargetCapacity'
    -> SpotFleetRequestConfigData
spotFleetRequestConfigData pIAMFleetRole_ pTargetCapacity_ =
  SpotFleetRequestConfigData'
    { _sfrcdClientToken = Nothing
    , _sfrcdInstanceInterruptionBehavior = Nothing
    , _sfrcdSpotPrice = Nothing
    , _sfrcdLoadBalancersConfig = Nothing
    , _sfrcdExcessCapacityTerminationPolicy = Nothing
    , _sfrcdOnDemandTargetCapacity = Nothing
    , _sfrcdLaunchTemplateConfigs = Nothing
    , _sfrcdValidUntil = Nothing
    , _sfrcdTerminateInstancesWithExpiration = Nothing
    , _sfrcdFulfilledCapacity = Nothing
    , _sfrcdType = Nothing
    , _sfrcdValidFrom = Nothing
    , _sfrcdReplaceUnhealthyInstances = Nothing
    , _sfrcdLaunchSpecifications = Nothing
    , _sfrcdOnDemandFulfilledCapacity = Nothing
    , _sfrcdAllocationStrategy = Nothing
    , _sfrcdIAMFleetRole = pIAMFleetRole_
    , _sfrcdTargetCapacity = pTargetCapacity_
    }


-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of your listings. This helps to avoid duplicate listings. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
sfrcdClientToken :: Lens' SpotFleetRequestConfigData (Maybe Text)
sfrcdClientToken = lens _sfrcdClientToken (\ s a -> s{_sfrcdClientToken = a})

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
sfrcdInstanceInterruptionBehavior :: Lens' SpotFleetRequestConfigData (Maybe InstanceInterruptionBehavior)
sfrcdInstanceInterruptionBehavior = lens _sfrcdInstanceInterruptionBehavior (\ s a -> s{_sfrcdInstanceInterruptionBehavior = a})

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
sfrcdSpotPrice :: Lens' SpotFleetRequestConfigData (Maybe Text)
sfrcdSpotPrice = lens _sfrcdSpotPrice (\ s a -> s{_sfrcdSpotPrice = a})

-- | One or more Classic Load Balancers and target groups to attach to the Spot Fleet request. Spot Fleet registers the running Spot Instances with the specified Classic Load Balancers and target groups. With Network Load Balancers, Spot Fleet cannot register instances that have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1.
sfrcdLoadBalancersConfig :: Lens' SpotFleetRequestConfigData (Maybe LoadBalancersConfig)
sfrcdLoadBalancersConfig = lens _sfrcdLoadBalancersConfig (\ s a -> s{_sfrcdLoadBalancersConfig = a})

-- | Indicates whether running Spot Instances should be terminated if the target capacity of the Spot Fleet request is decreased below the current size of the Spot Fleet.
sfrcdExcessCapacityTerminationPolicy :: Lens' SpotFleetRequestConfigData (Maybe ExcessCapacityTerminationPolicy)
sfrcdExcessCapacityTerminationPolicy = lens _sfrcdExcessCapacityTerminationPolicy (\ s a -> s{_sfrcdExcessCapacityTerminationPolicy = a})

-- | The number of On-Demand units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
sfrcdOnDemandTargetCapacity :: Lens' SpotFleetRequestConfigData (Maybe Int)
sfrcdOnDemandTargetCapacity = lens _sfrcdOnDemandTargetCapacity (\ s a -> s{_sfrcdOnDemandTargetCapacity = a})

-- | The launch template and overrides.
sfrcdLaunchTemplateConfigs :: Lens' SpotFleetRequestConfigData [LaunchTemplateConfig]
sfrcdLaunchTemplateConfigs = lens _sfrcdLaunchTemplateConfigs (\ s a -> s{_sfrcdLaunchTemplateConfigs = a}) . _Default . _Coerce

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new Spot Instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
sfrcdValidUntil :: Lens' SpotFleetRequestConfigData (Maybe UTCTime)
sfrcdValidUntil = lens _sfrcdValidUntil (\ s a -> s{_sfrcdValidUntil = a}) . mapping _Time

-- | Indicates whether running Spot Instances should be terminated when the Spot Fleet request expires.
sfrcdTerminateInstancesWithExpiration :: Lens' SpotFleetRequestConfigData (Maybe Bool)
sfrcdTerminateInstancesWithExpiration = lens _sfrcdTerminateInstancesWithExpiration (\ s a -> s{_sfrcdTerminateInstancesWithExpiration = a})

-- | The number of units fulfilled by this request compared to the set target capacity.
sfrcdFulfilledCapacity :: Lens' SpotFleetRequestConfigData (Maybe Double)
sfrcdFulfilledCapacity = lens _sfrcdFulfilledCapacity (\ s a -> s{_sfrcdFulfilledCapacity = a})

-- | The type of request. Indicates whether the Spot Fleet only requests the target capacity or also attempts to maintain it. When this value is @request@ , the Spot Fleet only places the required requests. It does not attempt to replenish Spot Instances if capacity is diminished, nor does it submit requests in alternative Spot pools if capacity is not available. To maintain a certain target capacity, the Spot Fleet places the required requests to meet capacity and automatically replenishes any interrupted instances. Default: @maintain@ .
sfrcdType :: Lens' SpotFleetRequestConfigData (Maybe FleetType)
sfrcdType = lens _sfrcdType (\ s a -> s{_sfrcdType = a})

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
sfrcdValidFrom :: Lens' SpotFleetRequestConfigData (Maybe UTCTime)
sfrcdValidFrom = lens _sfrcdValidFrom (\ s a -> s{_sfrcdValidFrom = a}) . mapping _Time

-- | Indicates whether Spot Fleet should replace unhealthy instances.
sfrcdReplaceUnhealthyInstances :: Lens' SpotFleetRequestConfigData (Maybe Bool)
sfrcdReplaceUnhealthyInstances = lens _sfrcdReplaceUnhealthyInstances (\ s a -> s{_sfrcdReplaceUnhealthyInstances = a})

-- | The launch specifications for the Spot Fleet request.
sfrcdLaunchSpecifications :: Lens' SpotFleetRequestConfigData [SpotFleetLaunchSpecification]
sfrcdLaunchSpecifications = lens _sfrcdLaunchSpecifications (\ s a -> s{_sfrcdLaunchSpecifications = a}) . _Default . _Coerce

-- | The number of On-Demand units fulfilled by this request compared to the set target On-Demand capacity.
sfrcdOnDemandFulfilledCapacity :: Lens' SpotFleetRequestConfigData (Maybe Double)
sfrcdOnDemandFulfilledCapacity = lens _sfrcdOnDemandFulfilledCapacity (\ s a -> s{_sfrcdOnDemandFulfilledCapacity = a})

-- | Indicates how to allocate the target capacity across the Spot pools specified by the Spot Fleet request. The default is @lowestPrice@ .
sfrcdAllocationStrategy :: Lens' SpotFleetRequestConfigData (Maybe AllocationStrategy)
sfrcdAllocationStrategy = lens _sfrcdAllocationStrategy (\ s a -> s{_sfrcdAllocationStrategy = a})

-- | Grants the Spot Fleet permission to terminate Spot Instances on your behalf when you cancel its Spot Fleet request using 'CancelSpotFleetRequests' or when the Spot Fleet request expires, if you set @terminateInstancesWithExpiration@ .
sfrcdIAMFleetRole :: Lens' SpotFleetRequestConfigData Text
sfrcdIAMFleetRole = lens _sfrcdIAMFleetRole (\ s a -> s{_sfrcdIAMFleetRole = a})

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
sfrcdTargetCapacity :: Lens' SpotFleetRequestConfigData Int
sfrcdTargetCapacity = lens _sfrcdTargetCapacity (\ s a -> s{_sfrcdTargetCapacity = a})

instance FromXML SpotFleetRequestConfigData where
        parseXML x
          = SpotFleetRequestConfigData' <$>
              (x .@? "clientToken") <*>
                (x .@? "instanceInterruptionBehavior")
                <*> (x .@? "spotPrice")
                <*> (x .@? "loadBalancersConfig")
                <*> (x .@? "excessCapacityTerminationPolicy")
                <*> (x .@? "onDemandTargetCapacity")
                <*>
                (x .@? "launchTemplateConfigs" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "validUntil")
                <*> (x .@? "terminateInstancesWithExpiration")
                <*> (x .@? "fulfilledCapacity")
                <*> (x .@? "type")
                <*> (x .@? "validFrom")
                <*> (x .@? "replaceUnhealthyInstances")
                <*>
                (x .@? "launchSpecifications" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "onDemandFulfilledCapacity")
                <*> (x .@? "allocationStrategy")
                <*> (x .@ "iamFleetRole")
                <*> (x .@ "targetCapacity")

instance NFData SpotFleetRequestConfigData where

instance ToQuery SpotFleetRequestConfigData where
        toQuery SpotFleetRequestConfigData'{..}
          = mconcat
              ["ClientToken" =: _sfrcdClientToken,
               "InstanceInterruptionBehavior" =:
                 _sfrcdInstanceInterruptionBehavior,
               "SpotPrice" =: _sfrcdSpotPrice,
               "LoadBalancersConfig" =: _sfrcdLoadBalancersConfig,
               "ExcessCapacityTerminationPolicy" =:
                 _sfrcdExcessCapacityTerminationPolicy,
               "OnDemandTargetCapacity" =:
                 _sfrcdOnDemandTargetCapacity,
               toQuery
                 (toQueryList "LaunchTemplateConfigs" <$>
                    _sfrcdLaunchTemplateConfigs),
               "ValidUntil" =: _sfrcdValidUntil,
               "TerminateInstancesWithExpiration" =:
                 _sfrcdTerminateInstancesWithExpiration,
               "FulfilledCapacity" =: _sfrcdFulfilledCapacity,
               "Type" =: _sfrcdType, "ValidFrom" =: _sfrcdValidFrom,
               "ReplaceUnhealthyInstances" =:
                 _sfrcdReplaceUnhealthyInstances,
               toQuery
                 (toQueryList "LaunchSpecifications" <$>
                    _sfrcdLaunchSpecifications),
               "OnDemandFulfilledCapacity" =:
                 _sfrcdOnDemandFulfilledCapacity,
               "AllocationStrategy" =: _sfrcdAllocationStrategy,
               "IamFleetRole" =: _sfrcdIAMFleetRole,
               "TargetCapacity" =: _sfrcdTargetCapacity]

-- | The tags for a Spot Fleet resource.
--
--
--
-- /See:/ 'spotFleetTagSpecification' smart constructor.
data SpotFleetTagSpecification = SpotFleetTagSpecification'
  { _sftsResourceType :: !(Maybe ResourceType)
  , _sftsTags         :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotFleetTagSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sftsResourceType' - The type of resource. Currently, the only resource type that is supported is @instance@ .
--
-- * 'sftsTags' - The tags.
spotFleetTagSpecification
    :: SpotFleetTagSpecification
spotFleetTagSpecification =
  SpotFleetTagSpecification' {_sftsResourceType = Nothing, _sftsTags = Nothing}


-- | The type of resource. Currently, the only resource type that is supported is @instance@ .
sftsResourceType :: Lens' SpotFleetTagSpecification (Maybe ResourceType)
sftsResourceType = lens _sftsResourceType (\ s a -> s{_sftsResourceType = a})

-- | The tags.
sftsTags :: Lens' SpotFleetTagSpecification [Tag]
sftsTags = lens _sftsTags (\ s a -> s{_sftsTags = a}) . _Default . _Coerce

instance FromXML SpotFleetTagSpecification where
        parseXML x
          = SpotFleetTagSpecification' <$>
              (x .@? "resourceType") <*>
                (x .@? "tag" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData SpotFleetTagSpecification where

instance ToQuery SpotFleetTagSpecification where
        toQuery SpotFleetTagSpecification'{..}
          = mconcat
              ["ResourceType" =: _sftsResourceType,
               toQuery (toQueryList "Tag" <$> _sftsTags)]

-- | Describes a Spot Instance request.
--
--
--
-- /See:/ 'spotInstanceRequest' smart constructor.
data SpotInstanceRequest = SpotInstanceRequest'
  { _sirInstanceId                   :: !(Maybe Text)
  , _sirStatus                       :: !(Maybe SpotInstanceStatus)
  , _sirState                        :: !(Maybe SpotInstanceState)
  , _sirActualBlockHourlyPrice       :: !(Maybe Text)
  , _sirBlockDurationMinutes         :: !(Maybe Int)
  , _sirInstanceInterruptionBehavior :: !(Maybe InstanceInterruptionBehavior)
  , _sirProductDescription           :: !(Maybe RIProductDescription)
  , _sirSpotPrice                    :: !(Maybe Text)
  , _sirLaunchSpecification          :: !(Maybe LaunchSpecification)
  , _sirAvailabilityZoneGroup        :: !(Maybe Text)
  , _sirLaunchedAvailabilityZone     :: !(Maybe Text)
  , _sirValidUntil                   :: !(Maybe ISO8601)
  , _sirLaunchGroup                  :: !(Maybe Text)
  , _sirFault                        :: !(Maybe SpotInstanceStateFault)
  , _sirSpotInstanceRequestId        :: !(Maybe Text)
  , _sirType                         :: !(Maybe SpotInstanceType)
  , _sirValidFrom                    :: !(Maybe ISO8601)
  , _sirCreateTime                   :: !(Maybe ISO8601)
  , _sirTags                         :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotInstanceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirInstanceId' - The instance ID, if an instance has been launched to fulfill the Spot Instance request.
--
-- * 'sirStatus' - The status code and status message describing the Spot Instance request.
--
-- * 'sirState' - The state of the Spot Instance request. Spot status information helps track your Spot Instance requests. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot Status> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'sirActualBlockHourlyPrice' - If you specified a duration and your Spot Instance request was fulfilled, this is the fixed hourly price in effect for the Spot Instance while it runs.
--
-- * 'sirBlockDurationMinutes' - The duration for the Spot Instance, in minutes.
--
-- * 'sirInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- * 'sirProductDescription' - The product description associated with the Spot Instance.
--
-- * 'sirSpotPrice' - The maximum price per hour that you are willing to pay for a Spot Instance.
--
-- * 'sirLaunchSpecification' - Additional information for launching instances.
--
-- * 'sirAvailabilityZoneGroup' - The Availability Zone group. If you specify the same Availability Zone group for all Spot Instance requests, all Spot Instances are launched in the same Availability Zone.
--
-- * 'sirLaunchedAvailabilityZone' - The Availability Zone in which the request is launched.
--
-- * 'sirValidUntil' - The end date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). If this is a one-time request, it remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date is reached. The default end date is 7 days from the current date.
--
-- * 'sirLaunchGroup' - The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
--
-- * 'sirFault' - The fault codes for the Spot Instance request, if any.
--
-- * 'sirSpotInstanceRequestId' - The ID of the Spot Instance request.
--
-- * 'sirType' - The Spot Instance request type.
--
-- * 'sirValidFrom' - The start date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The request becomes active at this date and time.
--
-- * 'sirCreateTime' - The date and time when the Spot Instance request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- * 'sirTags' - Any tags assigned to the resource.
spotInstanceRequest
    :: SpotInstanceRequest
spotInstanceRequest =
  SpotInstanceRequest'
    { _sirInstanceId = Nothing
    , _sirStatus = Nothing
    , _sirState = Nothing
    , _sirActualBlockHourlyPrice = Nothing
    , _sirBlockDurationMinutes = Nothing
    , _sirInstanceInterruptionBehavior = Nothing
    , _sirProductDescription = Nothing
    , _sirSpotPrice = Nothing
    , _sirLaunchSpecification = Nothing
    , _sirAvailabilityZoneGroup = Nothing
    , _sirLaunchedAvailabilityZone = Nothing
    , _sirValidUntil = Nothing
    , _sirLaunchGroup = Nothing
    , _sirFault = Nothing
    , _sirSpotInstanceRequestId = Nothing
    , _sirType = Nothing
    , _sirValidFrom = Nothing
    , _sirCreateTime = Nothing
    , _sirTags = Nothing
    }


-- | The instance ID, if an instance has been launched to fulfill the Spot Instance request.
sirInstanceId :: Lens' SpotInstanceRequest (Maybe Text)
sirInstanceId = lens _sirInstanceId (\ s a -> s{_sirInstanceId = a})

-- | The status code and status message describing the Spot Instance request.
sirStatus :: Lens' SpotInstanceRequest (Maybe SpotInstanceStatus)
sirStatus = lens _sirStatus (\ s a -> s{_sirStatus = a})

-- | The state of the Spot Instance request. Spot status information helps track your Spot Instance requests. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot Status> in the /Amazon EC2 User Guide for Linux Instances/ .
sirState :: Lens' SpotInstanceRequest (Maybe SpotInstanceState)
sirState = lens _sirState (\ s a -> s{_sirState = a})

-- | If you specified a duration and your Spot Instance request was fulfilled, this is the fixed hourly price in effect for the Spot Instance while it runs.
sirActualBlockHourlyPrice :: Lens' SpotInstanceRequest (Maybe Text)
sirActualBlockHourlyPrice = lens _sirActualBlockHourlyPrice (\ s a -> s{_sirActualBlockHourlyPrice = a})

-- | The duration for the Spot Instance, in minutes.
sirBlockDurationMinutes :: Lens' SpotInstanceRequest (Maybe Int)
sirBlockDurationMinutes = lens _sirBlockDurationMinutes (\ s a -> s{_sirBlockDurationMinutes = a})

-- | The behavior when a Spot Instance is interrupted.
sirInstanceInterruptionBehavior :: Lens' SpotInstanceRequest (Maybe InstanceInterruptionBehavior)
sirInstanceInterruptionBehavior = lens _sirInstanceInterruptionBehavior (\ s a -> s{_sirInstanceInterruptionBehavior = a})

-- | The product description associated with the Spot Instance.
sirProductDescription :: Lens' SpotInstanceRequest (Maybe RIProductDescription)
sirProductDescription = lens _sirProductDescription (\ s a -> s{_sirProductDescription = a})

-- | The maximum price per hour that you are willing to pay for a Spot Instance.
sirSpotPrice :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotPrice = lens _sirSpotPrice (\ s a -> s{_sirSpotPrice = a})

-- | Additional information for launching instances.
sirLaunchSpecification :: Lens' SpotInstanceRequest (Maybe LaunchSpecification)
sirLaunchSpecification = lens _sirLaunchSpecification (\ s a -> s{_sirLaunchSpecification = a})

-- | The Availability Zone group. If you specify the same Availability Zone group for all Spot Instance requests, all Spot Instances are launched in the same Availability Zone.
sirAvailabilityZoneGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirAvailabilityZoneGroup = lens _sirAvailabilityZoneGroup (\ s a -> s{_sirAvailabilityZoneGroup = a})

-- | The Availability Zone in which the request is launched.
sirLaunchedAvailabilityZone :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchedAvailabilityZone = lens _sirLaunchedAvailabilityZone (\ s a -> s{_sirLaunchedAvailabilityZone = a})

-- | The end date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). If this is a one-time request, it remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date is reached. The default end date is 7 days from the current date.
sirValidUntil :: Lens' SpotInstanceRequest (Maybe UTCTime)
sirValidUntil = lens _sirValidUntil (\ s a -> s{_sirValidUntil = a}) . mapping _Time

-- | The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
sirLaunchGroup :: Lens' SpotInstanceRequest (Maybe Text)
sirLaunchGroup = lens _sirLaunchGroup (\ s a -> s{_sirLaunchGroup = a})

-- | The fault codes for the Spot Instance request, if any.
sirFault :: Lens' SpotInstanceRequest (Maybe SpotInstanceStateFault)
sirFault = lens _sirFault (\ s a -> s{_sirFault = a})

-- | The ID of the Spot Instance request.
sirSpotInstanceRequestId :: Lens' SpotInstanceRequest (Maybe Text)
sirSpotInstanceRequestId = lens _sirSpotInstanceRequestId (\ s a -> s{_sirSpotInstanceRequestId = a})

-- | The Spot Instance request type.
sirType :: Lens' SpotInstanceRequest (Maybe SpotInstanceType)
sirType = lens _sirType (\ s a -> s{_sirType = a})

-- | The start date of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The request becomes active at this date and time.
sirValidFrom :: Lens' SpotInstanceRequest (Maybe UTCTime)
sirValidFrom = lens _sirValidFrom (\ s a -> s{_sirValidFrom = a}) . mapping _Time

-- | The date and time when the Spot Instance request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
sirCreateTime :: Lens' SpotInstanceRequest (Maybe UTCTime)
sirCreateTime = lens _sirCreateTime (\ s a -> s{_sirCreateTime = a}) . mapping _Time

-- | Any tags assigned to the resource.
sirTags :: Lens' SpotInstanceRequest [Tag]
sirTags = lens _sirTags (\ s a -> s{_sirTags = a}) . _Default . _Coerce

instance FromXML SpotInstanceRequest where
        parseXML x
          = SpotInstanceRequest' <$>
              (x .@? "instanceId") <*> (x .@? "status") <*>
                (x .@? "state")
                <*> (x .@? "actualBlockHourlyPrice")
                <*> (x .@? "blockDurationMinutes")
                <*> (x .@? "instanceInterruptionBehavior")
                <*> (x .@? "productDescription")
                <*> (x .@? "spotPrice")
                <*> (x .@? "launchSpecification")
                <*> (x .@? "availabilityZoneGroup")
                <*> (x .@? "launchedAvailabilityZone")
                <*> (x .@? "validUntil")
                <*> (x .@? "launchGroup")
                <*> (x .@? "fault")
                <*> (x .@? "spotInstanceRequestId")
                <*> (x .@? "type")
                <*> (x .@? "validFrom")
                <*> (x .@? "createTime")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData SpotInstanceRequest where

-- | Describes a Spot Instance state change.
--
--
--
-- /See:/ 'spotInstanceStateFault' smart constructor.
data SpotInstanceStateFault = SpotInstanceStateFault'
  { _sisfCode    :: !(Maybe Text)
  , _sisfMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotInstanceStateFault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sisfCode' - The reason code for the Spot Instance state change.
--
-- * 'sisfMessage' - The message for the Spot Instance state change.
spotInstanceStateFault
    :: SpotInstanceStateFault
spotInstanceStateFault =
  SpotInstanceStateFault' {_sisfCode = Nothing, _sisfMessage = Nothing}


-- | The reason code for the Spot Instance state change.
sisfCode :: Lens' SpotInstanceStateFault (Maybe Text)
sisfCode = lens _sisfCode (\ s a -> s{_sisfCode = a})

-- | The message for the Spot Instance state change.
sisfMessage :: Lens' SpotInstanceStateFault (Maybe Text)
sisfMessage = lens _sisfMessage (\ s a -> s{_sisfMessage = a})

instance FromXML SpotInstanceStateFault where
        parseXML x
          = SpotInstanceStateFault' <$>
              (x .@? "code") <*> (x .@? "message")

instance NFData SpotInstanceStateFault where

-- | Describes the status of a Spot Instance request.
--
--
--
-- /See:/ 'spotInstanceStatus' smart constructor.
data SpotInstanceStatus = SpotInstanceStatus'
  { _sisUpdateTime :: !(Maybe ISO8601)
  , _sisCode       :: !(Maybe Text)
  , _sisMessage    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotInstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sisUpdateTime' - The date and time of the most recent status update, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- * 'sisCode' - The status code. For a list of status codes, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html#spot-instance-bid-status-understand Spot Status Codes> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'sisMessage' - The description for the status code.
spotInstanceStatus
    :: SpotInstanceStatus
spotInstanceStatus =
  SpotInstanceStatus'
    {_sisUpdateTime = Nothing, _sisCode = Nothing, _sisMessage = Nothing}


-- | The date and time of the most recent status update, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
sisUpdateTime :: Lens' SpotInstanceStatus (Maybe UTCTime)
sisUpdateTime = lens _sisUpdateTime (\ s a -> s{_sisUpdateTime = a}) . mapping _Time

-- | The status code. For a list of status codes, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html#spot-instance-bid-status-understand Spot Status Codes> in the /Amazon EC2 User Guide for Linux Instances/ .
sisCode :: Lens' SpotInstanceStatus (Maybe Text)
sisCode = lens _sisCode (\ s a -> s{_sisCode = a})

-- | The description for the status code.
sisMessage :: Lens' SpotInstanceStatus (Maybe Text)
sisMessage = lens _sisMessage (\ s a -> s{_sisMessage = a})

instance FromXML SpotInstanceStatus where
        parseXML x
          = SpotInstanceStatus' <$>
              (x .@? "updateTime") <*> (x .@? "code") <*>
                (x .@? "message")

instance NFData SpotInstanceStatus where

-- | The options for Spot Instances.
--
--
--
-- /See:/ 'spotMarketOptions' smart constructor.
data SpotMarketOptions = SpotMarketOptions'
  { _smoBlockDurationMinutes         :: !(Maybe Int)
  , _smoInstanceInterruptionBehavior :: !(Maybe InstanceInterruptionBehavior)
  , _smoValidUntil                   :: !(Maybe ISO8601)
  , _smoSpotInstanceType             :: !(Maybe SpotInstanceType)
  , _smoMaxPrice                     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotMarketOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smoBlockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- * 'smoInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'smoValidUntil' - The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
--
-- * 'smoSpotInstanceType' - The Spot Instance request type.
--
-- * 'smoMaxPrice' - The maximum hourly price you're willing to pay for the Spot Instances. The default is the On-Demand price.
spotMarketOptions
    :: SpotMarketOptions
spotMarketOptions =
  SpotMarketOptions'
    { _smoBlockDurationMinutes = Nothing
    , _smoInstanceInterruptionBehavior = Nothing
    , _smoValidUntil = Nothing
    , _smoSpotInstanceType = Nothing
    , _smoMaxPrice = Nothing
    }


-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
smoBlockDurationMinutes :: Lens' SpotMarketOptions (Maybe Int)
smoBlockDurationMinutes = lens _smoBlockDurationMinutes (\ s a -> s{_smoBlockDurationMinutes = a})

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
smoInstanceInterruptionBehavior :: Lens' SpotMarketOptions (Maybe InstanceInterruptionBehavior)
smoInstanceInterruptionBehavior = lens _smoInstanceInterruptionBehavior (\ s a -> s{_smoInstanceInterruptionBehavior = a})

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
smoValidUntil :: Lens' SpotMarketOptions (Maybe UTCTime)
smoValidUntil = lens _smoValidUntil (\ s a -> s{_smoValidUntil = a}) . mapping _Time

-- | The Spot Instance request type.
smoSpotInstanceType :: Lens' SpotMarketOptions (Maybe SpotInstanceType)
smoSpotInstanceType = lens _smoSpotInstanceType (\ s a -> s{_smoSpotInstanceType = a})

-- | The maximum hourly price you're willing to pay for the Spot Instances. The default is the On-Demand price.
smoMaxPrice :: Lens' SpotMarketOptions (Maybe Text)
smoMaxPrice = lens _smoMaxPrice (\ s a -> s{_smoMaxPrice = a})

instance NFData SpotMarketOptions where

instance ToQuery SpotMarketOptions where
        toQuery SpotMarketOptions'{..}
          = mconcat
              ["BlockDurationMinutes" =: _smoBlockDurationMinutes,
               "InstanceInterruptionBehavior" =:
                 _smoInstanceInterruptionBehavior,
               "ValidUntil" =: _smoValidUntil,
               "SpotInstanceType" =: _smoSpotInstanceType,
               "MaxPrice" =: _smoMaxPrice]

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
--
--
-- /See:/ 'spotOptions' smart constructor.
data SpotOptions = SpotOptions'
  { _soInstanceInterruptionBehavior :: !(Maybe SpotInstanceInterruptionBehavior)
  , _soAllocationStrategy           :: !(Maybe SpotAllocationStrategy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'soAllocationStrategy' - Indicates how to allocate the target capacity across the Spot pools specified by the Spot Fleet request. The default is @lowestPrice@ .
spotOptions
    :: SpotOptions
spotOptions =
  SpotOptions'
    {_soInstanceInterruptionBehavior = Nothing, _soAllocationStrategy = Nothing}


-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
soInstanceInterruptionBehavior :: Lens' SpotOptions (Maybe SpotInstanceInterruptionBehavior)
soInstanceInterruptionBehavior = lens _soInstanceInterruptionBehavior (\ s a -> s{_soInstanceInterruptionBehavior = a})

-- | Indicates how to allocate the target capacity across the Spot pools specified by the Spot Fleet request. The default is @lowestPrice@ .
soAllocationStrategy :: Lens' SpotOptions (Maybe SpotAllocationStrategy)
soAllocationStrategy = lens _soAllocationStrategy (\ s a -> s{_soAllocationStrategy = a})

instance FromXML SpotOptions where
        parseXML x
          = SpotOptions' <$>
              (x .@? "instanceInterruptionBehavior") <*>
                (x .@? "allocationStrategy")

instance NFData SpotOptions where

-- | Describes the configuration of Spot Instances in an EC2 Fleet request.
--
--
--
-- /See:/ 'spotOptionsRequest' smart constructor.
data SpotOptionsRequest = SpotOptionsRequest'
  { _sorInstanceInterruptionBehavior :: !(Maybe SpotInstanceInterruptionBehavior)
  , _sorAllocationStrategy :: !(Maybe SpotAllocationStrategy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sorInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'sorAllocationStrategy' - Indicates how to allocate the target capacity across the Spot pools specified by the Spot Fleet request. The default is @lowestPrice@ .
spotOptionsRequest
    :: SpotOptionsRequest
spotOptionsRequest =
  SpotOptionsRequest'
    { _sorInstanceInterruptionBehavior = Nothing
    , _sorAllocationStrategy = Nothing
    }


-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
sorInstanceInterruptionBehavior :: Lens' SpotOptionsRequest (Maybe SpotInstanceInterruptionBehavior)
sorInstanceInterruptionBehavior = lens _sorInstanceInterruptionBehavior (\ s a -> s{_sorInstanceInterruptionBehavior = a})

-- | Indicates how to allocate the target capacity across the Spot pools specified by the Spot Fleet request. The default is @lowestPrice@ .
sorAllocationStrategy :: Lens' SpotOptionsRequest (Maybe SpotAllocationStrategy)
sorAllocationStrategy = lens _sorAllocationStrategy (\ s a -> s{_sorAllocationStrategy = a})

instance NFData SpotOptionsRequest where

instance ToQuery SpotOptionsRequest where
        toQuery SpotOptionsRequest'{..}
          = mconcat
              ["InstanceInterruptionBehavior" =:
                 _sorInstanceInterruptionBehavior,
               "AllocationStrategy" =: _sorAllocationStrategy]

-- | Describes Spot Instance placement.
--
--
--
-- /See:/ 'spotPlacement' smart constructor.
data SpotPlacement = SpotPlacement'
  { _spAvailabilityZone :: !(Maybe Text)
  , _spTenancy          :: !(Maybe Tenancy)
  , _spGroupName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spAvailabilityZone' - The Availability Zone. [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
--
-- * 'spTenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
--
-- * 'spGroupName' - The name of the placement group.
spotPlacement
    :: SpotPlacement
spotPlacement =
  SpotPlacement'
    { _spAvailabilityZone = Nothing
    , _spTenancy = Nothing
    , _spGroupName = Nothing
    }


-- | The Availability Zone. [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
spAvailabilityZone :: Lens' SpotPlacement (Maybe Text)
spAvailabilityZone = lens _spAvailabilityZone (\ s a -> s{_spAvailabilityZone = a})

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
spTenancy :: Lens' SpotPlacement (Maybe Tenancy)
spTenancy = lens _spTenancy (\ s a -> s{_spTenancy = a})

-- | The name of the placement group.
spGroupName :: Lens' SpotPlacement (Maybe Text)
spGroupName = lens _spGroupName (\ s a -> s{_spGroupName = a})

instance FromXML SpotPlacement where
        parseXML x
          = SpotPlacement' <$>
              (x .@? "availabilityZone") <*> (x .@? "tenancy") <*>
                (x .@? "groupName")

instance NFData SpotPlacement where

instance ToQuery SpotPlacement where
        toQuery SpotPlacement'{..}
          = mconcat
              ["AvailabilityZone" =: _spAvailabilityZone,
               "Tenancy" =: _spTenancy, "GroupName" =: _spGroupName]

-- | Describes the maximum price per hour that you are willing to pay for a Spot Instance.
--
--
--
-- /See:/ 'spotPrice' smart constructor.
data SpotPrice = SpotPrice'
  { _sProductDescription :: !(Maybe RIProductDescription)
  , _sSpotPrice          :: !(Maybe Text)
  , _sInstanceType       :: !(Maybe InstanceType)
  , _sAvailabilityZone   :: !(Maybe Text)
  , _sTimestamp          :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpotPrice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sProductDescription' - A general description of the AMI.
--
-- * 'sSpotPrice' - The maximum price per hour that you are willing to pay for a Spot Instance.
--
-- * 'sInstanceType' - The instance type.
--
-- * 'sAvailabilityZone' - The Availability Zone.
--
-- * 'sTimestamp' - The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
spotPrice
    :: SpotPrice
spotPrice =
  SpotPrice'
    { _sProductDescription = Nothing
    , _sSpotPrice = Nothing
    , _sInstanceType = Nothing
    , _sAvailabilityZone = Nothing
    , _sTimestamp = Nothing
    }


-- | A general description of the AMI.
sProductDescription :: Lens' SpotPrice (Maybe RIProductDescription)
sProductDescription = lens _sProductDescription (\ s a -> s{_sProductDescription = a})

-- | The maximum price per hour that you are willing to pay for a Spot Instance.
sSpotPrice :: Lens' SpotPrice (Maybe Text)
sSpotPrice = lens _sSpotPrice (\ s a -> s{_sSpotPrice = a})

-- | The instance type.
sInstanceType :: Lens' SpotPrice (Maybe InstanceType)
sInstanceType = lens _sInstanceType (\ s a -> s{_sInstanceType = a})

-- | The Availability Zone.
sAvailabilityZone :: Lens' SpotPrice (Maybe Text)
sAvailabilityZone = lens _sAvailabilityZone (\ s a -> s{_sAvailabilityZone = a})

-- | The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
sTimestamp :: Lens' SpotPrice (Maybe UTCTime)
sTimestamp = lens _sTimestamp (\ s a -> s{_sTimestamp = a}) . mapping _Time

instance FromXML SpotPrice where
        parseXML x
          = SpotPrice' <$>
              (x .@? "productDescription") <*> (x .@? "spotPrice")
                <*> (x .@? "instanceType")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "timestamp")

instance NFData SpotPrice where

-- | Describes a stale rule in a security group.
--
--
--
-- /See:/ 'staleIPPermission' smart constructor.
data StaleIPPermission = StaleIPPermission'
  { _sipFromPort         :: !(Maybe Int)
  , _sipUserIdGroupPairs :: !(Maybe [UserIdGroupPair])
  , _sipPrefixListIds    :: !(Maybe [Text])
  , _sipIPProtocol       :: !(Maybe Text)
  , _sipToPort           :: !(Maybe Int)
  , _sipIPRanges         :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StaleIPPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sipFromPort' - The start of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types.
--
-- * 'sipUserIdGroupPairs' - One or more security group pairs. Returns the ID of the referenced security group and VPC, and the ID and status of the VPC peering connection.
--
-- * 'sipPrefixListIds' - One or more prefix list IDs for an AWS service. Not applicable for stale security group rules.
--
-- * 'sipIPProtocol' - The IP protocol name (for @tcp@ , @udp@ , and @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)> .
--
-- * 'sipToPort' - The end of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types.
--
-- * 'sipIPRanges' - One or more IP ranges. Not applicable for stale security group rules.
staleIPPermission
    :: StaleIPPermission
staleIPPermission =
  StaleIPPermission'
    { _sipFromPort = Nothing
    , _sipUserIdGroupPairs = Nothing
    , _sipPrefixListIds = Nothing
    , _sipIPProtocol = Nothing
    , _sipToPort = Nothing
    , _sipIPRanges = Nothing
    }


-- | The start of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types.
sipFromPort :: Lens' StaleIPPermission (Maybe Int)
sipFromPort = lens _sipFromPort (\ s a -> s{_sipFromPort = a})

-- | One or more security group pairs. Returns the ID of the referenced security group and VPC, and the ID and status of the VPC peering connection.
sipUserIdGroupPairs :: Lens' StaleIPPermission [UserIdGroupPair]
sipUserIdGroupPairs = lens _sipUserIdGroupPairs (\ s a -> s{_sipUserIdGroupPairs = a}) . _Default . _Coerce

-- | One or more prefix list IDs for an AWS service. Not applicable for stale security group rules.
sipPrefixListIds :: Lens' StaleIPPermission [Text]
sipPrefixListIds = lens _sipPrefixListIds (\ s a -> s{_sipPrefixListIds = a}) . _Default . _Coerce

-- | The IP protocol name (for @tcp@ , @udp@ , and @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)> .
sipIPProtocol :: Lens' StaleIPPermission (Maybe Text)
sipIPProtocol = lens _sipIPProtocol (\ s a -> s{_sipIPProtocol = a})

-- | The end of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types.
sipToPort :: Lens' StaleIPPermission (Maybe Int)
sipToPort = lens _sipToPort (\ s a -> s{_sipToPort = a})

-- | One or more IP ranges. Not applicable for stale security group rules.
sipIPRanges :: Lens' StaleIPPermission [Text]
sipIPRanges = lens _sipIPRanges (\ s a -> s{_sipIPRanges = a}) . _Default . _Coerce

instance FromXML StaleIPPermission where
        parseXML x
          = StaleIPPermission' <$>
              (x .@? "fromPort") <*>
                (x .@? "groups" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "prefixListIds" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "ipProtocol")
                <*> (x .@? "toPort")
                <*>
                (x .@? "ipRanges" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData StaleIPPermission where

-- | Describes a stale security group (a security group that contains stale rules).
--
--
--
-- /See:/ 'staleSecurityGroup' smart constructor.
data StaleSecurityGroup = StaleSecurityGroup'
  { _ssgVPCId                    :: !(Maybe Text)
  , _ssgGroupName                :: !(Maybe Text)
  , _ssgStaleIPPermissionsEgress :: !(Maybe [StaleIPPermission])
  , _ssgStaleIPPermissions       :: !(Maybe [StaleIPPermission])
  , _ssgDescription              :: !(Maybe Text)
  , _ssgGroupId                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StaleSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgVPCId' - The ID of the VPC for the security group.
--
-- * 'ssgGroupName' - The name of the security group.
--
-- * 'ssgStaleIPPermissionsEgress' - Information about the stale outbound rules in the security group.
--
-- * 'ssgStaleIPPermissions' - Information about the stale inbound rules in the security group.
--
-- * 'ssgDescription' - The description of the security group.
--
-- * 'ssgGroupId' - The ID of the security group.
staleSecurityGroup
    :: Text -- ^ 'ssgGroupId'
    -> StaleSecurityGroup
staleSecurityGroup pGroupId_ =
  StaleSecurityGroup'
    { _ssgVPCId = Nothing
    , _ssgGroupName = Nothing
    , _ssgStaleIPPermissionsEgress = Nothing
    , _ssgStaleIPPermissions = Nothing
    , _ssgDescription = Nothing
    , _ssgGroupId = pGroupId_
    }


-- | The ID of the VPC for the security group.
ssgVPCId :: Lens' StaleSecurityGroup (Maybe Text)
ssgVPCId = lens _ssgVPCId (\ s a -> s{_ssgVPCId = a})

-- | The name of the security group.
ssgGroupName :: Lens' StaleSecurityGroup (Maybe Text)
ssgGroupName = lens _ssgGroupName (\ s a -> s{_ssgGroupName = a})

-- | Information about the stale outbound rules in the security group.
ssgStaleIPPermissionsEgress :: Lens' StaleSecurityGroup [StaleIPPermission]
ssgStaleIPPermissionsEgress = lens _ssgStaleIPPermissionsEgress (\ s a -> s{_ssgStaleIPPermissionsEgress = a}) . _Default . _Coerce

-- | Information about the stale inbound rules in the security group.
ssgStaleIPPermissions :: Lens' StaleSecurityGroup [StaleIPPermission]
ssgStaleIPPermissions = lens _ssgStaleIPPermissions (\ s a -> s{_ssgStaleIPPermissions = a}) . _Default . _Coerce

-- | The description of the security group.
ssgDescription :: Lens' StaleSecurityGroup (Maybe Text)
ssgDescription = lens _ssgDescription (\ s a -> s{_ssgDescription = a})

-- | The ID of the security group.
ssgGroupId :: Lens' StaleSecurityGroup Text
ssgGroupId = lens _ssgGroupId (\ s a -> s{_ssgGroupId = a})

instance FromXML StaleSecurityGroup where
        parseXML x
          = StaleSecurityGroup' <$>
              (x .@? "vpcId") <*> (x .@? "groupName") <*>
                (x .@? "staleIpPermissionsEgress" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "staleIpPermissions" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "description")
                <*> (x .@ "groupId")

instance NFData StaleSecurityGroup where

-- | Describes a state change.
--
--
--
-- /See:/ 'stateReason' smart constructor.
data StateReason = StateReason'
  { _srCode    :: !(Maybe Text)
  , _srMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StateReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srCode' - The reason code for the state change.
--
-- * 'srMessage' - The message for the state change.     * @Server.InsufficientInstanceCapacity@ : There was insufficient instance capacity to satisfy the launch request.     * @Server.InternalError@ : An internal error occurred during instance launch, resulting in termination.     * @Server.ScheduledStop@ : The instance was stopped due to a scheduled retirement.     * @Server.SpotInstanceTermination@ : A Spot Instance was terminated due to an increase in the Spot price.     * @Client.InternalError@ : A client error caused the instance to terminate on launch.     * @Client.InstanceInitiatedShutdown@ : The instance was shut down using the @shutdown -h@ command from the instance.     * @Client.InstanceTerminated@ : The instance was terminated or rebooted during AMI creation.     * @Client.UserInitiatedShutdown@ : The instance was shut down using the Amazon EC2 API.     * @Client.VolumeLimitExceeded@ : The limit on the number of EBS volumes or total storage was exceeded. Decrease usage or request an increase in your limits.     * @Client.InvalidSnapshot.NotFound@ : The specified snapshot was not found.
stateReason
    :: StateReason
stateReason = StateReason' {_srCode = Nothing, _srMessage = Nothing}


-- | The reason code for the state change.
srCode :: Lens' StateReason (Maybe Text)
srCode = lens _srCode (\ s a -> s{_srCode = a})

-- | The message for the state change.     * @Server.InsufficientInstanceCapacity@ : There was insufficient instance capacity to satisfy the launch request.     * @Server.InternalError@ : An internal error occurred during instance launch, resulting in termination.     * @Server.ScheduledStop@ : The instance was stopped due to a scheduled retirement.     * @Server.SpotInstanceTermination@ : A Spot Instance was terminated due to an increase in the Spot price.     * @Client.InternalError@ : A client error caused the instance to terminate on launch.     * @Client.InstanceInitiatedShutdown@ : The instance was shut down using the @shutdown -h@ command from the instance.     * @Client.InstanceTerminated@ : The instance was terminated or rebooted during AMI creation.     * @Client.UserInitiatedShutdown@ : The instance was shut down using the Amazon EC2 API.     * @Client.VolumeLimitExceeded@ : The limit on the number of EBS volumes or total storage was exceeded. Decrease usage or request an increase in your limits.     * @Client.InvalidSnapshot.NotFound@ : The specified snapshot was not found.
srMessage :: Lens' StateReason (Maybe Text)
srMessage = lens _srMessage (\ s a -> s{_srMessage = a})

instance FromXML StateReason where
        parseXML x
          = StateReason' <$>
              (x .@? "code") <*> (x .@? "message")

instance NFData StateReason where

-- | Describes the storage location for an instance store-backed AMI.
--
--
--
-- /See:/ 'storage' smart constructor.
newtype Storage = Storage'
  { _sS3 :: Maybe S3Storage
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Storage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sS3' - An Amazon S3 storage location.
storage
    :: Storage
storage = Storage' {_sS3 = Nothing}


-- | An Amazon S3 storage location.
sS3 :: Lens' Storage (Maybe S3Storage)
sS3 = lens _sS3 (\ s a -> s{_sS3 = a})

instance FromXML Storage where
        parseXML x = Storage' <$> (x .@? "S3")

instance NFData Storage where

instance ToQuery Storage where
        toQuery Storage'{..} = mconcat ["S3" =: _sS3]

-- | Describes a storage location in Amazon S3.
--
--
--
-- /See:/ 'storageLocation' smart constructor.
data StorageLocation = StorageLocation'
  { _slBucket :: !(Maybe Text)
  , _slKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucket' - The name of the S3 bucket.
--
-- * 'slKey' - The key.
storageLocation
    :: StorageLocation
storageLocation = StorageLocation' {_slBucket = Nothing, _slKey = Nothing}


-- | The name of the S3 bucket.
slBucket :: Lens' StorageLocation (Maybe Text)
slBucket = lens _slBucket (\ s a -> s{_slBucket = a})

-- | The key.
slKey :: Lens' StorageLocation (Maybe Text)
slKey = lens _slKey (\ s a -> s{_slKey = a})

instance NFData StorageLocation where

instance ToQuery StorageLocation where
        toQuery StorageLocation'{..}
          = mconcat ["Bucket" =: _slBucket, "Key" =: _slKey]

-- | Describes a subnet.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _subIPv6CidrBlockAssociationSet :: !(Maybe [SubnetIPv6CidrBlockAssociation])
  , _subAssignIPv6AddressOnCreation :: !(Maybe Bool)
  , _subMapPublicIPOnLaunch         :: !(Maybe Bool)
  , _subDefaultForAz                :: !(Maybe Bool)
  , _subTags                        :: !(Maybe [Tag])
  , _subAvailabilityZone            :: !Text
  , _subAvailableIPAddressCount     :: !Int
  , _subCidrBlock                   :: !Text
  , _subState                       :: !SubnetState
  , _subSubnetId                    :: !Text
  , _subVPCId                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'subIPv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the subnet.
--
-- * 'subAssignIPv6AddressOnCreation' - Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
--
-- * 'subMapPublicIPOnLaunch' - Indicates whether instances launched in this subnet receive a public IPv4 address.
--
-- * 'subDefaultForAz' - Indicates whether this is the default subnet for the Availability Zone.
--
-- * 'subTags' - Any tags assigned to the subnet.
--
-- * 'subAvailabilityZone' - The Availability Zone of the subnet.
--
-- * 'subAvailableIPAddressCount' - The number of unused private IPv4 addresses in the subnet. Note that the IPv4 addresses for any stopped instances are considered unavailable.
--
-- * 'subCidrBlock' - The IPv4 CIDR block assigned to the subnet.
--
-- * 'subState' - The current state of the subnet.
--
-- * 'subSubnetId' - The ID of the subnet.
--
-- * 'subVPCId' - The ID of the VPC the subnet is in.
subnet
    :: Text -- ^ 'subAvailabilityZone'
    -> Int -- ^ 'subAvailableIPAddressCount'
    -> Text -- ^ 'subCidrBlock'
    -> SubnetState -- ^ 'subState'
    -> Text -- ^ 'subSubnetId'
    -> Text -- ^ 'subVPCId'
    -> Subnet
subnet pAvailabilityZone_ pAvailableIPAddressCount_ pCidrBlock_ pState_ pSubnetId_ pVPCId_ =
  Subnet'
    { _subIPv6CidrBlockAssociationSet = Nothing
    , _subAssignIPv6AddressOnCreation = Nothing
    , _subMapPublicIPOnLaunch = Nothing
    , _subDefaultForAz = Nothing
    , _subTags = Nothing
    , _subAvailabilityZone = pAvailabilityZone_
    , _subAvailableIPAddressCount = pAvailableIPAddressCount_
    , _subCidrBlock = pCidrBlock_
    , _subState = pState_
    , _subSubnetId = pSubnetId_
    , _subVPCId = pVPCId_
    }


-- | Information about the IPv6 CIDR blocks associated with the subnet.
subIPv6CidrBlockAssociationSet :: Lens' Subnet [SubnetIPv6CidrBlockAssociation]
subIPv6CidrBlockAssociationSet = lens _subIPv6CidrBlockAssociationSet (\ s a -> s{_subIPv6CidrBlockAssociationSet = a}) . _Default . _Coerce

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
subAssignIPv6AddressOnCreation :: Lens' Subnet (Maybe Bool)
subAssignIPv6AddressOnCreation = lens _subAssignIPv6AddressOnCreation (\ s a -> s{_subAssignIPv6AddressOnCreation = a})

-- | Indicates whether instances launched in this subnet receive a public IPv4 address.
subMapPublicIPOnLaunch :: Lens' Subnet (Maybe Bool)
subMapPublicIPOnLaunch = lens _subMapPublicIPOnLaunch (\ s a -> s{_subMapPublicIPOnLaunch = a})

-- | Indicates whether this is the default subnet for the Availability Zone.
subDefaultForAz :: Lens' Subnet (Maybe Bool)
subDefaultForAz = lens _subDefaultForAz (\ s a -> s{_subDefaultForAz = a})

-- | Any tags assigned to the subnet.
subTags :: Lens' Subnet [Tag]
subTags = lens _subTags (\ s a -> s{_subTags = a}) . _Default . _Coerce

-- | The Availability Zone of the subnet.
subAvailabilityZone :: Lens' Subnet Text
subAvailabilityZone = lens _subAvailabilityZone (\ s a -> s{_subAvailabilityZone = a})

-- | The number of unused private IPv4 addresses in the subnet. Note that the IPv4 addresses for any stopped instances are considered unavailable.
subAvailableIPAddressCount :: Lens' Subnet Int
subAvailableIPAddressCount = lens _subAvailableIPAddressCount (\ s a -> s{_subAvailableIPAddressCount = a})

-- | The IPv4 CIDR block assigned to the subnet.
subCidrBlock :: Lens' Subnet Text
subCidrBlock = lens _subCidrBlock (\ s a -> s{_subCidrBlock = a})

-- | The current state of the subnet.
subState :: Lens' Subnet SubnetState
subState = lens _subState (\ s a -> s{_subState = a})

-- | The ID of the subnet.
subSubnetId :: Lens' Subnet Text
subSubnetId = lens _subSubnetId (\ s a -> s{_subSubnetId = a})

-- | The ID of the VPC the subnet is in.
subVPCId :: Lens' Subnet Text
subVPCId = lens _subVPCId (\ s a -> s{_subVPCId = a})

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "ipv6CidrBlockAssociationSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "assignIpv6AddressOnCreation")
                <*> (x .@? "mapPublicIpOnLaunch")
                <*> (x .@? "defaultForAz")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "availabilityZone")
                <*> (x .@ "availableIpAddressCount")
                <*> (x .@ "cidrBlock")
                <*> (x .@ "state")
                <*> (x .@ "subnetId")
                <*> (x .@ "vpcId")

instance NFData Subnet where

-- | Describes the state of a CIDR block.
--
--
--
-- /See:/ 'subnetCidrBlockState' smart constructor.
data SubnetCidrBlockState = SubnetCidrBlockState'
  { _scbsState         :: !(Maybe SubnetCidrBlockStateCode)
  , _scbsStatusMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubnetCidrBlockState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scbsState' - The state of a CIDR block.
--
-- * 'scbsStatusMessage' - A message about the status of the CIDR block, if applicable.
subnetCidrBlockState
    :: SubnetCidrBlockState
subnetCidrBlockState =
  SubnetCidrBlockState' {_scbsState = Nothing, _scbsStatusMessage = Nothing}


-- | The state of a CIDR block.
scbsState :: Lens' SubnetCidrBlockState (Maybe SubnetCidrBlockStateCode)
scbsState = lens _scbsState (\ s a -> s{_scbsState = a})

-- | A message about the status of the CIDR block, if applicable.
scbsStatusMessage :: Lens' SubnetCidrBlockState (Maybe Text)
scbsStatusMessage = lens _scbsStatusMessage (\ s a -> s{_scbsStatusMessage = a})

instance FromXML SubnetCidrBlockState where
        parseXML x
          = SubnetCidrBlockState' <$>
              (x .@? "state") <*> (x .@? "statusMessage")

instance NFData SubnetCidrBlockState where

-- | Describes an IPv6 CIDR block associated with a subnet.
--
--
--
-- /See:/ 'subnetIPv6CidrBlockAssociation' smart constructor.
data SubnetIPv6CidrBlockAssociation = SubnetIPv6CidrBlockAssociation'
  { _sicbaAssociationId      :: !(Maybe Text)
  , _sicbaIPv6CidrBlock      :: !(Maybe Text)
  , _sicbaIPv6CidrBlockState :: !(Maybe SubnetCidrBlockState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubnetIPv6CidrBlockAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sicbaAssociationId' - The association ID for the CIDR block.
--
-- * 'sicbaIPv6CidrBlock' - The IPv6 CIDR block.
--
-- * 'sicbaIPv6CidrBlockState' - Information about the state of the CIDR block.
subnetIPv6CidrBlockAssociation
    :: SubnetIPv6CidrBlockAssociation
subnetIPv6CidrBlockAssociation =
  SubnetIPv6CidrBlockAssociation'
    { _sicbaAssociationId = Nothing
    , _sicbaIPv6CidrBlock = Nothing
    , _sicbaIPv6CidrBlockState = Nothing
    }


-- | The association ID for the CIDR block.
sicbaAssociationId :: Lens' SubnetIPv6CidrBlockAssociation (Maybe Text)
sicbaAssociationId = lens _sicbaAssociationId (\ s a -> s{_sicbaAssociationId = a})

-- | The IPv6 CIDR block.
sicbaIPv6CidrBlock :: Lens' SubnetIPv6CidrBlockAssociation (Maybe Text)
sicbaIPv6CidrBlock = lens _sicbaIPv6CidrBlock (\ s a -> s{_sicbaIPv6CidrBlock = a})

-- | Information about the state of the CIDR block.
sicbaIPv6CidrBlockState :: Lens' SubnetIPv6CidrBlockAssociation (Maybe SubnetCidrBlockState)
sicbaIPv6CidrBlockState = lens _sicbaIPv6CidrBlockState (\ s a -> s{_sicbaIPv6CidrBlockState = a})

instance FromXML SubnetIPv6CidrBlockAssociation where
        parseXML x
          = SubnetIPv6CidrBlockAssociation' <$>
              (x .@? "associationId") <*> (x .@? "ipv6CidrBlock")
                <*> (x .@? "ipv6CidrBlockState")

instance NFData SubnetIPv6CidrBlockAssociation where

-- | Describes the T2 instance whose credit option for CPU usage was successfully modified.
--
--
--
-- /See:/ 'successfulInstanceCreditSpecificationItem' smart constructor.
newtype SuccessfulInstanceCreditSpecificationItem = SuccessfulInstanceCreditSpecificationItem'
  { _sicsiInstanceId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuccessfulInstanceCreditSpecificationItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sicsiInstanceId' - The ID of the instance.
successfulInstanceCreditSpecificationItem
    :: SuccessfulInstanceCreditSpecificationItem
successfulInstanceCreditSpecificationItem =
  SuccessfulInstanceCreditSpecificationItem' {_sicsiInstanceId = Nothing}


-- | The ID of the instance.
sicsiInstanceId :: Lens' SuccessfulInstanceCreditSpecificationItem (Maybe Text)
sicsiInstanceId = lens _sicsiInstanceId (\ s a -> s{_sicsiInstanceId = a})

instance FromXML
           SuccessfulInstanceCreditSpecificationItem
         where
        parseXML x
          = SuccessfulInstanceCreditSpecificationItem' <$>
              (x .@? "instanceId")

instance NFData
           SuccessfulInstanceCreditSpecificationItem
         where

-- | Describes a tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The key of the tag. Constraints: Tag keys are case-sensitive and accept a maximum of 127 Unicode characters. May not begin with @aws:@
--
-- * 'tagValue' - The value of the tag. Constraints: Tag values are case-sensitive and accept a maximum of 255 Unicode characters.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The key of the tag. Constraints: Tag keys are case-sensitive and accept a maximum of 127 Unicode characters. May not begin with @aws:@
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The value of the tag. Constraints: Tag values are case-sensitive and accept a maximum of 255 Unicode characters.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromXML Tag where
        parseXML x = Tag' <$> (x .@ "key") <*> (x .@ "value")

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Key" =: _tagKey, "Value" =: _tagValue]

-- | Describes a tag.
--
--
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
  { _tdResourceId   :: !Text
  , _tdResourceType :: !ResourceType
  , _tdKey          :: !Text
  , _tdValue        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdResourceId' - The ID of the resource. For example, @ami-1a2b3c4d@ .
--
-- * 'tdResourceType' - The resource type.
--
-- * 'tdKey' - The tag key.
--
-- * 'tdValue' - The tag value.
tagDescription
    :: Text -- ^ 'tdResourceId'
    -> ResourceType -- ^ 'tdResourceType'
    -> Text -- ^ 'tdKey'
    -> Text -- ^ 'tdValue'
    -> TagDescription
tagDescription pResourceId_ pResourceType_ pKey_ pValue_ =
  TagDescription'
    { _tdResourceId = pResourceId_
    , _tdResourceType = pResourceType_
    , _tdKey = pKey_
    , _tdValue = pValue_
    }


-- | The ID of the resource. For example, @ami-1a2b3c4d@ .
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\ s a -> s{_tdResourceId = a})

-- | The resource type.
tdResourceType :: Lens' TagDescription ResourceType
tdResourceType = lens _tdResourceType (\ s a -> s{_tdResourceType = a})

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\ s a -> s{_tdKey = a})

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\ s a -> s{_tdValue = a})

instance FromXML TagDescription where
        parseXML x
          = TagDescription' <$>
              (x .@ "resourceId") <*> (x .@ "resourceType") <*>
                (x .@ "key")
                <*> (x .@ "value")

instance NFData TagDescription where

-- | The tags to apply to a resource when the resource is being created.
--
--
--
-- /See:/ 'tagSpecification' smart constructor.
data TagSpecification = TagSpecification'
  { _tsResourceType :: !(Maybe ResourceType)
  , _tsTags         :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsResourceType' - The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ .
--
-- * 'tsTags' - The tags to apply to the resource.
tagSpecification
    :: TagSpecification
tagSpecification =
  TagSpecification' {_tsResourceType = Nothing, _tsTags = Nothing}


-- | The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ .
tsResourceType :: Lens' TagSpecification (Maybe ResourceType)
tsResourceType = lens _tsResourceType (\ s a -> s{_tsResourceType = a})

-- | The tags to apply to the resource.
tsTags :: Lens' TagSpecification [Tag]
tsTags = lens _tsTags (\ s a -> s{_tsTags = a}) . _Default . _Coerce

instance NFData TagSpecification where

instance ToQuery TagSpecification where
        toQuery TagSpecification'{..}
          = mconcat
              ["ResourceType" =: _tsResourceType,
               toQuery (toQueryList "Tag" <$> _tsTags)]

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
--
--
-- /See:/ 'targetCapacitySpecification' smart constructor.
data TargetCapacitySpecification = TargetCapacitySpecification'
  { _tcsOnDemandTargetCapacity    :: !(Maybe Int)
  , _tcsDefaultTargetCapacityType :: !(Maybe DefaultTargetCapacityType)
  , _tcsTotalTargetCapacity       :: !(Maybe Int)
  , _tcsSpotTargetCapacity        :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetCapacitySpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcsOnDemandTargetCapacity' - The number of On-Demand units to request.
--
-- * 'tcsDefaultTargetCapacityType' - The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
--
-- * 'tcsTotalTargetCapacity' - The number of units to request, filled using @DefaultTargetCapacityType@ .
--
-- * 'tcsSpotTargetCapacity' - The maximum number of Spot units to launch.
targetCapacitySpecification
    :: TargetCapacitySpecification
targetCapacitySpecification =
  TargetCapacitySpecification'
    { _tcsOnDemandTargetCapacity = Nothing
    , _tcsDefaultTargetCapacityType = Nothing
    , _tcsTotalTargetCapacity = Nothing
    , _tcsSpotTargetCapacity = Nothing
    }


-- | The number of On-Demand units to request.
tcsOnDemandTargetCapacity :: Lens' TargetCapacitySpecification (Maybe Int)
tcsOnDemandTargetCapacity = lens _tcsOnDemandTargetCapacity (\ s a -> s{_tcsOnDemandTargetCapacity = a})

-- | The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
tcsDefaultTargetCapacityType :: Lens' TargetCapacitySpecification (Maybe DefaultTargetCapacityType)
tcsDefaultTargetCapacityType = lens _tcsDefaultTargetCapacityType (\ s a -> s{_tcsDefaultTargetCapacityType = a})

-- | The number of units to request, filled using @DefaultTargetCapacityType@ .
tcsTotalTargetCapacity :: Lens' TargetCapacitySpecification (Maybe Int)
tcsTotalTargetCapacity = lens _tcsTotalTargetCapacity (\ s a -> s{_tcsTotalTargetCapacity = a})

-- | The maximum number of Spot units to launch.
tcsSpotTargetCapacity :: Lens' TargetCapacitySpecification (Maybe Int)
tcsSpotTargetCapacity = lens _tcsSpotTargetCapacity (\ s a -> s{_tcsSpotTargetCapacity = a})

instance FromXML TargetCapacitySpecification where
        parseXML x
          = TargetCapacitySpecification' <$>
              (x .@? "onDemandTargetCapacity") <*>
                (x .@? "defaultTargetCapacityType")
                <*> (x .@? "totalTargetCapacity")
                <*> (x .@? "spotTargetCapacity")

instance NFData TargetCapacitySpecification where

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
--
--
-- /See:/ 'targetCapacitySpecificationRequest' smart constructor.
data TargetCapacitySpecificationRequest = TargetCapacitySpecificationRequest'
  { _tcsrOnDemandTargetCapacity    :: !(Maybe Int)
  , _tcsrDefaultTargetCapacityType :: !(Maybe DefaultTargetCapacityType)
  , _tcsrSpotTargetCapacity        :: !(Maybe Int)
  , _tcsrTotalTargetCapacity       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetCapacitySpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcsrOnDemandTargetCapacity' - The number of On-Demand units to request.
--
-- * 'tcsrDefaultTargetCapacityType' - The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
--
-- * 'tcsrSpotTargetCapacity' - The number of Spot units to request.
--
-- * 'tcsrTotalTargetCapacity' - The number of units to request, filled using @DefaultTargetCapacityType@ .
targetCapacitySpecificationRequest
    :: Int -- ^ 'tcsrTotalTargetCapacity'
    -> TargetCapacitySpecificationRequest
targetCapacitySpecificationRequest pTotalTargetCapacity_ =
  TargetCapacitySpecificationRequest'
    { _tcsrOnDemandTargetCapacity = Nothing
    , _tcsrDefaultTargetCapacityType = Nothing
    , _tcsrSpotTargetCapacity = Nothing
    , _tcsrTotalTargetCapacity = pTotalTargetCapacity_
    }


-- | The number of On-Demand units to request.
tcsrOnDemandTargetCapacity :: Lens' TargetCapacitySpecificationRequest (Maybe Int)
tcsrOnDemandTargetCapacity = lens _tcsrOnDemandTargetCapacity (\ s a -> s{_tcsrOnDemandTargetCapacity = a})

-- | The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
tcsrDefaultTargetCapacityType :: Lens' TargetCapacitySpecificationRequest (Maybe DefaultTargetCapacityType)
tcsrDefaultTargetCapacityType = lens _tcsrDefaultTargetCapacityType (\ s a -> s{_tcsrDefaultTargetCapacityType = a})

-- | The number of Spot units to request.
tcsrSpotTargetCapacity :: Lens' TargetCapacitySpecificationRequest (Maybe Int)
tcsrSpotTargetCapacity = lens _tcsrSpotTargetCapacity (\ s a -> s{_tcsrSpotTargetCapacity = a})

-- | The number of units to request, filled using @DefaultTargetCapacityType@ .
tcsrTotalTargetCapacity :: Lens' TargetCapacitySpecificationRequest Int
tcsrTotalTargetCapacity = lens _tcsrTotalTargetCapacity (\ s a -> s{_tcsrTotalTargetCapacity = a})

instance NFData TargetCapacitySpecificationRequest
         where

instance ToQuery TargetCapacitySpecificationRequest
         where
        toQuery TargetCapacitySpecificationRequest'{..}
          = mconcat
              ["OnDemandTargetCapacity" =:
                 _tcsrOnDemandTargetCapacity,
               "DefaultTargetCapacityType" =:
                 _tcsrDefaultTargetCapacityType,
               "SpotTargetCapacity" =: _tcsrSpotTargetCapacity,
               "TotalTargetCapacity" =: _tcsrTotalTargetCapacity]

-- | Information about the Convertible Reserved Instance offering.
--
--
--
-- /See:/ 'targetConfiguration' smart constructor.
data TargetConfiguration = TargetConfiguration'
  { _tcInstanceCount :: !(Maybe Int)
  , _tcOfferingId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcInstanceCount' - The number of instances the Convertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
--
-- * 'tcOfferingId' - The ID of the Convertible Reserved Instance offering.
targetConfiguration
    :: TargetConfiguration
targetConfiguration =
  TargetConfiguration' {_tcInstanceCount = Nothing, _tcOfferingId = Nothing}


-- | The number of instances the Convertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
tcInstanceCount :: Lens' TargetConfiguration (Maybe Int)
tcInstanceCount = lens _tcInstanceCount (\ s a -> s{_tcInstanceCount = a})

-- | The ID of the Convertible Reserved Instance offering.
tcOfferingId :: Lens' TargetConfiguration (Maybe Text)
tcOfferingId = lens _tcOfferingId (\ s a -> s{_tcOfferingId = a})

instance FromXML TargetConfiguration where
        parseXML x
          = TargetConfiguration' <$>
              (x .@? "instanceCount") <*> (x .@? "offeringId")

instance NFData TargetConfiguration where

-- | Details about the target configuration.
--
--
--
-- /See:/ 'targetConfigurationRequest' smart constructor.
data TargetConfigurationRequest = TargetConfigurationRequest'
  { _tcrInstanceCount :: !(Maybe Int)
  , _tcrOfferingId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetConfigurationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcrInstanceCount' - The number of instances the Covertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
--
-- * 'tcrOfferingId' - The Convertible Reserved Instance offering ID.
targetConfigurationRequest
    :: Text -- ^ 'tcrOfferingId'
    -> TargetConfigurationRequest
targetConfigurationRequest pOfferingId_ =
  TargetConfigurationRequest'
    {_tcrInstanceCount = Nothing, _tcrOfferingId = pOfferingId_}


-- | The number of instances the Covertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
tcrInstanceCount :: Lens' TargetConfigurationRequest (Maybe Int)
tcrInstanceCount = lens _tcrInstanceCount (\ s a -> s{_tcrInstanceCount = a})

-- | The Convertible Reserved Instance offering ID.
tcrOfferingId :: Lens' TargetConfigurationRequest Text
tcrOfferingId = lens _tcrOfferingId (\ s a -> s{_tcrOfferingId = a})

instance NFData TargetConfigurationRequest where

instance ToQuery TargetConfigurationRequest where
        toQuery TargetConfigurationRequest'{..}
          = mconcat
              ["InstanceCount" =: _tcrInstanceCount,
               "OfferingId" =: _tcrOfferingId]

-- | Describes a load balancer target group.
--
--
--
-- /See:/ 'targetGroup' smart constructor.
newtype TargetGroup = TargetGroup'
  { _tgARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgARN' - The Amazon Resource Name (ARN) of the target group.
targetGroup
    :: Text -- ^ 'tgARN'
    -> TargetGroup
targetGroup pARN_ = TargetGroup' {_tgARN = pARN_}


-- | The Amazon Resource Name (ARN) of the target group.
tgARN :: Lens' TargetGroup Text
tgARN = lens _tgARN (\ s a -> s{_tgARN = a})

instance FromXML TargetGroup where
        parseXML x = TargetGroup' <$> (x .@ "arn")

instance NFData TargetGroup where

instance ToQuery TargetGroup where
        toQuery TargetGroup'{..} = mconcat ["Arn" =: _tgARN]

-- | Describes the target groups to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these target groups.
--
--
--
-- /See:/ 'targetGroupsConfig' smart constructor.
newtype TargetGroupsConfig = TargetGroupsConfig'
  { _tgcTargetGroups :: List1 TargetGroup
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGroupsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgcTargetGroups' - One or more target groups.
targetGroupsConfig
    :: NonEmpty TargetGroup -- ^ 'tgcTargetGroups'
    -> TargetGroupsConfig
targetGroupsConfig pTargetGroups_ =
  TargetGroupsConfig' {_tgcTargetGroups = _List1 # pTargetGroups_}


-- | One or more target groups.
tgcTargetGroups :: Lens' TargetGroupsConfig (NonEmpty TargetGroup)
tgcTargetGroups = lens _tgcTargetGroups (\ s a -> s{_tgcTargetGroups = a}) . _List1

instance FromXML TargetGroupsConfig where
        parseXML x
          = TargetGroupsConfig' <$>
              (x .@? "targetGroups" .!@ mempty >>=
                 parseXMLList1 "item")

instance NFData TargetGroupsConfig where

instance ToQuery TargetGroupsConfig where
        toQuery TargetGroupsConfig'{..}
          = mconcat
              [toQueryList "TargetGroups" _tgcTargetGroups]

-- | The total value of the new Convertible Reserved Instances.
--
--
--
-- /See:/ 'targetReservationValue' smart constructor.
data TargetReservationValue = TargetReservationValue'
  { _trvReservationValue    :: !(Maybe ReservationValue)
  , _trvTargetConfiguration :: !(Maybe TargetConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetReservationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trvReservationValue' - The total value of the Convertible Reserved Instances that make up the exchange. This is the sum of the list value, remaining upfront price, and additional upfront cost of the exchange.
--
-- * 'trvTargetConfiguration' - The configuration of the Convertible Reserved Instances that make up the exchange.
targetReservationValue
    :: TargetReservationValue
targetReservationValue =
  TargetReservationValue'
    {_trvReservationValue = Nothing, _trvTargetConfiguration = Nothing}


-- | The total value of the Convertible Reserved Instances that make up the exchange. This is the sum of the list value, remaining upfront price, and additional upfront cost of the exchange.
trvReservationValue :: Lens' TargetReservationValue (Maybe ReservationValue)
trvReservationValue = lens _trvReservationValue (\ s a -> s{_trvReservationValue = a})

-- | The configuration of the Convertible Reserved Instances that make up the exchange.
trvTargetConfiguration :: Lens' TargetReservationValue (Maybe TargetConfiguration)
trvTargetConfiguration = lens _trvTargetConfiguration (\ s a -> s{_trvTargetConfiguration = a})

instance FromXML TargetReservationValue where
        parseXML x
          = TargetReservationValue' <$>
              (x .@? "reservationValue") <*>
                (x .@? "targetConfiguration")

instance NFData TargetReservationValue where

-- | Describes the T2 instance whose credit option for CPU usage was not modified.
--
--
--
-- /See:/ 'unsuccessfulInstanceCreditSpecificationItem' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItem = UnsuccessfulInstanceCreditSpecificationItem'
  { _uicsiInstanceId :: !(Maybe Text)
  , _uicsiError :: !(Maybe UnsuccessfulInstanceCreditSpecificationItemError)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsuccessfulInstanceCreditSpecificationItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uicsiInstanceId' - The ID of the instance.
--
-- * 'uicsiError' - The applicable error for the T2 instance whose credit option for CPU usage was not modified.
unsuccessfulInstanceCreditSpecificationItem
    :: UnsuccessfulInstanceCreditSpecificationItem
unsuccessfulInstanceCreditSpecificationItem =
  UnsuccessfulInstanceCreditSpecificationItem'
    {_uicsiInstanceId = Nothing, _uicsiError = Nothing}


-- | The ID of the instance.
uicsiInstanceId :: Lens' UnsuccessfulInstanceCreditSpecificationItem (Maybe Text)
uicsiInstanceId = lens _uicsiInstanceId (\ s a -> s{_uicsiInstanceId = a})

-- | The applicable error for the T2 instance whose credit option for CPU usage was not modified.
uicsiError :: Lens' UnsuccessfulInstanceCreditSpecificationItem (Maybe UnsuccessfulInstanceCreditSpecificationItemError)
uicsiError = lens _uicsiError (\ s a -> s{_uicsiError = a})

instance FromXML
           UnsuccessfulInstanceCreditSpecificationItem
         where
        parseXML x
          = UnsuccessfulInstanceCreditSpecificationItem' <$>
              (x .@? "instanceId") <*> (x .@? "error")

instance NFData
           UnsuccessfulInstanceCreditSpecificationItem
         where

-- | Information about the error for the T2 instance whose credit option for CPU usage was not modified.
--
--
--
-- /See:/ 'unsuccessfulInstanceCreditSpecificationItemError' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItemError = UnsuccessfulInstanceCreditSpecificationItemError'
  { _uicsieCode    :: !(Maybe UnsuccessfulInstanceCreditSpecificationErrorCode)
  , _uicsieMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsuccessfulInstanceCreditSpecificationItemError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uicsieCode' - The error code.
--
-- * 'uicsieMessage' - The applicable error message.
unsuccessfulInstanceCreditSpecificationItemError
    :: UnsuccessfulInstanceCreditSpecificationItemError
unsuccessfulInstanceCreditSpecificationItemError =
  UnsuccessfulInstanceCreditSpecificationItemError'
    {_uicsieCode = Nothing, _uicsieMessage = Nothing}


-- | The error code.
uicsieCode :: Lens' UnsuccessfulInstanceCreditSpecificationItemError (Maybe UnsuccessfulInstanceCreditSpecificationErrorCode)
uicsieCode = lens _uicsieCode (\ s a -> s{_uicsieCode = a})

-- | The applicable error message.
uicsieMessage :: Lens' UnsuccessfulInstanceCreditSpecificationItemError (Maybe Text)
uicsieMessage = lens _uicsieMessage (\ s a -> s{_uicsieMessage = a})

instance FromXML
           UnsuccessfulInstanceCreditSpecificationItemError
         where
        parseXML x
          = UnsuccessfulInstanceCreditSpecificationItemError'
              <$> (x .@? "code") <*> (x .@? "message")

instance NFData
           UnsuccessfulInstanceCreditSpecificationItemError
         where

-- | Information about items that were not successfully processed in a batch call.
--
--
--
-- /See:/ 'unsuccessfulItem' smart constructor.
data UnsuccessfulItem = UnsuccessfulItem'
  { _uiResourceId :: !(Maybe Text)
  , _uiError      :: !UnsuccessfulItemError
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsuccessfulItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiResourceId' - The ID of the resource.
--
-- * 'uiError' - Information about the error.
unsuccessfulItem
    :: UnsuccessfulItemError -- ^ 'uiError'
    -> UnsuccessfulItem
unsuccessfulItem pError_ =
  UnsuccessfulItem' {_uiResourceId = Nothing, _uiError = pError_}


-- | The ID of the resource.
uiResourceId :: Lens' UnsuccessfulItem (Maybe Text)
uiResourceId = lens _uiResourceId (\ s a -> s{_uiResourceId = a})

-- | Information about the error.
uiError :: Lens' UnsuccessfulItem UnsuccessfulItemError
uiError = lens _uiError (\ s a -> s{_uiError = a})

instance FromXML UnsuccessfulItem where
        parseXML x
          = UnsuccessfulItem' <$>
              (x .@? "resourceId") <*> (x .@ "error")

instance NFData UnsuccessfulItem where

-- | Information about the error that occurred. For more information about errors, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes> .
--
--
--
-- /See:/ 'unsuccessfulItemError' smart constructor.
data UnsuccessfulItemError = UnsuccessfulItemError'
  { _uieCode    :: !Text
  , _uieMessage :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsuccessfulItemError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uieCode' - The error code.
--
-- * 'uieMessage' - The error message accompanying the error code.
unsuccessfulItemError
    :: Text -- ^ 'uieCode'
    -> Text -- ^ 'uieMessage'
    -> UnsuccessfulItemError
unsuccessfulItemError pCode_ pMessage_ =
  UnsuccessfulItemError' {_uieCode = pCode_, _uieMessage = pMessage_}


-- | The error code.
uieCode :: Lens' UnsuccessfulItemError Text
uieCode = lens _uieCode (\ s a -> s{_uieCode = a})

-- | The error message accompanying the error code.
uieMessage :: Lens' UnsuccessfulItemError Text
uieMessage = lens _uieMessage (\ s a -> s{_uieMessage = a})

instance FromXML UnsuccessfulItemError where
        parseXML x
          = UnsuccessfulItemError' <$>
              (x .@ "code") <*> (x .@ "message")

instance NFData UnsuccessfulItemError where

-- | Describes the S3 bucket for the disk image.
--
--
--
-- /See:/ 'userBucket' smart constructor.
data UserBucket = UserBucket'
  { _ubS3Key    :: !(Maybe Text)
  , _ubS3Bucket :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubS3Key' - The file name of the disk image.
--
-- * 'ubS3Bucket' - The name of the S3 bucket where the disk image is located.
userBucket
    :: UserBucket
userBucket = UserBucket' {_ubS3Key = Nothing, _ubS3Bucket = Nothing}


-- | The file name of the disk image.
ubS3Key :: Lens' UserBucket (Maybe Text)
ubS3Key = lens _ubS3Key (\ s a -> s{_ubS3Key = a})

-- | The name of the S3 bucket where the disk image is located.
ubS3Bucket :: Lens' UserBucket (Maybe Text)
ubS3Bucket = lens _ubS3Bucket (\ s a -> s{_ubS3Bucket = a})

instance NFData UserBucket where

instance ToQuery UserBucket where
        toQuery UserBucket'{..}
          = mconcat
              ["S3Key" =: _ubS3Key, "S3Bucket" =: _ubS3Bucket]

-- | Describes the S3 bucket for the disk image.
--
--
--
-- /See:/ 'userBucketDetails' smart constructor.
data UserBucketDetails = UserBucketDetails'
  { _ubdS3Key    :: !(Maybe Text)
  , _ubdS3Bucket :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserBucketDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubdS3Key' - The file name of the disk image.
--
-- * 'ubdS3Bucket' - The S3 bucket from which the disk image was created.
userBucketDetails
    :: UserBucketDetails
userBucketDetails =
  UserBucketDetails' {_ubdS3Key = Nothing, _ubdS3Bucket = Nothing}


-- | The file name of the disk image.
ubdS3Key :: Lens' UserBucketDetails (Maybe Text)
ubdS3Key = lens _ubdS3Key (\ s a -> s{_ubdS3Key = a})

-- | The S3 bucket from which the disk image was created.
ubdS3Bucket :: Lens' UserBucketDetails (Maybe Text)
ubdS3Bucket = lens _ubdS3Bucket (\ s a -> s{_ubdS3Bucket = a})

instance FromXML UserBucketDetails where
        parseXML x
          = UserBucketDetails' <$>
              (x .@? "s3Key") <*> (x .@? "s3Bucket")

instance NFData UserBucketDetails where

-- | Describes the user data for an instance.
--
--
--
-- /See:/ 'userData' smart constructor.
newtype UserData = UserData'
  { _udData :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udData' - The user data. If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
userData
    :: UserData
userData = UserData' {_udData = Nothing}


-- | The user data. If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
udData :: Lens' UserData (Maybe Text)
udData = lens _udData (\ s a -> s{_udData = a})

instance NFData UserData where

instance ToQuery UserData where
        toQuery UserData'{..} = mconcat ["Data" =: _udData]

-- | Describes a security group and AWS account ID pair.
--
--
--
-- /See:/ 'userIdGroupPair' smart constructor.
data UserIdGroupPair = UserIdGroupPair'
  { _uigpVPCPeeringConnectionId :: !(Maybe Text)
  , _uigpVPCId                  :: !(Maybe Text)
  , _uigpUserId                 :: !(Maybe Text)
  , _uigpGroupId                :: !(Maybe Text)
  , _uigpGroupName              :: !(Maybe Text)
  , _uigpDescription            :: !(Maybe Text)
  , _uigpPeeringStatus          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserIdGroupPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uigpVPCPeeringConnectionId' - The ID of the VPC peering connection, if applicable.
--
-- * 'uigpVPCId' - The ID of the VPC for the referenced security group, if applicable.
--
-- * 'uigpUserId' - The ID of an AWS account. For a referenced security group in another VPC, the account ID of the referenced security group is returned in the response. If the referenced security group is deleted, this value is not returned. [EC2-Classic] Required when adding or removing rules that reference a security group in another AWS account.
--
-- * 'uigpGroupId' - The ID of the security group.
--
-- * 'uigpGroupName' - The name of the security group. In a request, use this parameter for a security group in EC2-Classic or a default VPC only. For a security group in a nondefault VPC, use the security group ID.  For a referenced security group in another VPC, this value is not returned if the referenced security group is deleted.
--
-- * 'uigpDescription' - A description for the security group rule that references this user ID group pair. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
--
-- * 'uigpPeeringStatus' - The status of a VPC peering connection, if applicable.
userIdGroupPair
    :: UserIdGroupPair
userIdGroupPair =
  UserIdGroupPair'
    { _uigpVPCPeeringConnectionId = Nothing
    , _uigpVPCId = Nothing
    , _uigpUserId = Nothing
    , _uigpGroupId = Nothing
    , _uigpGroupName = Nothing
    , _uigpDescription = Nothing
    , _uigpPeeringStatus = Nothing
    }


-- | The ID of the VPC peering connection, if applicable.
uigpVPCPeeringConnectionId :: Lens' UserIdGroupPair (Maybe Text)
uigpVPCPeeringConnectionId = lens _uigpVPCPeeringConnectionId (\ s a -> s{_uigpVPCPeeringConnectionId = a})

-- | The ID of the VPC for the referenced security group, if applicable.
uigpVPCId :: Lens' UserIdGroupPair (Maybe Text)
uigpVPCId = lens _uigpVPCId (\ s a -> s{_uigpVPCId = a})

-- | The ID of an AWS account. For a referenced security group in another VPC, the account ID of the referenced security group is returned in the response. If the referenced security group is deleted, this value is not returned. [EC2-Classic] Required when adding or removing rules that reference a security group in another AWS account.
uigpUserId :: Lens' UserIdGroupPair (Maybe Text)
uigpUserId = lens _uigpUserId (\ s a -> s{_uigpUserId = a})

-- | The ID of the security group.
uigpGroupId :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupId = lens _uigpGroupId (\ s a -> s{_uigpGroupId = a})

-- | The name of the security group. In a request, use this parameter for a security group in EC2-Classic or a default VPC only. For a security group in a nondefault VPC, use the security group ID.  For a referenced security group in another VPC, this value is not returned if the referenced security group is deleted.
uigpGroupName :: Lens' UserIdGroupPair (Maybe Text)
uigpGroupName = lens _uigpGroupName (\ s a -> s{_uigpGroupName = a})

-- | A description for the security group rule that references this user ID group pair. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
uigpDescription :: Lens' UserIdGroupPair (Maybe Text)
uigpDescription = lens _uigpDescription (\ s a -> s{_uigpDescription = a})

-- | The status of a VPC peering connection, if applicable.
uigpPeeringStatus :: Lens' UserIdGroupPair (Maybe Text)
uigpPeeringStatus = lens _uigpPeeringStatus (\ s a -> s{_uigpPeeringStatus = a})

instance FromXML UserIdGroupPair where
        parseXML x
          = UserIdGroupPair' <$>
              (x .@? "vpcPeeringConnectionId") <*> (x .@? "vpcId")
                <*> (x .@? "userId")
                <*> (x .@? "groupId")
                <*> (x .@? "groupName")
                <*> (x .@? "description")
                <*> (x .@? "peeringStatus")

instance NFData UserIdGroupPair where

instance ToQuery UserIdGroupPair where
        toQuery UserIdGroupPair'{..}
          = mconcat
              ["VpcPeeringConnectionId" =:
                 _uigpVPCPeeringConnectionId,
               "VpcId" =: _uigpVPCId, "UserId" =: _uigpUserId,
               "GroupId" =: _uigpGroupId,
               "GroupName" =: _uigpGroupName,
               "Description" =: _uigpDescription,
               "PeeringStatus" =: _uigpPeeringStatus]

-- | Describes telemetry for a VPN tunnel.
--
--
--
-- /See:/ 'vgwTelemetry' smart constructor.
data VGWTelemetry = VGWTelemetry'
  { _vtStatus             :: !(Maybe TelemetryStatus)
  , _vtOutsideIPAddress   :: !(Maybe Text)
  , _vtLastStatusChange   :: !(Maybe ISO8601)
  , _vtAcceptedRouteCount :: !(Maybe Int)
  , _vtStatusMessage      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VGWTelemetry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtStatus' - The status of the VPN tunnel.
--
-- * 'vtOutsideIPAddress' - The Internet-routable IP address of the virtual private gateway's outside interface.
--
-- * 'vtLastStatusChange' - The date and time of the last change in status.
--
-- * 'vtAcceptedRouteCount' - The number of accepted routes.
--
-- * 'vtStatusMessage' - If an error occurs, a description of the error.
vgwTelemetry
    :: VGWTelemetry
vgwTelemetry =
  VGWTelemetry'
    { _vtStatus = Nothing
    , _vtOutsideIPAddress = Nothing
    , _vtLastStatusChange = Nothing
    , _vtAcceptedRouteCount = Nothing
    , _vtStatusMessage = Nothing
    }


-- | The status of the VPN tunnel.
vtStatus :: Lens' VGWTelemetry (Maybe TelemetryStatus)
vtStatus = lens _vtStatus (\ s a -> s{_vtStatus = a})

-- | The Internet-routable IP address of the virtual private gateway's outside interface.
vtOutsideIPAddress :: Lens' VGWTelemetry (Maybe Text)
vtOutsideIPAddress = lens _vtOutsideIPAddress (\ s a -> s{_vtOutsideIPAddress = a})

-- | The date and time of the last change in status.
vtLastStatusChange :: Lens' VGWTelemetry (Maybe UTCTime)
vtLastStatusChange = lens _vtLastStatusChange (\ s a -> s{_vtLastStatusChange = a}) . mapping _Time

-- | The number of accepted routes.
vtAcceptedRouteCount :: Lens' VGWTelemetry (Maybe Int)
vtAcceptedRouteCount = lens _vtAcceptedRouteCount (\ s a -> s{_vtAcceptedRouteCount = a})

-- | If an error occurs, a description of the error.
vtStatusMessage :: Lens' VGWTelemetry (Maybe Text)
vtStatusMessage = lens _vtStatusMessage (\ s a -> s{_vtStatusMessage = a})

instance FromXML VGWTelemetry where
        parseXML x
          = VGWTelemetry' <$>
              (x .@? "status") <*> (x .@? "outsideIpAddress") <*>
                (x .@? "lastStatusChange")
                <*> (x .@? "acceptedRouteCount")
                <*> (x .@? "statusMessage")

instance NFData VGWTelemetry where

-- | Describes a VPC.
--
--
--
-- /See:/ 'vpc' smart constructor.
data VPC = VPC'
  { _vpcIPv6CidrBlockAssociationSet :: !(Maybe [VPCIPv6CidrBlockAssociation])
  , _vpcCidrBlockAssociationSet     :: !(Maybe [VPCCidrBlockAssociation])
  , _vpcTags                        :: !(Maybe [Tag])
  , _vpcIsDefault                   :: !(Maybe Bool)
  , _vpcCidrBlock                   :: !Text
  , _vpcDHCPOptionsId               :: !Text
  , _vpcInstanceTenancy             :: !Tenancy
  , _vpcState                       :: !VPCState
  , _vpcVPCId                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcIPv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the VPC.
--
-- * 'vpcCidrBlockAssociationSet' - Information about the IPv4 CIDR blocks associated with the VPC.
--
-- * 'vpcTags' - Any tags assigned to the VPC.
--
-- * 'vpcIsDefault' - Indicates whether the VPC is the default VPC.
--
-- * 'vpcCidrBlock' - The primary IPv4 CIDR block for the VPC.
--
-- * 'vpcDHCPOptionsId' - The ID of the set of DHCP options you've associated with the VPC (or @default@ if the default options are associated with the VPC).
--
-- * 'vpcInstanceTenancy' - The allowed tenancy of instances launched into the VPC.
--
-- * 'vpcState' - The current state of the VPC.
--
-- * 'vpcVPCId' - The ID of the VPC.
vpc
    :: Text -- ^ 'vpcCidrBlock'
    -> Text -- ^ 'vpcDHCPOptionsId'
    -> Tenancy -- ^ 'vpcInstanceTenancy'
    -> VPCState -- ^ 'vpcState'
    -> Text -- ^ 'vpcVPCId'
    -> VPC
vpc pCidrBlock_ pDHCPOptionsId_ pInstanceTenancy_ pState_ pVPCId_ =
  VPC'
    { _vpcIPv6CidrBlockAssociationSet = Nothing
    , _vpcCidrBlockAssociationSet = Nothing
    , _vpcTags = Nothing
    , _vpcIsDefault = Nothing
    , _vpcCidrBlock = pCidrBlock_
    , _vpcDHCPOptionsId = pDHCPOptionsId_
    , _vpcInstanceTenancy = pInstanceTenancy_
    , _vpcState = pState_
    , _vpcVPCId = pVPCId_
    }


-- | Information about the IPv6 CIDR blocks associated with the VPC.
vpcIPv6CidrBlockAssociationSet :: Lens' VPC [VPCIPv6CidrBlockAssociation]
vpcIPv6CidrBlockAssociationSet = lens _vpcIPv6CidrBlockAssociationSet (\ s a -> s{_vpcIPv6CidrBlockAssociationSet = a}) . _Default . _Coerce

-- | Information about the IPv4 CIDR blocks associated with the VPC.
vpcCidrBlockAssociationSet :: Lens' VPC [VPCCidrBlockAssociation]
vpcCidrBlockAssociationSet = lens _vpcCidrBlockAssociationSet (\ s a -> s{_vpcCidrBlockAssociationSet = a}) . _Default . _Coerce

-- | Any tags assigned to the VPC.
vpcTags :: Lens' VPC [Tag]
vpcTags = lens _vpcTags (\ s a -> s{_vpcTags = a}) . _Default . _Coerce

-- | Indicates whether the VPC is the default VPC.
vpcIsDefault :: Lens' VPC (Maybe Bool)
vpcIsDefault = lens _vpcIsDefault (\ s a -> s{_vpcIsDefault = a})

-- | The primary IPv4 CIDR block for the VPC.
vpcCidrBlock :: Lens' VPC Text
vpcCidrBlock = lens _vpcCidrBlock (\ s a -> s{_vpcCidrBlock = a})

-- | The ID of the set of DHCP options you've associated with the VPC (or @default@ if the default options are associated with the VPC).
vpcDHCPOptionsId :: Lens' VPC Text
vpcDHCPOptionsId = lens _vpcDHCPOptionsId (\ s a -> s{_vpcDHCPOptionsId = a})

-- | The allowed tenancy of instances launched into the VPC.
vpcInstanceTenancy :: Lens' VPC Tenancy
vpcInstanceTenancy = lens _vpcInstanceTenancy (\ s a -> s{_vpcInstanceTenancy = a})

-- | The current state of the VPC.
vpcState :: Lens' VPC VPCState
vpcState = lens _vpcState (\ s a -> s{_vpcState = a})

-- | The ID of the VPC.
vpcVPCId :: Lens' VPC Text
vpcVPCId = lens _vpcVPCId (\ s a -> s{_vpcVPCId = a})

instance FromXML VPC where
        parseXML x
          = VPC' <$>
              (x .@? "ipv6CidrBlockAssociationSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*>
                (x .@? "cidrBlockAssociationSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "isDefault")
                <*> (x .@ "cidrBlock")
                <*> (x .@ "dhcpOptionsId")
                <*> (x .@ "instanceTenancy")
                <*> (x .@ "state")
                <*> (x .@ "vpcId")

instance NFData VPC where

-- | Describes an attachment between a virtual private gateway and a VPC.
--
--
--
-- /See:/ 'vpcAttachment' smart constructor.
data VPCAttachment = VPCAttachment'
  { _vaState :: !(Maybe AttachmentStatus)
  , _vaVPCId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vaState' - The current state of the attachment.
--
-- * 'vaVPCId' - The ID of the VPC.
vpcAttachment
    :: VPCAttachment
vpcAttachment = VPCAttachment' {_vaState = Nothing, _vaVPCId = Nothing}


-- | The current state of the attachment.
vaState :: Lens' VPCAttachment (Maybe AttachmentStatus)
vaState = lens _vaState (\ s a -> s{_vaState = a})

-- | The ID of the VPC.
vaVPCId :: Lens' VPCAttachment (Maybe Text)
vaVPCId = lens _vaVPCId (\ s a -> s{_vaVPCId = a})

instance FromXML VPCAttachment where
        parseXML x
          = VPCAttachment' <$>
              (x .@? "state") <*> (x .@? "vpcId")

instance NFData VPCAttachment where

-- | Describes an IPv4 CIDR block associated with a VPC.
--
--
--
-- /See:/ 'vpcCidrBlockAssociation' smart constructor.
data VPCCidrBlockAssociation = VPCCidrBlockAssociation'
  { _vcbaAssociationId  :: !(Maybe Text)
  , _vcbaCidrBlockState :: !(Maybe VPCCidrBlockState)
  , _vcbaCidrBlock      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCCidrBlockAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcbaAssociationId' - The association ID for the IPv4 CIDR block.
--
-- * 'vcbaCidrBlockState' - Information about the state of the CIDR block.
--
-- * 'vcbaCidrBlock' - The IPv4 CIDR block.
vpcCidrBlockAssociation
    :: VPCCidrBlockAssociation
vpcCidrBlockAssociation =
  VPCCidrBlockAssociation'
    { _vcbaAssociationId = Nothing
    , _vcbaCidrBlockState = Nothing
    , _vcbaCidrBlock = Nothing
    }


-- | The association ID for the IPv4 CIDR block.
vcbaAssociationId :: Lens' VPCCidrBlockAssociation (Maybe Text)
vcbaAssociationId = lens _vcbaAssociationId (\ s a -> s{_vcbaAssociationId = a})

-- | Information about the state of the CIDR block.
vcbaCidrBlockState :: Lens' VPCCidrBlockAssociation (Maybe VPCCidrBlockState)
vcbaCidrBlockState = lens _vcbaCidrBlockState (\ s a -> s{_vcbaCidrBlockState = a})

-- | The IPv4 CIDR block.
vcbaCidrBlock :: Lens' VPCCidrBlockAssociation (Maybe Text)
vcbaCidrBlock = lens _vcbaCidrBlock (\ s a -> s{_vcbaCidrBlock = a})

instance FromXML VPCCidrBlockAssociation where
        parseXML x
          = VPCCidrBlockAssociation' <$>
              (x .@? "associationId") <*> (x .@? "cidrBlockState")
                <*> (x .@? "cidrBlock")

instance NFData VPCCidrBlockAssociation where

-- | Describes the state of a CIDR block.
--
--
--
-- /See:/ 'vpcCidrBlockState' smart constructor.
data VPCCidrBlockState = VPCCidrBlockState'
  { _vcbsState         :: !(Maybe VPCCidrBlockStateCode)
  , _vcbsStatusMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCCidrBlockState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcbsState' - The state of the CIDR block.
--
-- * 'vcbsStatusMessage' - A message about the status of the CIDR block, if applicable.
vpcCidrBlockState
    :: VPCCidrBlockState
vpcCidrBlockState =
  VPCCidrBlockState' {_vcbsState = Nothing, _vcbsStatusMessage = Nothing}


-- | The state of the CIDR block.
vcbsState :: Lens' VPCCidrBlockState (Maybe VPCCidrBlockStateCode)
vcbsState = lens _vcbsState (\ s a -> s{_vcbsState = a})

-- | A message about the status of the CIDR block, if applicable.
vcbsStatusMessage :: Lens' VPCCidrBlockState (Maybe Text)
vcbsStatusMessage = lens _vcbsStatusMessage (\ s a -> s{_vcbsStatusMessage = a})

instance FromXML VPCCidrBlockState where
        parseXML x
          = VPCCidrBlockState' <$>
              (x .@? "state") <*> (x .@? "statusMessage")

instance NFData VPCCidrBlockState where

-- | Describes whether a VPC is enabled for ClassicLink.
--
--
--
-- /See:/ 'vpcClassicLink' smart constructor.
data VPCClassicLink = VPCClassicLink'
  { _vclVPCId              :: !(Maybe Text)
  , _vclTags               :: !(Maybe [Tag])
  , _vclClassicLinkEnabled :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCClassicLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vclVPCId' - The ID of the VPC.
--
-- * 'vclTags' - Any tags assigned to the VPC.
--
-- * 'vclClassicLinkEnabled' - Indicates whether the VPC is enabled for ClassicLink.
vpcClassicLink
    :: VPCClassicLink
vpcClassicLink =
  VPCClassicLink'
    {_vclVPCId = Nothing, _vclTags = Nothing, _vclClassicLinkEnabled = Nothing}


-- | The ID of the VPC.
vclVPCId :: Lens' VPCClassicLink (Maybe Text)
vclVPCId = lens _vclVPCId (\ s a -> s{_vclVPCId = a})

-- | Any tags assigned to the VPC.
vclTags :: Lens' VPCClassicLink [Tag]
vclTags = lens _vclTags (\ s a -> s{_vclTags = a}) . _Default . _Coerce

-- | Indicates whether the VPC is enabled for ClassicLink.
vclClassicLinkEnabled :: Lens' VPCClassicLink (Maybe Bool)
vclClassicLinkEnabled = lens _vclClassicLinkEnabled (\ s a -> s{_vclClassicLinkEnabled = a})

instance FromXML VPCClassicLink where
        parseXML x
          = VPCClassicLink' <$>
              (x .@? "vpcId") <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "classicLinkEnabled")

instance NFData VPCClassicLink where

-- | Describes a VPC endpoint.
--
--
--
-- /See:/ 'vpcEndpoint' smart constructor.
data VPCEndpoint = VPCEndpoint'
  { _veGroups              :: !(Maybe [SecurityGroupIdentifier])
  , _veState               :: !(Maybe State)
  , _vePolicyDocument      :: !(Maybe Text)
  , _veSubnetIds           :: !(Maybe [Text])
  , _veNetworkInterfaceIds :: !(Maybe [Text])
  , _veVPCId               :: !(Maybe Text)
  , _veDNSEntries          :: !(Maybe [DNSEntry])
  , _veVPCEndpointType     :: !(Maybe VPCEndpointType)
  , _vePrivateDNSEnabled   :: !(Maybe Bool)
  , _veCreationTimestamp   :: !(Maybe ISO8601)
  , _veServiceName         :: !(Maybe Text)
  , _veVPCEndpointId       :: !(Maybe Text)
  , _veRouteTableIds       :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veGroups' - (Interface endpoint) Information about the security groups associated with the network interface.
--
-- * 'veState' - The state of the VPC endpoint.
--
-- * 'vePolicyDocument' - The policy document associated with the endpoint, if applicable.
--
-- * 'veSubnetIds' - (Interface endpoint) One or more subnets in which the endpoint is located.
--
-- * 'veNetworkInterfaceIds' - (Interface endpoint) One or more network interfaces for the endpoint.
--
-- * 'veVPCId' - The ID of the VPC to which the endpoint is associated.
--
-- * 'veDNSEntries' - (Interface endpoint) The DNS entries for the endpoint.
--
-- * 'veVPCEndpointType' - The type of endpoint.
--
-- * 'vePrivateDNSEnabled' - (Interface endpoint) Indicates whether the VPC is associated with a private hosted zone.
--
-- * 'veCreationTimestamp' - The date and time the VPC endpoint was created.
--
-- * 'veServiceName' - The name of the service to which the endpoint is associated.
--
-- * 'veVPCEndpointId' - The ID of the VPC endpoint.
--
-- * 'veRouteTableIds' - (Gateway endpoint) One or more route tables associated with the endpoint.
vpcEndpoint
    :: VPCEndpoint
vpcEndpoint =
  VPCEndpoint'
    { _veGroups = Nothing
    , _veState = Nothing
    , _vePolicyDocument = Nothing
    , _veSubnetIds = Nothing
    , _veNetworkInterfaceIds = Nothing
    , _veVPCId = Nothing
    , _veDNSEntries = Nothing
    , _veVPCEndpointType = Nothing
    , _vePrivateDNSEnabled = Nothing
    , _veCreationTimestamp = Nothing
    , _veServiceName = Nothing
    , _veVPCEndpointId = Nothing
    , _veRouteTableIds = Nothing
    }


-- | (Interface endpoint) Information about the security groups associated with the network interface.
veGroups :: Lens' VPCEndpoint [SecurityGroupIdentifier]
veGroups = lens _veGroups (\ s a -> s{_veGroups = a}) . _Default . _Coerce

-- | The state of the VPC endpoint.
veState :: Lens' VPCEndpoint (Maybe State)
veState = lens _veState (\ s a -> s{_veState = a})

-- | The policy document associated with the endpoint, if applicable.
vePolicyDocument :: Lens' VPCEndpoint (Maybe Text)
vePolicyDocument = lens _vePolicyDocument (\ s a -> s{_vePolicyDocument = a})

-- | (Interface endpoint) One or more subnets in which the endpoint is located.
veSubnetIds :: Lens' VPCEndpoint [Text]
veSubnetIds = lens _veSubnetIds (\ s a -> s{_veSubnetIds = a}) . _Default . _Coerce

-- | (Interface endpoint) One or more network interfaces for the endpoint.
veNetworkInterfaceIds :: Lens' VPCEndpoint [Text]
veNetworkInterfaceIds = lens _veNetworkInterfaceIds (\ s a -> s{_veNetworkInterfaceIds = a}) . _Default . _Coerce

-- | The ID of the VPC to which the endpoint is associated.
veVPCId :: Lens' VPCEndpoint (Maybe Text)
veVPCId = lens _veVPCId (\ s a -> s{_veVPCId = a})

-- | (Interface endpoint) The DNS entries for the endpoint.
veDNSEntries :: Lens' VPCEndpoint [DNSEntry]
veDNSEntries = lens _veDNSEntries (\ s a -> s{_veDNSEntries = a}) . _Default . _Coerce

-- | The type of endpoint.
veVPCEndpointType :: Lens' VPCEndpoint (Maybe VPCEndpointType)
veVPCEndpointType = lens _veVPCEndpointType (\ s a -> s{_veVPCEndpointType = a})

-- | (Interface endpoint) Indicates whether the VPC is associated with a private hosted zone.
vePrivateDNSEnabled :: Lens' VPCEndpoint (Maybe Bool)
vePrivateDNSEnabled = lens _vePrivateDNSEnabled (\ s a -> s{_vePrivateDNSEnabled = a})

-- | The date and time the VPC endpoint was created.
veCreationTimestamp :: Lens' VPCEndpoint (Maybe UTCTime)
veCreationTimestamp = lens _veCreationTimestamp (\ s a -> s{_veCreationTimestamp = a}) . mapping _Time

-- | The name of the service to which the endpoint is associated.
veServiceName :: Lens' VPCEndpoint (Maybe Text)
veServiceName = lens _veServiceName (\ s a -> s{_veServiceName = a})

-- | The ID of the VPC endpoint.
veVPCEndpointId :: Lens' VPCEndpoint (Maybe Text)
veVPCEndpointId = lens _veVPCEndpointId (\ s a -> s{_veVPCEndpointId = a})

-- | (Gateway endpoint) One or more route tables associated with the endpoint.
veRouteTableIds :: Lens' VPCEndpoint [Text]
veRouteTableIds = lens _veRouteTableIds (\ s a -> s{_veRouteTableIds = a}) . _Default . _Coerce

instance FromXML VPCEndpoint where
        parseXML x
          = VPCEndpoint' <$>
              (x .@? "groupSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "state")
                <*> (x .@? "policyDocument")
                <*>
                (x .@? "subnetIdSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "networkInterfaceIdSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "vpcId")
                <*>
                (x .@? "dnsEntrySet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "vpcEndpointType")
                <*> (x .@? "privateDnsEnabled")
                <*> (x .@? "creationTimestamp")
                <*> (x .@? "serviceName")
                <*> (x .@? "vpcEndpointId")
                <*>
                (x .@? "routeTableIdSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData VPCEndpoint where

-- | Describes a VPC endpoint connection to a service.
--
--
--
-- /See:/ 'vpcEndpointConnection' smart constructor.
data VPCEndpointConnection = VPCEndpointConnection'
  { _vecVPCEndpointOwner  :: !(Maybe Text)
  , _vecVPCEndpointState  :: !(Maybe State)
  , _vecCreationTimestamp :: !(Maybe ISO8601)
  , _vecServiceId         :: !(Maybe Text)
  , _vecVPCEndpointId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCEndpointConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vecVPCEndpointOwner' - The AWS account ID of the owner of the VPC endpoint.
--
-- * 'vecVPCEndpointState' - The state of the VPC endpoint.
--
-- * 'vecCreationTimestamp' - The date and time the VPC endpoint was created.
--
-- * 'vecServiceId' - The ID of the service to which the endpoint is connected.
--
-- * 'vecVPCEndpointId' - The ID of the VPC endpoint.
vpcEndpointConnection
    :: VPCEndpointConnection
vpcEndpointConnection =
  VPCEndpointConnection'
    { _vecVPCEndpointOwner = Nothing
    , _vecVPCEndpointState = Nothing
    , _vecCreationTimestamp = Nothing
    , _vecServiceId = Nothing
    , _vecVPCEndpointId = Nothing
    }


-- | The AWS account ID of the owner of the VPC endpoint.
vecVPCEndpointOwner :: Lens' VPCEndpointConnection (Maybe Text)
vecVPCEndpointOwner = lens _vecVPCEndpointOwner (\ s a -> s{_vecVPCEndpointOwner = a})

-- | The state of the VPC endpoint.
vecVPCEndpointState :: Lens' VPCEndpointConnection (Maybe State)
vecVPCEndpointState = lens _vecVPCEndpointState (\ s a -> s{_vecVPCEndpointState = a})

-- | The date and time the VPC endpoint was created.
vecCreationTimestamp :: Lens' VPCEndpointConnection (Maybe UTCTime)
vecCreationTimestamp = lens _vecCreationTimestamp (\ s a -> s{_vecCreationTimestamp = a}) . mapping _Time

-- | The ID of the service to which the endpoint is connected.
vecServiceId :: Lens' VPCEndpointConnection (Maybe Text)
vecServiceId = lens _vecServiceId (\ s a -> s{_vecServiceId = a})

-- | The ID of the VPC endpoint.
vecVPCEndpointId :: Lens' VPCEndpointConnection (Maybe Text)
vecVPCEndpointId = lens _vecVPCEndpointId (\ s a -> s{_vecVPCEndpointId = a})

instance FromXML VPCEndpointConnection where
        parseXML x
          = VPCEndpointConnection' <$>
              (x .@? "vpcEndpointOwner") <*>
                (x .@? "vpcEndpointState")
                <*> (x .@? "creationTimestamp")
                <*> (x .@? "serviceId")
                <*> (x .@? "vpcEndpointId")

instance NFData VPCEndpointConnection where

-- | Describes an IPv6 CIDR block associated with a VPC.
--
--
--
-- /See:/ 'vpcIPv6CidrBlockAssociation' smart constructor.
data VPCIPv6CidrBlockAssociation = VPCIPv6CidrBlockAssociation'
  { _vicbaAssociationId      :: !(Maybe Text)
  , _vicbaIPv6CidrBlock      :: !(Maybe Text)
  , _vicbaIPv6CidrBlockState :: !(Maybe VPCCidrBlockState)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCIPv6CidrBlockAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vicbaAssociationId' - The association ID for the IPv6 CIDR block.
--
-- * 'vicbaIPv6CidrBlock' - The IPv6 CIDR block.
--
-- * 'vicbaIPv6CidrBlockState' - Information about the state of the CIDR block.
vpcIPv6CidrBlockAssociation
    :: VPCIPv6CidrBlockAssociation
vpcIPv6CidrBlockAssociation =
  VPCIPv6CidrBlockAssociation'
    { _vicbaAssociationId = Nothing
    , _vicbaIPv6CidrBlock = Nothing
    , _vicbaIPv6CidrBlockState = Nothing
    }


-- | The association ID for the IPv6 CIDR block.
vicbaAssociationId :: Lens' VPCIPv6CidrBlockAssociation (Maybe Text)
vicbaAssociationId = lens _vicbaAssociationId (\ s a -> s{_vicbaAssociationId = a})

-- | The IPv6 CIDR block.
vicbaIPv6CidrBlock :: Lens' VPCIPv6CidrBlockAssociation (Maybe Text)
vicbaIPv6CidrBlock = lens _vicbaIPv6CidrBlock (\ s a -> s{_vicbaIPv6CidrBlock = a})

-- | Information about the state of the CIDR block.
vicbaIPv6CidrBlockState :: Lens' VPCIPv6CidrBlockAssociation (Maybe VPCCidrBlockState)
vicbaIPv6CidrBlockState = lens _vicbaIPv6CidrBlockState (\ s a -> s{_vicbaIPv6CidrBlockState = a})

instance FromXML VPCIPv6CidrBlockAssociation where
        parseXML x
          = VPCIPv6CidrBlockAssociation' <$>
              (x .@? "associationId") <*> (x .@? "ipv6CidrBlock")
                <*> (x .@? "ipv6CidrBlockState")

instance NFData VPCIPv6CidrBlockAssociation where

-- | Describes a VPC peering connection.
--
--
--
-- /See:/ 'vpcPeeringConnection' smart constructor.
data VPCPeeringConnection = VPCPeeringConnection'
  { _vpcpcVPCPeeringConnectionId :: !(Maybe Text)
  , _vpcpcStatus                 :: !(Maybe VPCPeeringConnectionStateReason)
  , _vpcpcAccepterVPCInfo        :: !(Maybe VPCPeeringConnectionVPCInfo)
  , _vpcpcRequesterVPCInfo       :: !(Maybe VPCPeeringConnectionVPCInfo)
  , _vpcpcExpirationTime         :: !(Maybe ISO8601)
  , _vpcpcTags                   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCPeeringConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcpcVPCPeeringConnectionId' - The ID of the VPC peering connection.
--
-- * 'vpcpcStatus' - The status of the VPC peering connection.
--
-- * 'vpcpcAccepterVPCInfo' - Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- * 'vpcpcRequesterVPCInfo' - Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- * 'vpcpcExpirationTime' - The time that an unaccepted VPC peering connection will expire.
--
-- * 'vpcpcTags' - Any tags assigned to the resource.
vpcPeeringConnection
    :: VPCPeeringConnection
vpcPeeringConnection =
  VPCPeeringConnection'
    { _vpcpcVPCPeeringConnectionId = Nothing
    , _vpcpcStatus = Nothing
    , _vpcpcAccepterVPCInfo = Nothing
    , _vpcpcRequesterVPCInfo = Nothing
    , _vpcpcExpirationTime = Nothing
    , _vpcpcTags = Nothing
    }


-- | The ID of the VPC peering connection.
vpcpcVPCPeeringConnectionId :: Lens' VPCPeeringConnection (Maybe Text)
vpcpcVPCPeeringConnectionId = lens _vpcpcVPCPeeringConnectionId (\ s a -> s{_vpcpcVPCPeeringConnectionId = a})

-- | The status of the VPC peering connection.
vpcpcStatus :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionStateReason)
vpcpcStatus = lens _vpcpcStatus (\ s a -> s{_vpcpcStatus = a})

-- | Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
vpcpcAccepterVPCInfo :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionVPCInfo)
vpcpcAccepterVPCInfo = lens _vpcpcAccepterVPCInfo (\ s a -> s{_vpcpcAccepterVPCInfo = a})

-- | Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
vpcpcRequesterVPCInfo :: Lens' VPCPeeringConnection (Maybe VPCPeeringConnectionVPCInfo)
vpcpcRequesterVPCInfo = lens _vpcpcRequesterVPCInfo (\ s a -> s{_vpcpcRequesterVPCInfo = a})

-- | The time that an unaccepted VPC peering connection will expire.
vpcpcExpirationTime :: Lens' VPCPeeringConnection (Maybe UTCTime)
vpcpcExpirationTime = lens _vpcpcExpirationTime (\ s a -> s{_vpcpcExpirationTime = a}) . mapping _Time

-- | Any tags assigned to the resource.
vpcpcTags :: Lens' VPCPeeringConnection [Tag]
vpcpcTags = lens _vpcpcTags (\ s a -> s{_vpcpcTags = a}) . _Default . _Coerce

instance FromXML VPCPeeringConnection where
        parseXML x
          = VPCPeeringConnection' <$>
              (x .@? "vpcPeeringConnectionId") <*> (x .@? "status")
                <*> (x .@? "accepterVpcInfo")
                <*> (x .@? "requesterVpcInfo")
                <*> (x .@? "expirationTime")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData VPCPeeringConnection where

-- | Describes the VPC peering connection options.
--
--
--
-- /See:/ 'vpcPeeringConnectionOptionsDescription' smart constructor.
data VPCPeeringConnectionOptionsDescription = VPCPeeringConnectionOptionsDescription'
  { _vpcodAllowEgressFromLocalVPCToRemoteClassicLink :: !(Maybe Bool)
  , _vpcodAllowEgressFromLocalClassicLinkToRemoteVPC :: !(Maybe Bool)
  , _vpcodAllowDNSResolutionFromRemoteVPC            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCPeeringConnectionOptionsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcodAllowEgressFromLocalVPCToRemoteClassicLink' - Indicates whether a local VPC can communicate with a ClassicLink connection in the peer VPC over the VPC peering connection.
--
-- * 'vpcodAllowEgressFromLocalClassicLinkToRemoteVPC' - Indicates whether a local ClassicLink connection can communicate with the peer VPC over the VPC peering connection.
--
-- * 'vpcodAllowDNSResolutionFromRemoteVPC' - Indicates whether a local VPC can resolve public DNS hostnames to private IP addresses when queried from instances in a peer VPC.
vpcPeeringConnectionOptionsDescription
    :: VPCPeeringConnectionOptionsDescription
vpcPeeringConnectionOptionsDescription =
  VPCPeeringConnectionOptionsDescription'
    { _vpcodAllowEgressFromLocalVPCToRemoteClassicLink = Nothing
    , _vpcodAllowEgressFromLocalClassicLinkToRemoteVPC = Nothing
    , _vpcodAllowDNSResolutionFromRemoteVPC = Nothing
    }


-- | Indicates whether a local VPC can communicate with a ClassicLink connection in the peer VPC over the VPC peering connection.
vpcodAllowEgressFromLocalVPCToRemoteClassicLink :: Lens' VPCPeeringConnectionOptionsDescription (Maybe Bool)
vpcodAllowEgressFromLocalVPCToRemoteClassicLink = lens _vpcodAllowEgressFromLocalVPCToRemoteClassicLink (\ s a -> s{_vpcodAllowEgressFromLocalVPCToRemoteClassicLink = a})

-- | Indicates whether a local ClassicLink connection can communicate with the peer VPC over the VPC peering connection.
vpcodAllowEgressFromLocalClassicLinkToRemoteVPC :: Lens' VPCPeeringConnectionOptionsDescription (Maybe Bool)
vpcodAllowEgressFromLocalClassicLinkToRemoteVPC = lens _vpcodAllowEgressFromLocalClassicLinkToRemoteVPC (\ s a -> s{_vpcodAllowEgressFromLocalClassicLinkToRemoteVPC = a})

-- | Indicates whether a local VPC can resolve public DNS hostnames to private IP addresses when queried from instances in a peer VPC.
vpcodAllowDNSResolutionFromRemoteVPC :: Lens' VPCPeeringConnectionOptionsDescription (Maybe Bool)
vpcodAllowDNSResolutionFromRemoteVPC = lens _vpcodAllowDNSResolutionFromRemoteVPC (\ s a -> s{_vpcodAllowDNSResolutionFromRemoteVPC = a})

instance FromXML
           VPCPeeringConnectionOptionsDescription
         where
        parseXML x
          = VPCPeeringConnectionOptionsDescription' <$>
              (x .@? "allowEgressFromLocalVpcToRemoteClassicLink")
                <*>
                (x .@? "allowEgressFromLocalClassicLinkToRemoteVpc")
                <*> (x .@? "allowDnsResolutionFromRemoteVpc")

instance NFData
           VPCPeeringConnectionOptionsDescription
         where

-- | Describes the status of a VPC peering connection.
--
--
--
-- /See:/ 'vpcPeeringConnectionStateReason' smart constructor.
data VPCPeeringConnectionStateReason = VPCPeeringConnectionStateReason'
  { _vpcsrCode    :: !(Maybe VPCPeeringConnectionStateReasonCode)
  , _vpcsrMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCPeeringConnectionStateReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcsrCode' - The status of the VPC peering connection.
--
-- * 'vpcsrMessage' - A message that provides more information about the status, if applicable.
vpcPeeringConnectionStateReason
    :: VPCPeeringConnectionStateReason
vpcPeeringConnectionStateReason =
  VPCPeeringConnectionStateReason'
    {_vpcsrCode = Nothing, _vpcsrMessage = Nothing}


-- | The status of the VPC peering connection.
vpcsrCode :: Lens' VPCPeeringConnectionStateReason (Maybe VPCPeeringConnectionStateReasonCode)
vpcsrCode = lens _vpcsrCode (\ s a -> s{_vpcsrCode = a})

-- | A message that provides more information about the status, if applicable.
vpcsrMessage :: Lens' VPCPeeringConnectionStateReason (Maybe Text)
vpcsrMessage = lens _vpcsrMessage (\ s a -> s{_vpcsrMessage = a})

instance FromXML VPCPeeringConnectionStateReason
         where
        parseXML x
          = VPCPeeringConnectionStateReason' <$>
              (x .@? "code") <*> (x .@? "message")

instance NFData VPCPeeringConnectionStateReason where

-- | Describes a VPC in a VPC peering connection.
--
--
--
-- /See:/ 'vpcPeeringConnectionVPCInfo' smart constructor.
data VPCPeeringConnectionVPCInfo = VPCPeeringConnectionVPCInfo'
  { _vpcviCidrBlockSet     :: !(Maybe [CidrBlock])
  , _vpcviVPCId            :: !(Maybe Text)
  , _vpcviOwnerId          :: !(Maybe Text)
  , _vpcviPeeringOptions   :: !(Maybe VPCPeeringConnectionOptionsDescription)
  , _vpcviCidrBlock        :: !(Maybe Text)
  , _vpcviRegion           :: !(Maybe Text)
  , _vpcviIPv6CidrBlockSet :: !(Maybe [IPv6CidrBlock])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCPeeringConnectionVPCInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcviCidrBlockSet' - Information about the IPv4 CIDR blocks for the VPC.
--
-- * 'vpcviVPCId' - The ID of the VPC.
--
-- * 'vpcviOwnerId' - The AWS account ID of the VPC owner.
--
-- * 'vpcviPeeringOptions' - Information about the VPC peering connection options for the accepter or requester VPC.
--
-- * 'vpcviCidrBlock' - The IPv4 CIDR block for the VPC.
--
-- * 'vpcviRegion' - The region in which the VPC is located.
--
-- * 'vpcviIPv6CidrBlockSet' - The IPv6 CIDR block for the VPC.
vpcPeeringConnectionVPCInfo
    :: VPCPeeringConnectionVPCInfo
vpcPeeringConnectionVPCInfo =
  VPCPeeringConnectionVPCInfo'
    { _vpcviCidrBlockSet = Nothing
    , _vpcviVPCId = Nothing
    , _vpcviOwnerId = Nothing
    , _vpcviPeeringOptions = Nothing
    , _vpcviCidrBlock = Nothing
    , _vpcviRegion = Nothing
    , _vpcviIPv6CidrBlockSet = Nothing
    }


-- | Information about the IPv4 CIDR blocks for the VPC.
vpcviCidrBlockSet :: Lens' VPCPeeringConnectionVPCInfo [CidrBlock]
vpcviCidrBlockSet = lens _vpcviCidrBlockSet (\ s a -> s{_vpcviCidrBlockSet = a}) . _Default . _Coerce

-- | The ID of the VPC.
vpcviVPCId :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviVPCId = lens _vpcviVPCId (\ s a -> s{_vpcviVPCId = a})

-- | The AWS account ID of the VPC owner.
vpcviOwnerId :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviOwnerId = lens _vpcviOwnerId (\ s a -> s{_vpcviOwnerId = a})

-- | Information about the VPC peering connection options for the accepter or requester VPC.
vpcviPeeringOptions :: Lens' VPCPeeringConnectionVPCInfo (Maybe VPCPeeringConnectionOptionsDescription)
vpcviPeeringOptions = lens _vpcviPeeringOptions (\ s a -> s{_vpcviPeeringOptions = a})

-- | The IPv4 CIDR block for the VPC.
vpcviCidrBlock :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviCidrBlock = lens _vpcviCidrBlock (\ s a -> s{_vpcviCidrBlock = a})

-- | The region in which the VPC is located.
vpcviRegion :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviRegion = lens _vpcviRegion (\ s a -> s{_vpcviRegion = a})

-- | The IPv6 CIDR block for the VPC.
vpcviIPv6CidrBlockSet :: Lens' VPCPeeringConnectionVPCInfo [IPv6CidrBlock]
vpcviIPv6CidrBlockSet = lens _vpcviIPv6CidrBlockSet (\ s a -> s{_vpcviIPv6CidrBlockSet = a}) . _Default . _Coerce

instance FromXML VPCPeeringConnectionVPCInfo where
        parseXML x
          = VPCPeeringConnectionVPCInfo' <$>
              (x .@? "cidrBlockSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "vpcId")
                <*> (x .@? "ownerId")
                <*> (x .@? "peeringOptions")
                <*> (x .@? "cidrBlock")
                <*> (x .@? "region")
                <*>
                (x .@? "ipv6CidrBlockSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData VPCPeeringConnectionVPCInfo where

-- | Describes a VPN connection.
--
--
--
-- /See:/ 'vpnConnection' smart constructor.
data VPNConnection = VPNConnection'
  { _vcCustomerGatewayConfiguration :: !(Maybe Text)
  , _vcRoutes                       :: !(Maybe [VPNStaticRoute])
  , _vcVPNGatewayId                 :: !(Maybe Text)
  , _vcCategory                     :: !(Maybe Text)
  , _vcOptions                      :: !(Maybe VPNConnectionOptions)
  , _vcTags                         :: !(Maybe [Tag])
  , _vcVGWTelemetry                 :: !(Maybe [VGWTelemetry])
  , _vcVPNConnectionId              :: !Text
  , _vcCustomerGatewayId            :: !Text
  , _vcState                        :: !VPNState
  , _vcType                         :: !GatewayType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPNConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcCustomerGatewayConfiguration' - The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
--
-- * 'vcRoutes' - The static routes associated with the VPN connection.
--
-- * 'vcVPNGatewayId' - The ID of the virtual private gateway at the AWS side of the VPN connection.
--
-- * 'vcCategory' - The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html#vpn-categories AWS Managed VPN Categories> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- * 'vcOptions' - The VPN connection options.
--
-- * 'vcTags' - Any tags assigned to the VPN connection.
--
-- * 'vcVGWTelemetry' - Information about the VPN tunnel.
--
-- * 'vcVPNConnectionId' - The ID of the VPN connection.
--
-- * 'vcCustomerGatewayId' - The ID of the customer gateway at your end of the VPN connection.
--
-- * 'vcState' - The current state of the VPN connection.
--
-- * 'vcType' - The type of VPN connection.
vpnConnection
    :: Text -- ^ 'vcVPNConnectionId'
    -> Text -- ^ 'vcCustomerGatewayId'
    -> VPNState -- ^ 'vcState'
    -> GatewayType -- ^ 'vcType'
    -> VPNConnection
vpnConnection pVPNConnectionId_ pCustomerGatewayId_ pState_ pType_ =
  VPNConnection'
    { _vcCustomerGatewayConfiguration = Nothing
    , _vcRoutes = Nothing
    , _vcVPNGatewayId = Nothing
    , _vcCategory = Nothing
    , _vcOptions = Nothing
    , _vcTags = Nothing
    , _vcVGWTelemetry = Nothing
    , _vcVPNConnectionId = pVPNConnectionId_
    , _vcCustomerGatewayId = pCustomerGatewayId_
    , _vcState = pState_
    , _vcType = pType_
    }


-- | The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
vcCustomerGatewayConfiguration :: Lens' VPNConnection (Maybe Text)
vcCustomerGatewayConfiguration = lens _vcCustomerGatewayConfiguration (\ s a -> s{_vcCustomerGatewayConfiguration = a})

-- | The static routes associated with the VPN connection.
vcRoutes :: Lens' VPNConnection [VPNStaticRoute]
vcRoutes = lens _vcRoutes (\ s a -> s{_vcRoutes = a}) . _Default . _Coerce

-- | The ID of the virtual private gateway at the AWS side of the VPN connection.
vcVPNGatewayId :: Lens' VPNConnection (Maybe Text)
vcVPNGatewayId = lens _vcVPNGatewayId (\ s a -> s{_vcVPNGatewayId = a})

-- | The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html#vpn-categories AWS Managed VPN Categories> in the /Amazon Virtual Private Cloud User Guide/ .
vcCategory :: Lens' VPNConnection (Maybe Text)
vcCategory = lens _vcCategory (\ s a -> s{_vcCategory = a})

-- | The VPN connection options.
vcOptions :: Lens' VPNConnection (Maybe VPNConnectionOptions)
vcOptions = lens _vcOptions (\ s a -> s{_vcOptions = a})

-- | Any tags assigned to the VPN connection.
vcTags :: Lens' VPNConnection [Tag]
vcTags = lens _vcTags (\ s a -> s{_vcTags = a}) . _Default . _Coerce

-- | Information about the VPN tunnel.
vcVGWTelemetry :: Lens' VPNConnection [VGWTelemetry]
vcVGWTelemetry = lens _vcVGWTelemetry (\ s a -> s{_vcVGWTelemetry = a}) . _Default . _Coerce

-- | The ID of the VPN connection.
vcVPNConnectionId :: Lens' VPNConnection Text
vcVPNConnectionId = lens _vcVPNConnectionId (\ s a -> s{_vcVPNConnectionId = a})

-- | The ID of the customer gateway at your end of the VPN connection.
vcCustomerGatewayId :: Lens' VPNConnection Text
vcCustomerGatewayId = lens _vcCustomerGatewayId (\ s a -> s{_vcCustomerGatewayId = a})

-- | The current state of the VPN connection.
vcState :: Lens' VPNConnection VPNState
vcState = lens _vcState (\ s a -> s{_vcState = a})

-- | The type of VPN connection.
vcType :: Lens' VPNConnection GatewayType
vcType = lens _vcType (\ s a -> s{_vcType = a})

instance FromXML VPNConnection where
        parseXML x
          = VPNConnection' <$>
              (x .@? "customerGatewayConfiguration") <*>
                (x .@? "routes" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "vpnGatewayId")
                <*> (x .@? "category")
                <*> (x .@? "options")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "vgwTelemetry" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "vpnConnectionId")
                <*> (x .@ "customerGatewayId")
                <*> (x .@ "state")
                <*> (x .@ "type")

instance NFData VPNConnection where

-- | Describes VPN connection options.
--
--
--
-- /See:/ 'vpnConnectionOptions' smart constructor.
newtype VPNConnectionOptions = VPNConnectionOptions'
  { _vcoStaticRoutesOnly :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPNConnectionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcoStaticRoutesOnly' - Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
vpnConnectionOptions
    :: VPNConnectionOptions
vpnConnectionOptions = VPNConnectionOptions' {_vcoStaticRoutesOnly = Nothing}


-- | Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
vcoStaticRoutesOnly :: Lens' VPNConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly = lens _vcoStaticRoutesOnly (\ s a -> s{_vcoStaticRoutesOnly = a})

instance FromXML VPNConnectionOptions where
        parseXML x
          = VPNConnectionOptions' <$>
              (x .@? "staticRoutesOnly")

instance NFData VPNConnectionOptions where

-- | Describes VPN connection options.
--
--
--
-- /See:/ 'vpnConnectionOptionsSpecification' smart constructor.
data VPNConnectionOptionsSpecification = VPNConnectionOptionsSpecification'
  { _vcosTunnelOptions    :: !(Maybe [VPNTunnelOptionsSpecification])
  , _vcosStaticRoutesOnly :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPNConnectionOptionsSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcosTunnelOptions' - The tunnel options for the VPN connection.
--
-- * 'vcosStaticRoutesOnly' - Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route. Default: @false@
vpnConnectionOptionsSpecification
    :: VPNConnectionOptionsSpecification
vpnConnectionOptionsSpecification =
  VPNConnectionOptionsSpecification'
    {_vcosTunnelOptions = Nothing, _vcosStaticRoutesOnly = Nothing}


-- | The tunnel options for the VPN connection.
vcosTunnelOptions :: Lens' VPNConnectionOptionsSpecification [VPNTunnelOptionsSpecification]
vcosTunnelOptions = lens _vcosTunnelOptions (\ s a -> s{_vcosTunnelOptions = a}) . _Default . _Coerce

-- | Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route. Default: @false@
vcosStaticRoutesOnly :: Lens' VPNConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly = lens _vcosStaticRoutesOnly (\ s a -> s{_vcosStaticRoutesOnly = a})

instance NFData VPNConnectionOptionsSpecification
         where

instance ToQuery VPNConnectionOptionsSpecification
         where
        toQuery VPNConnectionOptionsSpecification'{..}
          = mconcat
              [toQuery
                 (toQueryList "TunnelOptions" <$> _vcosTunnelOptions),
               "StaticRoutesOnly" =: _vcosStaticRoutesOnly]

-- | Describes a virtual private gateway.
--
--
--
-- /See:/ 'vpnGateway' smart constructor.
data VPNGateway = VPNGateway'
  { _vgState            :: !(Maybe VPNState)
  , _vgVPCAttachments   :: !(Maybe [VPCAttachment])
  , _vgVPNGatewayId     :: !(Maybe Text)
  , _vgAmazonSideASN    :: !(Maybe Integer)
  , _vgAvailabilityZone :: !(Maybe Text)
  , _vgType             :: !(Maybe GatewayType)
  , _vgTags             :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPNGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vgState' - The current state of the virtual private gateway.
--
-- * 'vgVPCAttachments' - Any VPCs attached to the virtual private gateway.
--
-- * 'vgVPNGatewayId' - The ID of the virtual private gateway.
--
-- * 'vgAmazonSideASN' - The private Autonomous System Number (ASN) for the Amazon side of a BGP session.
--
-- * 'vgAvailabilityZone' - The Availability Zone where the virtual private gateway was created, if applicable. This field may be empty or not returned.
--
-- * 'vgType' - The type of VPN connection the virtual private gateway supports.
--
-- * 'vgTags' - Any tags assigned to the virtual private gateway.
vpnGateway
    :: VPNGateway
vpnGateway =
  VPNGateway'
    { _vgState = Nothing
    , _vgVPCAttachments = Nothing
    , _vgVPNGatewayId = Nothing
    , _vgAmazonSideASN = Nothing
    , _vgAvailabilityZone = Nothing
    , _vgType = Nothing
    , _vgTags = Nothing
    }


-- | The current state of the virtual private gateway.
vgState :: Lens' VPNGateway (Maybe VPNState)
vgState = lens _vgState (\ s a -> s{_vgState = a})

-- | Any VPCs attached to the virtual private gateway.
vgVPCAttachments :: Lens' VPNGateway [VPCAttachment]
vgVPCAttachments = lens _vgVPCAttachments (\ s a -> s{_vgVPCAttachments = a}) . _Default . _Coerce

-- | The ID of the virtual private gateway.
vgVPNGatewayId :: Lens' VPNGateway (Maybe Text)
vgVPNGatewayId = lens _vgVPNGatewayId (\ s a -> s{_vgVPNGatewayId = a})

-- | The private Autonomous System Number (ASN) for the Amazon side of a BGP session.
vgAmazonSideASN :: Lens' VPNGateway (Maybe Integer)
vgAmazonSideASN = lens _vgAmazonSideASN (\ s a -> s{_vgAmazonSideASN = a})

-- | The Availability Zone where the virtual private gateway was created, if applicable. This field may be empty or not returned.
vgAvailabilityZone :: Lens' VPNGateway (Maybe Text)
vgAvailabilityZone = lens _vgAvailabilityZone (\ s a -> s{_vgAvailabilityZone = a})

-- | The type of VPN connection the virtual private gateway supports.
vgType :: Lens' VPNGateway (Maybe GatewayType)
vgType = lens _vgType (\ s a -> s{_vgType = a})

-- | Any tags assigned to the virtual private gateway.
vgTags :: Lens' VPNGateway [Tag]
vgTags = lens _vgTags (\ s a -> s{_vgTags = a}) . _Default . _Coerce

instance FromXML VPNGateway where
        parseXML x
          = VPNGateway' <$>
              (x .@? "state") <*>
                (x .@? "attachments" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "vpnGatewayId")
                <*> (x .@? "amazonSideAsn")
                <*> (x .@? "availabilityZone")
                <*> (x .@? "type")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData VPNGateway where

-- | Describes a static route for a VPN connection.
--
--
--
-- /See:/ 'vpnStaticRoute' smart constructor.
data VPNStaticRoute = VPNStaticRoute'
  { _vsrState                :: !(Maybe VPNState)
  , _vsrSource               :: !(Maybe VPNStaticRouteSource)
  , _vsrDestinationCidrBlock :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPNStaticRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsrState' - The current state of the static route.
--
-- * 'vsrSource' - Indicates how the routes were provided.
--
-- * 'vsrDestinationCidrBlock' - The CIDR block associated with the local subnet of the customer data center.
vpnStaticRoute
    :: VPNStaticRoute
vpnStaticRoute =
  VPNStaticRoute'
    { _vsrState = Nothing
    , _vsrSource = Nothing
    , _vsrDestinationCidrBlock = Nothing
    }


-- | The current state of the static route.
vsrState :: Lens' VPNStaticRoute (Maybe VPNState)
vsrState = lens _vsrState (\ s a -> s{_vsrState = a})

-- | Indicates how the routes were provided.
vsrSource :: Lens' VPNStaticRoute (Maybe VPNStaticRouteSource)
vsrSource = lens _vsrSource (\ s a -> s{_vsrSource = a})

-- | The CIDR block associated with the local subnet of the customer data center.
vsrDestinationCidrBlock :: Lens' VPNStaticRoute (Maybe Text)
vsrDestinationCidrBlock = lens _vsrDestinationCidrBlock (\ s a -> s{_vsrDestinationCidrBlock = a})

instance FromXML VPNStaticRoute where
        parseXML x
          = VPNStaticRoute' <$>
              (x .@? "state") <*> (x .@? "source") <*>
                (x .@? "destinationCidrBlock")

instance NFData VPNStaticRoute where

-- | The tunnel options for a VPN connection.
--
--
--
-- /See:/ 'vpnTunnelOptionsSpecification' smart constructor.
data VPNTunnelOptionsSpecification = VPNTunnelOptionsSpecification'
  { _vtosTunnelInsideCidr :: !(Maybe Text)
  , _vtosPreSharedKey     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPNTunnelOptionsSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtosTunnelInsideCidr' - The range of inside IP addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway.  Constraints: A size /30 CIDR block from the @169.254.0.0/16@ range. The following CIDR blocks are reserved and cannot be used:     * @169.254.0.0/30@      * @169.254.1.0/30@      * @169.254.2.0/30@      * @169.254.3.0/30@      * @169.254.4.0/30@      * @169.254.5.0/30@      * @169.254.169.252/30@
--
-- * 'vtosPreSharedKey' - The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and customer gateway. Constraints: Allowed characters are alphanumeric characters and ._. Must be between 8 and 64 characters in length and cannot start with zero (0).
vpnTunnelOptionsSpecification
    :: VPNTunnelOptionsSpecification
vpnTunnelOptionsSpecification =
  VPNTunnelOptionsSpecification'
    {_vtosTunnelInsideCidr = Nothing, _vtosPreSharedKey = Nothing}


-- | The range of inside IP addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway.  Constraints: A size /30 CIDR block from the @169.254.0.0/16@ range. The following CIDR blocks are reserved and cannot be used:     * @169.254.0.0/30@      * @169.254.1.0/30@      * @169.254.2.0/30@      * @169.254.3.0/30@      * @169.254.4.0/30@      * @169.254.5.0/30@      * @169.254.169.252/30@
vtosTunnelInsideCidr :: Lens' VPNTunnelOptionsSpecification (Maybe Text)
vtosTunnelInsideCidr = lens _vtosTunnelInsideCidr (\ s a -> s{_vtosTunnelInsideCidr = a})

-- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and customer gateway. Constraints: Allowed characters are alphanumeric characters and ._. Must be between 8 and 64 characters in length and cannot start with zero (0).
vtosPreSharedKey :: Lens' VPNTunnelOptionsSpecification (Maybe Text)
vtosPreSharedKey = lens _vtosPreSharedKey (\ s a -> s{_vtosPreSharedKey = a})

instance NFData VPNTunnelOptionsSpecification where

instance ToQuery VPNTunnelOptionsSpecification where
        toQuery VPNTunnelOptionsSpecification'{..}
          = mconcat
              ["TunnelInsideCidr" =: _vtosTunnelInsideCidr,
               "PreSharedKey" =: _vtosPreSharedKey]

-- | Describes a volume.
--
--
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
  { _vAttachments      :: !(Maybe [VolumeAttachment])
  , _vIOPS             :: !(Maybe Int)
  , _vKMSKeyId         :: !(Maybe Text)
  , _vTags             :: !(Maybe [Tag])
  , _vAvailabilityZone :: !Text
  , _vCreateTime       :: !ISO8601
  , _vEncrypted        :: !Bool
  , _vSize             :: !Int
  , _vSnapshotId       :: !Text
  , _vState            :: !VolumeState
  , _vVolumeId         :: !Text
  , _vVolumeType       :: !VolumeType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vAttachments' - Information about the volume attachments.
--
-- * 'vIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For Provisioned IOPS SSD volumes, this represents the number of IOPS that are provisioned for the volume. For General Purpose SSD volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information on General Purpose SSD baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraint: Range is 100-32000 IOPS for io1 volumes and 100-10000 IOPS for @gp2@ volumes. Condition: This parameter is required for requests to create @io1@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- * 'vKMSKeyId' - The full ARN of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the volume.
--
-- * 'vTags' - Any tags assigned to the volume.
--
-- * 'vAvailabilityZone' - The Availability Zone for the volume.
--
-- * 'vCreateTime' - The time stamp when volume creation was initiated.
--
-- * 'vEncrypted' - Indicates whether the volume will be encrypted.
--
-- * 'vSize' - The size of the volume, in GiBs.
--
-- * 'vSnapshotId' - The snapshot from which the volume was created, if applicable.
--
-- * 'vState' - The volume state.
--
-- * 'vVolumeId' - The ID of the volume.
--
-- * 'vVolumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
volume
    :: Text -- ^ 'vAvailabilityZone'
    -> UTCTime -- ^ 'vCreateTime'
    -> Bool -- ^ 'vEncrypted'
    -> Int -- ^ 'vSize'
    -> Text -- ^ 'vSnapshotId'
    -> VolumeState -- ^ 'vState'
    -> Text -- ^ 'vVolumeId'
    -> VolumeType -- ^ 'vVolumeType'
    -> Volume
volume pAvailabilityZone_ pCreateTime_ pEncrypted_ pSize_ pSnapshotId_ pState_ pVolumeId_ pVolumeType_ =
  Volume'
    { _vAttachments = Nothing
    , _vIOPS = Nothing
    , _vKMSKeyId = Nothing
    , _vTags = Nothing
    , _vAvailabilityZone = pAvailabilityZone_
    , _vCreateTime = _Time # pCreateTime_
    , _vEncrypted = pEncrypted_
    , _vSize = pSize_
    , _vSnapshotId = pSnapshotId_
    , _vState = pState_
    , _vVolumeId = pVolumeId_
    , _vVolumeType = pVolumeType_
    }


-- | Information about the volume attachments.
vAttachments :: Lens' Volume [VolumeAttachment]
vAttachments = lens _vAttachments (\ s a -> s{_vAttachments = a}) . _Default . _Coerce

-- | The number of I/O operations per second (IOPS) that the volume supports. For Provisioned IOPS SSD volumes, this represents the number of IOPS that are provisioned for the volume. For General Purpose SSD volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information on General Purpose SSD baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraint: Range is 100-32000 IOPS for io1 volumes and 100-10000 IOPS for @gp2@ volumes. Condition: This parameter is required for requests to create @io1@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
vIOPS :: Lens' Volume (Maybe Int)
vIOPS = lens _vIOPS (\ s a -> s{_vIOPS = a})

-- | The full ARN of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the volume.
vKMSKeyId :: Lens' Volume (Maybe Text)
vKMSKeyId = lens _vKMSKeyId (\ s a -> s{_vKMSKeyId = a})

-- | Any tags assigned to the volume.
vTags :: Lens' Volume [Tag]
vTags = lens _vTags (\ s a -> s{_vTags = a}) . _Default . _Coerce

-- | The Availability Zone for the volume.
vAvailabilityZone :: Lens' Volume Text
vAvailabilityZone = lens _vAvailabilityZone (\ s a -> s{_vAvailabilityZone = a})

-- | The time stamp when volume creation was initiated.
vCreateTime :: Lens' Volume UTCTime
vCreateTime = lens _vCreateTime (\ s a -> s{_vCreateTime = a}) . _Time

-- | Indicates whether the volume will be encrypted.
vEncrypted :: Lens' Volume Bool
vEncrypted = lens _vEncrypted (\ s a -> s{_vEncrypted = a})

-- | The size of the volume, in GiBs.
vSize :: Lens' Volume Int
vSize = lens _vSize (\ s a -> s{_vSize = a})

-- | The snapshot from which the volume was created, if applicable.
vSnapshotId :: Lens' Volume Text
vSnapshotId = lens _vSnapshotId (\ s a -> s{_vSnapshotId = a})

-- | The volume state.
vState :: Lens' Volume VolumeState
vState = lens _vState (\ s a -> s{_vState = a})

-- | The ID of the volume.
vVolumeId :: Lens' Volume Text
vVolumeId = lens _vVolumeId (\ s a -> s{_vVolumeId = a})

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
vVolumeType :: Lens' Volume VolumeType
vVolumeType = lens _vVolumeType (\ s a -> s{_vVolumeType = a})

instance FromXML Volume where
        parseXML x
          = Volume' <$>
              (x .@? "attachmentSet" .!@ mempty >>=
                 may (parseXMLList "item"))
                <*> (x .@? "iops")
                <*> (x .@? "kmsKeyId")
                <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "availabilityZone")
                <*> (x .@ "createTime")
                <*> (x .@ "encrypted")
                <*> (x .@ "size")
                <*> (x .@ "snapshotId")
                <*> (x .@ "status")
                <*> (x .@ "volumeId")
                <*> (x .@ "volumeType")

instance NFData Volume where

-- | Describes volume attachment details.
--
--
--
-- /See:/ 'volumeAttachment' smart constructor.
data VolumeAttachment = VolumeAttachment'
  { _volInstanceId          :: !(Maybe Text)
  , _volDeleteOnTermination :: !(Maybe Bool)
  , _volState               :: !(Maybe VolumeAttachmentState)
  , _volDevice              :: !(Maybe Text)
  , _volVolumeId            :: !(Maybe Text)
  , _volAttachTime          :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'volInstanceId' - The ID of the instance.
--
-- * 'volDeleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- * 'volState' - The attachment state of the volume.
--
-- * 'volDevice' - The device name.
--
-- * 'volVolumeId' - The ID of the volume.
--
-- * 'volAttachTime' - The time stamp when the attachment initiated.
volumeAttachment
    :: VolumeAttachment
volumeAttachment =
  VolumeAttachment'
    { _volInstanceId = Nothing
    , _volDeleteOnTermination = Nothing
    , _volState = Nothing
    , _volDevice = Nothing
    , _volVolumeId = Nothing
    , _volAttachTime = Nothing
    }


-- | The ID of the instance.
volInstanceId :: Lens' VolumeAttachment (Maybe Text)
volInstanceId = lens _volInstanceId (\ s a -> s{_volInstanceId = a})

-- | Indicates whether the EBS volume is deleted on instance termination.
volDeleteOnTermination :: Lens' VolumeAttachment (Maybe Bool)
volDeleteOnTermination = lens _volDeleteOnTermination (\ s a -> s{_volDeleteOnTermination = a})

-- | The attachment state of the volume.
volState :: Lens' VolumeAttachment (Maybe VolumeAttachmentState)
volState = lens _volState (\ s a -> s{_volState = a})

-- | The device name.
volDevice :: Lens' VolumeAttachment (Maybe Text)
volDevice = lens _volDevice (\ s a -> s{_volDevice = a})

-- | The ID of the volume.
volVolumeId :: Lens' VolumeAttachment (Maybe Text)
volVolumeId = lens _volVolumeId (\ s a -> s{_volVolumeId = a})

-- | The time stamp when the attachment initiated.
volAttachTime :: Lens' VolumeAttachment (Maybe UTCTime)
volAttachTime = lens _volAttachTime (\ s a -> s{_volAttachTime = a}) . mapping _Time

instance FromXML VolumeAttachment where
        parseXML x
          = VolumeAttachment' <$>
              (x .@? "instanceId") <*>
                (x .@? "deleteOnTermination")
                <*> (x .@? "status")
                <*> (x .@? "device")
                <*> (x .@? "volumeId")
                <*> (x .@? "attachTime")

instance NFData VolumeAttachment where

-- | Describes an EBS volume.
--
--
--
-- /See:/ 'volumeDetail' smart constructor.
newtype VolumeDetail = VolumeDetail'
  { _vdSize :: Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdSize' - The size of the volume, in GiB.
volumeDetail
    :: Integer -- ^ 'vdSize'
    -> VolumeDetail
volumeDetail pSize_ = VolumeDetail' {_vdSize = pSize_}


-- | The size of the volume, in GiB.
vdSize :: Lens' VolumeDetail Integer
vdSize = lens _vdSize (\ s a -> s{_vdSize = a})

instance NFData VolumeDetail where

instance ToQuery VolumeDetail where
        toQuery VolumeDetail'{..}
          = mconcat ["Size" =: _vdSize]

-- | Describes the modification status of an EBS volume.
--
--
-- If the volume has never been modified, some element values will be null.
--
--
-- /See:/ 'volumeModification' smart constructor.
data VolumeModification = VolumeModification'
  { _vmProgress           :: !(Maybe Integer)
  , _vmStartTime          :: !(Maybe ISO8601)
  , _vmModificationState  :: !(Maybe VolumeModificationState)
  , _vmTargetVolumeType   :: !(Maybe VolumeType)
  , _vmOriginalVolumeType :: !(Maybe VolumeType)
  , _vmTargetSize         :: !(Maybe Int)
  , _vmTargetIOPS         :: !(Maybe Int)
  , _vmOriginalSize       :: !(Maybe Int)
  , _vmOriginalIOPS       :: !(Maybe Int)
  , _vmStatusMessage      :: !(Maybe Text)
  , _vmEndTime            :: !(Maybe ISO8601)
  , _vmVolumeId           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeModification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmProgress' - Modification progress from 0 to 100%.
--
-- * 'vmStartTime' - Modification start time
--
-- * 'vmModificationState' - Current state of modification. Modification state is null for unmodified volumes.
--
-- * 'vmTargetVolumeType' - Target EBS volume type of the volume being modified.
--
-- * 'vmOriginalVolumeType' - Original EBS volume type of the volume being modified.
--
-- * 'vmTargetSize' - Target size of the volume being modified.
--
-- * 'vmTargetIOPS' - Target IOPS rate of the volume being modified.
--
-- * 'vmOriginalSize' - Original size of the volume being modified.
--
-- * 'vmOriginalIOPS' - Original IOPS rate of the volume being modified.
--
-- * 'vmStatusMessage' - Generic status message on modification progress or failure.
--
-- * 'vmEndTime' - Modification completion or failure time.
--
-- * 'vmVolumeId' - ID of the volume being modified.
volumeModification
    :: VolumeModification
volumeModification =
  VolumeModification'
    { _vmProgress = Nothing
    , _vmStartTime = Nothing
    , _vmModificationState = Nothing
    , _vmTargetVolumeType = Nothing
    , _vmOriginalVolumeType = Nothing
    , _vmTargetSize = Nothing
    , _vmTargetIOPS = Nothing
    , _vmOriginalSize = Nothing
    , _vmOriginalIOPS = Nothing
    , _vmStatusMessage = Nothing
    , _vmEndTime = Nothing
    , _vmVolumeId = Nothing
    }


-- | Modification progress from 0 to 100%.
vmProgress :: Lens' VolumeModification (Maybe Integer)
vmProgress = lens _vmProgress (\ s a -> s{_vmProgress = a})

-- | Modification start time
vmStartTime :: Lens' VolumeModification (Maybe UTCTime)
vmStartTime = lens _vmStartTime (\ s a -> s{_vmStartTime = a}) . mapping _Time

-- | Current state of modification. Modification state is null for unmodified volumes.
vmModificationState :: Lens' VolumeModification (Maybe VolumeModificationState)
vmModificationState = lens _vmModificationState (\ s a -> s{_vmModificationState = a})

-- | Target EBS volume type of the volume being modified.
vmTargetVolumeType :: Lens' VolumeModification (Maybe VolumeType)
vmTargetVolumeType = lens _vmTargetVolumeType (\ s a -> s{_vmTargetVolumeType = a})

-- | Original EBS volume type of the volume being modified.
vmOriginalVolumeType :: Lens' VolumeModification (Maybe VolumeType)
vmOriginalVolumeType = lens _vmOriginalVolumeType (\ s a -> s{_vmOriginalVolumeType = a})

-- | Target size of the volume being modified.
vmTargetSize :: Lens' VolumeModification (Maybe Int)
vmTargetSize = lens _vmTargetSize (\ s a -> s{_vmTargetSize = a})

-- | Target IOPS rate of the volume being modified.
vmTargetIOPS :: Lens' VolumeModification (Maybe Int)
vmTargetIOPS = lens _vmTargetIOPS (\ s a -> s{_vmTargetIOPS = a})

-- | Original size of the volume being modified.
vmOriginalSize :: Lens' VolumeModification (Maybe Int)
vmOriginalSize = lens _vmOriginalSize (\ s a -> s{_vmOriginalSize = a})

-- | Original IOPS rate of the volume being modified.
vmOriginalIOPS :: Lens' VolumeModification (Maybe Int)
vmOriginalIOPS = lens _vmOriginalIOPS (\ s a -> s{_vmOriginalIOPS = a})

-- | Generic status message on modification progress or failure.
vmStatusMessage :: Lens' VolumeModification (Maybe Text)
vmStatusMessage = lens _vmStatusMessage (\ s a -> s{_vmStatusMessage = a})

-- | Modification completion or failure time.
vmEndTime :: Lens' VolumeModification (Maybe UTCTime)
vmEndTime = lens _vmEndTime (\ s a -> s{_vmEndTime = a}) . mapping _Time

-- | ID of the volume being modified.
vmVolumeId :: Lens' VolumeModification (Maybe Text)
vmVolumeId = lens _vmVolumeId (\ s a -> s{_vmVolumeId = a})

instance FromXML VolumeModification where
        parseXML x
          = VolumeModification' <$>
              (x .@? "progress") <*> (x .@? "startTime") <*>
                (x .@? "modificationState")
                <*> (x .@? "targetVolumeType")
                <*> (x .@? "originalVolumeType")
                <*> (x .@? "targetSize")
                <*> (x .@? "targetIops")
                <*> (x .@? "originalSize")
                <*> (x .@? "originalIops")
                <*> (x .@? "statusMessage")
                <*> (x .@? "endTime")
                <*> (x .@? "volumeId")

instance NFData VolumeModification where

-- | Describes a volume status operation code.
--
--
--
-- /See:/ 'volumeStatusAction' smart constructor.
data VolumeStatusAction = VolumeStatusAction'
  { _vsaEventType   :: !(Maybe Text)
  , _vsaCode        :: !(Maybe Text)
  , _vsaDescription :: !(Maybe Text)
  , _vsaEventId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeStatusAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsaEventType' - The event type associated with this operation.
--
-- * 'vsaCode' - The code identifying the operation, for example, @enable-volume-io@ .
--
-- * 'vsaDescription' - A description of the operation.
--
-- * 'vsaEventId' - The ID of the event associated with this operation.
volumeStatusAction
    :: VolumeStatusAction
volumeStatusAction =
  VolumeStatusAction'
    { _vsaEventType = Nothing
    , _vsaCode = Nothing
    , _vsaDescription = Nothing
    , _vsaEventId = Nothing
    }


-- | The event type associated with this operation.
vsaEventType :: Lens' VolumeStatusAction (Maybe Text)
vsaEventType = lens _vsaEventType (\ s a -> s{_vsaEventType = a})

-- | The code identifying the operation, for example, @enable-volume-io@ .
vsaCode :: Lens' VolumeStatusAction (Maybe Text)
vsaCode = lens _vsaCode (\ s a -> s{_vsaCode = a})

-- | A description of the operation.
vsaDescription :: Lens' VolumeStatusAction (Maybe Text)
vsaDescription = lens _vsaDescription (\ s a -> s{_vsaDescription = a})

-- | The ID of the event associated with this operation.
vsaEventId :: Lens' VolumeStatusAction (Maybe Text)
vsaEventId = lens _vsaEventId (\ s a -> s{_vsaEventId = a})

instance FromXML VolumeStatusAction where
        parseXML x
          = VolumeStatusAction' <$>
              (x .@? "eventType") <*> (x .@? "code") <*>
                (x .@? "description")
                <*> (x .@? "eventId")

instance NFData VolumeStatusAction where

-- | Describes a volume status.
--
--
--
-- /See:/ 'volumeStatusDetails' smart constructor.
data VolumeStatusDetails = VolumeStatusDetails'
  { _vsdStatus :: !(Maybe Text)
  , _vsdName   :: !(Maybe VolumeStatusName)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsdStatus' - The intended status of the volume status.
--
-- * 'vsdName' - The name of the volume status.
volumeStatusDetails
    :: VolumeStatusDetails
volumeStatusDetails =
  VolumeStatusDetails' {_vsdStatus = Nothing, _vsdName = Nothing}


-- | The intended status of the volume status.
vsdStatus :: Lens' VolumeStatusDetails (Maybe Text)
vsdStatus = lens _vsdStatus (\ s a -> s{_vsdStatus = a})

-- | The name of the volume status.
vsdName :: Lens' VolumeStatusDetails (Maybe VolumeStatusName)
vsdName = lens _vsdName (\ s a -> s{_vsdName = a})

instance FromXML VolumeStatusDetails where
        parseXML x
          = VolumeStatusDetails' <$>
              (x .@? "status") <*> (x .@? "name")

instance NFData VolumeStatusDetails where

-- | Describes a volume status event.
--
--
--
-- /See:/ 'volumeStatusEvent' smart constructor.
data VolumeStatusEvent = VolumeStatusEvent'
  { _vseNotBefore   :: !(Maybe ISO8601)
  , _vseEventType   :: !(Maybe Text)
  , _vseDescription :: !(Maybe Text)
  , _vseNotAfter    :: !(Maybe ISO8601)
  , _vseEventId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeStatusEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vseNotBefore' - The earliest start time of the event.
--
-- * 'vseEventType' - The type of this event.
--
-- * 'vseDescription' - A description of the event.
--
-- * 'vseNotAfter' - The latest end time of the event.
--
-- * 'vseEventId' - The ID of this event.
volumeStatusEvent
    :: VolumeStatusEvent
volumeStatusEvent =
  VolumeStatusEvent'
    { _vseNotBefore = Nothing
    , _vseEventType = Nothing
    , _vseDescription = Nothing
    , _vseNotAfter = Nothing
    , _vseEventId = Nothing
    }


-- | The earliest start time of the event.
vseNotBefore :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotBefore = lens _vseNotBefore (\ s a -> s{_vseNotBefore = a}) . mapping _Time

-- | The type of this event.
vseEventType :: Lens' VolumeStatusEvent (Maybe Text)
vseEventType = lens _vseEventType (\ s a -> s{_vseEventType = a})

-- | A description of the event.
vseDescription :: Lens' VolumeStatusEvent (Maybe Text)
vseDescription = lens _vseDescription (\ s a -> s{_vseDescription = a})

-- | The latest end time of the event.
vseNotAfter :: Lens' VolumeStatusEvent (Maybe UTCTime)
vseNotAfter = lens _vseNotAfter (\ s a -> s{_vseNotAfter = a}) . mapping _Time

-- | The ID of this event.
vseEventId :: Lens' VolumeStatusEvent (Maybe Text)
vseEventId = lens _vseEventId (\ s a -> s{_vseEventId = a})

instance FromXML VolumeStatusEvent where
        parseXML x
          = VolumeStatusEvent' <$>
              (x .@? "notBefore") <*> (x .@? "eventType") <*>
                (x .@? "description")
                <*> (x .@? "notAfter")
                <*> (x .@? "eventId")

instance NFData VolumeStatusEvent where

-- | Describes the status of a volume.
--
--
--
-- /See:/ 'volumeStatusInfo' smart constructor.
data VolumeStatusInfo = VolumeStatusInfo'
  { _vsiStatus  :: !(Maybe VolumeStatusInfoStatus)
  , _vsiDetails :: !(Maybe [VolumeStatusDetails])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsiStatus' - The status of the volume.
--
-- * 'vsiDetails' - The details of the volume status.
volumeStatusInfo
    :: VolumeStatusInfo
volumeStatusInfo =
  VolumeStatusInfo' {_vsiStatus = Nothing, _vsiDetails = Nothing}


-- | The status of the volume.
vsiStatus :: Lens' VolumeStatusInfo (Maybe VolumeStatusInfoStatus)
vsiStatus = lens _vsiStatus (\ s a -> s{_vsiStatus = a})

-- | The details of the volume status.
vsiDetails :: Lens' VolumeStatusInfo [VolumeStatusDetails]
vsiDetails = lens _vsiDetails (\ s a -> s{_vsiDetails = a}) . _Default . _Coerce

instance FromXML VolumeStatusInfo where
        parseXML x
          = VolumeStatusInfo' <$>
              (x .@? "status") <*>
                (x .@? "details" .!@ mempty >>=
                   may (parseXMLList "item"))

instance NFData VolumeStatusInfo where

-- | Describes the volume status.
--
--
--
-- /See:/ 'volumeStatusItem' smart constructor.
data VolumeStatusItem = VolumeStatusItem'
  { _vsiVolumeStatus     :: !(Maybe VolumeStatusInfo)
  , _vsiActions          :: !(Maybe [VolumeStatusAction])
  , _vsiEvents           :: !(Maybe [VolumeStatusEvent])
  , _vsiAvailabilityZone :: !(Maybe Text)
  , _vsiVolumeId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeStatusItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsiVolumeStatus' - The volume status.
--
-- * 'vsiActions' - The details of the operation.
--
-- * 'vsiEvents' - A list of events associated with the volume.
--
-- * 'vsiAvailabilityZone' - The Availability Zone of the volume.
--
-- * 'vsiVolumeId' - The volume ID.
volumeStatusItem
    :: VolumeStatusItem
volumeStatusItem =
  VolumeStatusItem'
    { _vsiVolumeStatus = Nothing
    , _vsiActions = Nothing
    , _vsiEvents = Nothing
    , _vsiAvailabilityZone = Nothing
    , _vsiVolumeId = Nothing
    }


-- | The volume status.
vsiVolumeStatus :: Lens' VolumeStatusItem (Maybe VolumeStatusInfo)
vsiVolumeStatus = lens _vsiVolumeStatus (\ s a -> s{_vsiVolumeStatus = a})

-- | The details of the operation.
vsiActions :: Lens' VolumeStatusItem [VolumeStatusAction]
vsiActions = lens _vsiActions (\ s a -> s{_vsiActions = a}) . _Default . _Coerce

-- | A list of events associated with the volume.
vsiEvents :: Lens' VolumeStatusItem [VolumeStatusEvent]
vsiEvents = lens _vsiEvents (\ s a -> s{_vsiEvents = a}) . _Default . _Coerce

-- | The Availability Zone of the volume.
vsiAvailabilityZone :: Lens' VolumeStatusItem (Maybe Text)
vsiAvailabilityZone = lens _vsiAvailabilityZone (\ s a -> s{_vsiAvailabilityZone = a})

-- | The volume ID.
vsiVolumeId :: Lens' VolumeStatusItem (Maybe Text)
vsiVolumeId = lens _vsiVolumeId (\ s a -> s{_vsiVolumeId = a})

instance FromXML VolumeStatusItem where
        parseXML x
          = VolumeStatusItem' <$>
              (x .@? "volumeStatus") <*>
                (x .@? "actionsSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*>
                (x .@? "eventsSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@? "availabilityZone")
                <*> (x .@? "volumeId")

instance NFData VolumeStatusItem where

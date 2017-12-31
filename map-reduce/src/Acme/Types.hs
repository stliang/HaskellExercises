{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Acme.Types where

import Data.Csv
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Describes an Acme Customer with several attributes
data AcmeCustomerAttributes = AcmeCustomerAttributes
  { _acId :: Int
  , _acFirstName :: Text
  , _acLastName :: Text
  , _acAge :: Int
  , _acLTVEst :: Int
  , _acChurnScore :: Double
  , _acSegmentId :: Int
  } deriving (Show, Eq, Generic)
instance FromRecord AcmeCustomerAttributes
instance ToRecord AcmeCustomerAttributes

-- | Describes a single Acme customer segment
data AcmeSegmentInfo = AcmeSegmentInfo
  { _asSegmentId :: Int
  , _asSegmentCount :: Int
  } deriving (Show, Eq, Generic)
instance FromRecord AcmeSegmentInfo
instance ToRecord AcmeSegmentInfo

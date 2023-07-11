{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module DomainModel where

import           Control.Lens.TH (abbreviatedFields, makeLensesWith)
import           Data.Aeson
import           Data.Csv        (ToRecord)
import           Data.Text       (Text, pack, unpack)
import           GHC.Generics    (Generic)

data VoucherList = VoucherList
  { _vlContent    :: [Voucher],
    _vlTotalPages :: Int,
    _vlSize       :: Int
  }
  deriving (Eq, Show)

instance FromJSON VoucherList where
  parseJSON = withObject "VoucherList" $ \l ->
    VoucherList
      <$> l .: "content"
      <*> l .: "totalPages"
      <*> l .: "size"

data Voucher = Voucher
  { _vId            :: Text,
    _vVoucherNumber :: Text,
    _vVoucherDate   :: Text,
    _vContactName   :: Text,
    _vTotalAmount   :: Double
  }
  deriving (Show, Eq)

instance FromJSON Voucher where
  parseJSON = withObject "Voucher" $ \v ->
    Voucher
      <$> v .: "id"
      <*> v .: "voucherNumber"
      <*> v .: "voucherDate"
      <*> v .: "contactName"
      <*> v .: "totalAmount"

data Invoice = Invoice
  { lineItems :: [LineItem]
  }
  deriving (Show, Generic, FromJSON)

data LineItem = LineItem
  { name               :: String,
    quantity           :: Double,
    unitName           :: String,
    unitPrice          :: UnitPrice,
    discountPercentage :: Double,
    lineItemAmount     :: Double
  }
  deriving (Show, Generic, FromJSON)

data FlatLineItem = FlatLineItem
  { flatLineItemName                       :: String,
    flatLineItemQuantity                   :: Double,
    flatLineItemUnitName                   :: String,
    flatLineItemUnitPriceCurrency          :: String,
    flatLineItemUnitPriceNetAmount         :: String,
    flatLineItemUnitPriceGrossAmount       :: String,
    flatLineItemUnitPriceTaxRatePercentage :: Double,
    flatLineItemDiscountPercentage         :: Double,
    flatLineItemAmount                     :: String
  }
  deriving (Show, Generic, FromJSON, ToRecord)

buildFlatItem :: LineItem -> FlatLineItem
buildFlatItem item =
  FlatLineItem
    { flatLineItemName = name item,
      flatLineItemQuantity = quantity item,
      flatLineItemUnitName = unitName item,
      flatLineItemUnitPriceCurrency = currency $ unitPrice item,
      flatLineItemUnitPriceNetAmount = gerEncode $ netAmount $ unitPrice item,
      flatLineItemUnitPriceGrossAmount = gerEncode $ grossAmount $ unitPrice item,
      flatLineItemUnitPriceTaxRatePercentage = taxRatePercentage $ unitPrice item,
      flatLineItemDiscountPercentage = discountPercentage item,
      flatLineItemAmount = gerEncode $ lineItemAmount item
    }

data DenormalizedItem = DenormalizedItem
  { vNumber                    :: String,
    vDate                      :: String,
    customerName               :: String,
    total                      :: String,
    itemName                   :: String,
    itemQuantity               :: String,
    itemUnitName               :: String,
    unitPriceCurrency          :: String,
    unitPriceNetAmount         :: String,
    unitPriceGrossAmount       :: String,
    unitPriceTaxRatePercentage :: String,
    itemDiscountPercentage     :: String,
    itemAmount                 :: String
  }
  deriving (Show, Generic, FromJSON, ToRecord)

buildDenormalizedItem :: (Voucher, LineItem) -> DenormalizedItem
buildDenormalizedItem (voucher, item) =
  DenormalizedItem
    { vNumber = unpack $ _vVoucherNumber voucher,
      vDate = take 10 (unpack $ _vVoucherDate voucher),
      customerName = unpack $ _vContactName voucher,
      total = gerEncode $ _vTotalAmount voucher,
      itemName = name item,
      itemQuantity = gerEncode $ quantity item,
      itemUnitName = unitName item,
      unitPriceCurrency = currency $ unitPrice item,
      unitPriceNetAmount = gerEncode $ netAmount $ unitPrice item,
      unitPriceGrossAmount = gerEncode $ grossAmount $ unitPrice item,
      unitPriceTaxRatePercentage = gerEncode $ taxRatePercentage $ unitPrice item,
      itemDiscountPercentage = gerEncode $ discountPercentage item,
      itemAmount = gerEncode $ lineItemAmount item
    }

-- Function to convert a Double to German floating-point encoding
gerEncode :: Double -> String
gerEncode = replaceDotWithComma . show
  where
    replaceDotWithComma = map replaceDot
    replaceDot '.' = ','
    replaceDot c   = c

data UnitPrice = UnitPrice
  { currency          :: String,
    netAmount         :: Double,
    grossAmount       :: Double,
    taxRatePercentage :: Double
  }
  deriving (Show, Generic, FromJSON)

makeLensesWith abbreviatedFields 'Voucher
makeLensesWith abbreviatedFields 'VoucherList

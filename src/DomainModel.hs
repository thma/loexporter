{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module DomainModel where

import           Control.Lens.TH (abbreviatedFields, makeLensesWith)
import           Data.Aeson
import           Data.Text       (Text, pack, unpack)
import           GHC.Generics    (Generic)
import           Data.Maybe ( fromMaybe )
import           Data.Map (Map)
import qualified Data.Map as Map

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
    flatLineItemPLU                        :: String,
    flatLineItemQuantity                   :: Double,
    flatLineItemUnitName                   :: String,
    flatLineItemUnitPriceCurrency          :: String,
    flatLineItemUnitPriceNetAmount         :: Double,
    flatLineItemUnitPriceGrossAmount       :: Double,
    flatLineItemUnitPriceTaxRatePercentage :: Double,
    flatLineItemDiscountPercentage         :: Double,
    flatLineItemAmount                     :: Double
  }
  deriving (Show)

type PluMap = Map String String

buildFlatItem :: PluMap -> LineItem -> FlatLineItem
buildFlatItem pluMap item =
  FlatLineItem
    { flatLineItemName = name item,
      flatLineItemPLU = lookupPLU pluMap (name item),
      flatLineItemQuantity = quantity item,
      flatLineItemUnitName = unitName item,
      flatLineItemUnitPriceCurrency = currency $ unitPrice item,
      flatLineItemUnitPriceNetAmount = netAmount $ unitPrice item,
      flatLineItemUnitPriceGrossAmount = grossAmount $ unitPrice item,
      flatLineItemUnitPriceTaxRatePercentage = taxRatePercentage $ unitPrice item,
      flatLineItemDiscountPercentage = discountPercentage item,
      flatLineItemAmount = lineItemAmount item
    }

data DenormalizedItem = DenormalizedItem
  { vNumber                    :: String,
    vDate                      :: String,
    customerName               :: String,
    total                      :: Double,
    itemName                   :: String,
    itemPLU                    :: String,
    itemQuantity               :: Double,
    itemUnitName               :: String,
    unitPriceCurrency          :: String,
    unitPriceNetAmount         :: Double,
    unitPriceGrossAmount       :: Double,
    unitPriceTaxRatePercentage :: Double,
    itemDiscountPercentage     :: Double,
    itemAmount                 :: Double
  }
  deriving (Show)

buildDenormalizedItem :: PluMap -> (Voucher, LineItem) -> DenormalizedItem
buildDenormalizedItem pluMap (voucher, item) =
  DenormalizedItem
    { vNumber = unpack $ _vVoucherNumber voucher,
      vDate = take 10 (unpack $ _vVoucherDate voucher),
      customerName = unpack $ _vContactName voucher,
      total = _vTotalAmount voucher,
      itemName = name item,
      itemPLU = lookupPLU pluMap (name item),
      itemQuantity = quantity item,
      itemUnitName = unitName item,
      unitPriceCurrency = currency $ unitPrice item,
      unitPriceNetAmount = netAmount $ unitPrice item,
      unitPriceGrossAmount = grossAmount $ unitPrice item,
      unitPriceTaxRatePercentage = taxRatePercentage $ unitPrice item,
      itemDiscountPercentage = discountPercentage item,
      itemAmount = lineItemAmount item
    }

lookupPLU :: PluMap -> String -> String
lookupPLU pluMap name = fromMaybe name (Map.lookup name pluMap)

data UnitPrice = UnitPrice
  { currency          :: String,
    netAmount         :: Double,
    grossAmount       :: Double,
    taxRatePercentage :: Double
  }
  deriving (Show, Generic, FromJSON)

makeLensesWith abbreviatedFields 'Voucher
makeLensesWith abbreviatedFields 'VoucherList
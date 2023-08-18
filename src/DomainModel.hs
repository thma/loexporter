{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module DomainModel where

import           Control.Lens.TH (abbreviatedFields, makeLensesWith, makeLensesFor)
import           Data.Aeson
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text, pack, unpack, null, splitOn)
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
  { name               :: Text,
    quantity           :: Double,
    unitName           :: Text,
    unitPrice          :: UnitPrice,
    discountPercentage :: Double,
    lineItemAmount     :: Double
  }
  deriving (Show, Generic, FromJSON, Eq)



data FlatLineItem = FlatLineItem
  { flatLineItemName                       :: Text,
    flatLineItemPLU                        :: Text,
    flatLineItemQuantity                   :: Double,
    flatLineItemUnitName                   :: Text,
    flatLineItemUnitPriceCurrency          :: Text,
    flatLineItemUnitPriceNetAmount         :: Double,
    flatLineItemUnitPriceGrossAmount       :: Double,
    flatLineItemUnitPriceTaxRatePercentage :: Double,
    flatLineItemDiscountPercentage         :: Double,
    flatLineItemAmount                     :: Double
  }
  deriving (Show, Eq)

type PluMap = Map Text Text

buildFlatItem :: PluMap -> LineItem -> FlatLineItem
buildFlatItem pluMap item =
  FlatLineItem
    { flatLineItemName = itemName,
      flatLineItemPLU = itemPlu,
      flatLineItemQuantity = quantity item,
      flatLineItemUnitName = unitName item,
      flatLineItemUnitPriceCurrency = currency $ unitPrice item,
      flatLineItemUnitPriceNetAmount = netAmount $ unitPrice item,
      flatLineItemUnitPriceGrossAmount = grossAmount $ unitPrice item,
      flatLineItemUnitPriceTaxRatePercentage = taxRatePercentage $ unitPrice item,
      flatLineItemDiscountPercentage = discountPercentage item,
      flatLineItemAmount = lineItemAmount item
    } where
        (plu, itemName) = splitName (name item)
        itemPlu = if Data.Text.null plu
                      then lookupPLU pluMap (name item)
                      else plu

splitName :: Text -> (Text, Text)
splitName name =
  case splitOn " | " name of
    [p, n] -> (p, n)
    _      -> (mempty, name)


data DenormalizedItem = DenormalizedItem
  { vNumber                    :: String,
    vDate                      :: String,
    customerName               :: String,
    total                      :: Double,
    itemName                   :: Text,
    itemPLU                    :: Text,
    itemQuantity               :: Double,
    itemUnitName               :: Text,
    unitPriceCurrency          :: Text,
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

lookupPLU :: PluMap -> Text -> Text
lookupPLU pluMap name = fromMaybe "" (Map.lookup name pluMap)

data UnitPrice = UnitPrice
  { currency          :: Text,
    netAmount         :: Double,
    grossAmount       :: Double,
    taxRatePercentage :: Double
  }
  deriving (Show, Generic, FromJSON, Eq)

makeLensesWith abbreviatedFields 'Voucher
makeLensesWith abbreviatedFields 'VoucherList
makeLensesFor [("name", "_name"), ("quantity", "_quantity"), ("unitName", "_unitName"), ("lineItemAmount", "_lineItemAmount")] ''LineItem

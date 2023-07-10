{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
module UiModel where
  
import Control.Lens.TH ( abbreviatedFields, makeLensesWith )
import Data.Text ( Text )
import Data.Time.Calendar ( Day )
import Data.ByteString ( ByteString )
import DomainModel ( Voucher, VoucherList )

data InvoiceModel = InvoiceModel {
  _imQueryFrom :: Day,
  _imQueryTo :: Day,
  _imQueryFilter :: Text,
  _imApiToken :: ByteString,
  _imSearching :: Bool,
  _imErrorMsg :: Maybe Text,
  _imVouchers :: [Voucher],
  _imSelected :: Maybe Voucher
} deriving (Eq, Show)

data InvoiceEvt
  = InvoiceInit
  | InvoiceSearch
  | InvoiceSearchResult VoucherList
  | InvoiceSaveAll
  | InvoiceSearchError Text
  | InvoiceShowDetails Voucher
  | InvoiceCloseDetails
  | InvoiceCloseError
  deriving (Eq, Show)

makeLensesWith abbreviatedFields 'InvoiceModel

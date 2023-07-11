{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module UiModel 
where

import           Control.Lens.TH    (abbreviatedFields, makeLensesWith)
import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)
import           DomainModel        (Invoice, Voucher, VoucherList)

data InvoiceModel = InvoiceModel
  { _imQueryFrom   :: Day,
    _imQueryTo     :: Day,
    _imQueryFilter :: Text,
    _imApiAccess   :: ApiAccess,
    _imSearching   :: Bool,
    _imErrorMsg    :: Maybe Text,
    _imVouchers    :: [Voucher],
    _imSelected    :: Maybe Voucher
  }

instance Eq InvoiceModel where
  (==) :: InvoiceModel -> InvoiceModel -> Bool
  (==) a b =
    _imQueryFrom a == _imQueryFrom b
      && _imQueryTo a == _imQueryTo b
      && _imQueryFilter a == _imQueryFilter b
      && _imSearching a == _imSearching b
      && _imErrorMsg a == _imErrorMsg b
      && _imVouchers a == _imVouchers b
      && _imSelected a == _imSelected b

data ApiAccess = ApiAccess
  { retrieveVoucherPage :: Day -> Day -> Int -> IO VoucherList,
    retrieveInvoice     :: String -> IO Invoice
  }

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

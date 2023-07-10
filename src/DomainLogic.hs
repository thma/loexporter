module DomainLogic where

import DomainModel
import Data.Time.Calendar ( Day )
import Data.Csv
    ( defaultEncodeOptions,
      encodeWith,
      EncodeOptions(..),
      Quoting(QuoteMinimal) )
import Data.ByteString.Lazy (writeFile)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import Data.Char (ord)
import System.Directory ( getHomeDirectory )
import Control.Lens

import LexofficeApi ( getVoucherPage, getInvoice )

getCompleteVoucherList :: Day -> Day -> ByteString -> IO VoucherList
getCompleteVoucherList startDate toDate apiToken= do
  firstPage <- getVoucherPage startDate toDate apiToken 0 
  let numPages = firstPage ^. totalPages
  let firstPageContent = firstPage ^. content
  let otherPages = map (getVoucherPage startDate toDate apiToken) [1..numPages]
  otherPagesContent <- sequence otherPages
  let allVouchers = concat $ firstPageContent : map (^. content) otherPagesContent
  return $ VoucherList allVouchers numPages (length allVouchers)

searchVouchers :: Day -> Day -> T.Text -> ByteString -> IO VoucherList
searchVouchers startDate toDate filterName apiToken = do
  putStrLn $ "Searching all vouchers between: " <> show startDate <> " and " <> show toDate
  allVouchers <- getCompleteVoucherList startDate toDate apiToken
  if filterName == ""
    then return allVouchers
    else do
      putStrLn $ "Filtering vouchers by name: " <> show filterName
      let resultVouchers = filter (\v -> filterName `T.isInfixOf` (v ^. contactName)) (allVouchers ^. content)
      return (allVouchers & content .~ resultVouchers)

saveVoucher :: Voucher -> ByteString -> IO ()
saveVoucher voucher apiToken = do
  putStrLn $ "Saving voucher: " ++ T.unpack (voucher ^. voucherNumber)
  let vId = voucher ^. DomainModel.id
  invoice <- getInvoice apiToken (T.unpack vId) 
  let dnItems = map buildFlatItem (lineItems invoice)
  let header = "Artikel;Menge;Einheit;Währung;Einzelpreis netto;Einzelpreis brutto;MwSt %;Rabatt %;Position Summe netto\r\n"
  let csvData = header <> encodeWith encOptions dnItems
  fileName <- buildFilePath $ T.unpack (voucher ^. voucherNumber) ++ ".csv"
  Data.ByteString.Lazy.writeFile fileName csvData

saveAllVouchers :: [Voucher] -> ByteString -> IO ()
saveAllVouchers vouchers apiToken = do
  putStrLn "Saving all vouchers"
  allInvoices <- mapM (getInvoice apiToken . T.unpack . _vId) vouchers
  let vouchersAndInvoices = zip vouchers allInvoices
  let dnItems = map buildDenormalizedItem $ concatMap (\(voucher, invoice) -> map (voucher, ) (lineItems invoice)) vouchersAndInvoices
  let header = "Rechnungsnr.;Rech.-Datum;Kunde;Gesamt Brutto;Artikel;Menge;Einheit;Währung;Einzelpreis netto;Einzelpreis brutto;MwSt %;Rabatt %;Position Summe netto\r\n"
  let csvData = header <> encodeWith encOptions dnItems
  fileName <- buildFilePath "Alle Rechnungen.csv"
  Data.ByteString.Lazy.writeFile fileName csvData

buildFilePath :: String -> IO FilePath
buildFilePath fileName = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/Desktop/" ++ fileName

encOptions :: EncodeOptions
encOptions = defaultEncodeOptions {
      encDelimiter = fromIntegral (ord ';')
    , encUseCrLf   = True
    , encIncludeHeader = False
    , encQuoting = QuoteMinimal
    }  




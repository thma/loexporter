module DomainLogic
  ( getCompleteVoucherList,
    searchVouchers,
    saveVoucher,
    saveAllVouchers,
  )
where

import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (writeFile)
import           Data.Char            (ord)
import           Data.Csv             (EncodeOptions (..),
                                       Quoting (QuoteMinimal),
                                       defaultEncodeOptions, encodeWith)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TextEncoding
import           Data.Time.Calendar   (Day)
import           DomainModel
import           System.Directory     (getHomeDirectory)
import           UiModel              (ApiAccess (retrieveInvoice, retrieveVoucherPage),
                                       HasApiAccess (apiAccess))

getCompleteVoucherList :: Day -> Day -> ApiAccess -> IO VoucherList
getCompleteVoucherList startDate toDate apiAccess = do
  firstPage <- retrieveVoucherPage apiAccess startDate toDate 0
  let numPages = firstPage ^. totalPages
      firstPageContent = firstPage ^. content
      otherPages = map (retrieveVoucherPage apiAccess startDate toDate) [1 .. numPages]
  otherPagesContent <- sequence otherPages
  let allVouchers = concat $ firstPageContent : map (^. content) otherPagesContent
  return $ VoucherList allVouchers numPages (length allVouchers)

searchVouchers :: Day -> Day -> T.Text -> ApiAccess -> IO VoucherList
searchVouchers startDate toDate filterName apiAccess = do
  putStrLn $ "Searching all vouchers between: " <> show startDate <> " and " <> show toDate
  allVouchers <- getCompleteVoucherList startDate toDate apiAccess
  if filterName == ""
    then return allVouchers
    else do
      putStrLn $ "Filtering vouchers by name: " <> show filterName
      let resultVouchers = filter (\v -> filterName `T.isInfixOf` (v ^. contactName)) (allVouchers ^. content)
      return (allVouchers & content .~ resultVouchers)

saveVoucher :: Voucher -> ApiAccess -> IO ()
saveVoucher voucher apiAccess = do
  putStrLn $ "Saving voucher: " ++ T.unpack (voucher ^. voucherNumber)
  let vId = voucher ^. DomainModel.id
  invoice <- retrieveInvoice apiAccess (T.unpack vId)
  let dnItems = map buildFlatItem (lineItems invoice)
      header = "Artikel;Menge;Einheit;Währung;Einzelpreis netto;Einzelpreis brutto;MwSt %;Rabatt %;Position Summe netto\r\n"
      csvData = header <> encodeWith encOptions dnItems
  fileName <- buildFilePath $ T.unpack (voucher ^. voucherNumber) ++ ".csv"
  Data.ByteString.Lazy.writeFile fileName csvData

saveAllVouchers :: [Voucher] -> ApiAccess -> IO ()
saveAllVouchers vouchers apiAccess = do
  putStrLn "Saving all vouchers"
  allInvoices <- mapM (retrieveInvoice apiAccess . T.unpack . _vId) vouchers
  let vouchersAndInvoices = zip vouchers allInvoices
      dnItems = map buildDenormalizedItem $ concatMap (\(voucher, invoice) -> map (voucher,) (lineItems invoice)) vouchersAndInvoices
      header = "Rechnungsnr.;Rech.-Datum;Kunde;Gesamt Brutto;Artikel;Menge;Einheit;Währung;Einzelpreis netto;Einzelpreis brutto;MwSt %;Rabatt %;Position Summe netto\r\n"
      csvData = header <> encodeWith encOptions dnItems
  fileName <- buildFilePath "Alle Rechnungen.csv"
  Data.ByteString.Lazy.writeFile fileName csvData

buildFilePath :: String -> IO FilePath
buildFilePath fileName = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/Desktop/" ++ fileName

encOptions :: EncodeOptions
encOptions =
  defaultEncodeOptions
    { encDelimiter = fromIntegral (ord ';'),
      encUseCrLf = True,
      encIncludeHeader = False,
      encQuoting = QuoteMinimal
    }

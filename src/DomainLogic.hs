module DomainLogic
  ( getCompleteVoucherList,
    searchVouchers,
    retrieveVoucher,
    saveVoucher,
    saveAllVouchers,
  )
where

import           Codec.Xlsx
import           Control.Lens
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text             as T
import           Data.Time.Calendar    (Day)
import           Data.Time.Clock.POSIX
import           DomainModel
import           System.Directory      (getHomeDirectory)
import           System.Info           (os)
import           System.Process        (ProcessHandle, createProcess, shell)
import           UiModel
import           Data.Sequence (Seq, fromList)

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

retrieveVoucher :: Voucher -> ApiAccess -> PluMap -> IO (Seq FlatLineItem)
retrieveVoucher voucher apiAccess pluMap = do
  putStrLn $ "Retrieving voucher line items: " ++ T.unpack (voucher ^. voucherNumber) ++ "..."
  invoice <- retrieveInvoice apiAccess (T.unpack $ voucher ^. DomainModel.id)
  let items = fromList $ map (buildFlatItem pluMap) (DomainModel.lineItems invoice)
  putStrLn $ "...Found " ++ show (length items) ++ " line items"
  return items

saveVoucher :: Voucher -> ApiAccess -> PluMap -> IO ()
saveVoucher voucher apiAccess pluMap = do
  putStrLn $ "Saving voucher: " ++ T.unpack (voucher ^. voucherNumber)
  ct <- getPOSIXTime
  invoice <- retrieveInvoice apiAccess (T.unpack $ voucher ^. DomainModel.id)
  let items = map (buildFlatItem pluMap) (DomainModel.lineItems invoice)
      sheet =
        def
          & addHeaders ["Artikel", "Art-Nr.", "Menge", "Einheit", "Währung", "Einzelpreis netto", "Einzelpreis brutto", "MwSt %", "Rabatt %", "Position Summe netto"]
          & addItems 2 items
      xlsx = def & atSheet ("Rechnung " <> (voucher ^. voucherNumber)) ?~ sheet
  file <- buildFilePath $ T.unpack (voucher ^. voucherNumber) ++ ".xlsx"
  LBS.writeFile file $ fromXlsx ct xlsx
  openWorksheet file
  where
    addItems :: Int -> [FlatLineItem] -> Worksheet -> Worksheet
    addItems line items sheet = foldl (\s (i, item) -> addSingleItem (line + i) item s) sheet (zip [0 ..] items)

    addSingleItem :: Int -> FlatLineItem -> Worksheet -> Worksheet
    addSingleItem ri item sheet =
      sheet
        & cellValueAt (line, 1) ?~ CellText (T.pack $ flatLineItemName item)
        & cellValueAt (line, 2) ?~ CellText (T.pack $ flatLineItemPLU item)
        & cellValueAt (line, 3) ?~ CellDouble (flatLineItemQuantity item)
        & cellValueAt (line, 4) ?~ CellText (T.pack $ flatLineItemUnitName item)
        & cellValueAt (line, 5) ?~ CellText (T.pack $ flatLineItemUnitPriceCurrency item)
        & cellValueAt (line, 6) ?~ CellDouble (flatLineItemUnitPriceNetAmount item)
        & cellValueAt (line, 7) ?~ CellDouble (flatLineItemUnitPriceGrossAmount item)
        & cellValueAt (line, 8) ?~ CellDouble (flatLineItemUnitPriceTaxRatePercentage item)
        & cellValueAt (line, 9) ?~ CellDouble (flatLineItemDiscountPercentage item)
        & cellValueAt (line, 10) ?~ CellDouble (flatLineItemAmount item)
      where
        line = RowIndex ri

addHeaders :: [T.Text] -> Worksheet -> Worksheet
addHeaders headers sheet = foldl (\s (i, header) -> addSingleHeader (i + 1) header s) sheet (zip [0 ..] headers)
  where
    addSingleHeader :: Int -> T.Text -> Worksheet -> Worksheet
    addSingleHeader ci header sheet = sheet & cellValueAt (1, col) ?~ CellText header
      where col = ColumnIndex ci

saveAllVouchers :: [Voucher] -> ApiAccess -> PluMap -> IO ()
saveAllVouchers vouchers apiAccess pluMap = do
  putStrLn "Saving all vouchers"
  ct <- getPOSIXTime
  allInvoices <- mapM (retrieveInvoice apiAccess . T.unpack . _vId) vouchers
  let vouchersAndInvoices = zip vouchers allInvoices
      items = map (buildDenormalizedItem pluMap) $ concatMap (\(voucher, invoice) -> map (voucher,) (lineItems invoice)) vouchersAndInvoices
      sheet =
        def
          & addHeaders ["Rechnungsnr.", "Rech.-Datum", "Kunde", "Gesamt Brutto", "Artikel", "Art-Nr.", "Menge", "Einheit", "Währung", "Einzelpreis netto", "Einzelpreis brutto", "MwSt %", "Rabatt %", "Position Summe netto"]
          & addItems 2 items
      xlsx = def & atSheet "Alle Rechnungen" ?~ sheet
  file <- buildFilePath "Alle Rechnungen.xlsx"
  LBS.writeFile file $ fromXlsx ct xlsx
  where
    addItems :: Int -> [DenormalizedItem] -> Worksheet -> Worksheet
    addItems line items sheet = foldl (\s (i, item) -> addSingleItem (line + i) item s) sheet (zip [0 ..] items)

    addSingleItem :: Int -> DenormalizedItem -> Worksheet -> Worksheet
    addSingleItem ri item sheet =
      sheet
        & cellValueAt (line, 1) ?~ CellText (T.pack $ vNumber item)
        & cellValueAt (line, 2) ?~ CellText (T.pack $ vDate item)
        & cellValueAt (line, 3) ?~ CellText (T.pack $ customerName item)
        & cellValueAt (line, 4) ?~ CellDouble (total item)
        & cellValueAt (line, 5) ?~ CellText (T.pack $ itemName item)
        & cellValueAt (line, 6) ?~ CellText (T.pack $ itemPLU item)
        & cellValueAt (line, 7) ?~ CellDouble (itemQuantity item)
        & cellValueAt (line, 8) ?~ CellText (T.pack $ itemUnitName item)
        & cellValueAt (line, 9) ?~ CellText (T.pack $ unitPriceCurrency item)
        & cellValueAt (line, 10) ?~ CellDouble (unitPriceNetAmount item)
        & cellValueAt (line, 11) ?~ CellDouble (unitPriceGrossAmount item)
        & cellValueAt (line, 12) ?~ CellDouble (unitPriceTaxRatePercentage item)
        & cellValueAt (line, 13) ?~ CellDouble (itemDiscountPercentage item)
        & cellValueAt (line, 14) ?~ CellDouble (itemAmount item)
      where 
        line = RowIndex ri

buildFilePath :: String -> IO FilePath
buildFilePath fileName = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/Desktop/" ++ fileName

openWorksheet :: FilePath -> IO ()
openWorksheet file = do
  let openCmd = case os of
        "mingw32" -> "start"
        "darwin"  -> "open"
        _         -> "xdg-open"
  createProcess (shell $ openCmd ++ " " ++ file)
  return ()

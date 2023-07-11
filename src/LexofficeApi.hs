module LexofficeApi 
  ( getVoucherPage,
    getInvoice,
  )
where

import           Data.ByteString     (ByteString)
import           Data.Time.Calendar  (Day, showGregorian)
import           DomainModel         (Invoice, VoucherList)
import           Network.HTTP.Simple (RequestHeaders, Response, getResponseBody,
                                      httpJSON, parseRequest, setRequestHeaders)

getVoucherPage :: ByteString -> Day -> Day -> Int -> IO VoucherList
getVoucherPage apiToken startDate toDate pageId = do
  let fromDateAsString = showGregorian startDate
      toDateAsString = showGregorian toDate
      url = voucherListUrl ++ fromDateAsString ++ "&voucherDateTo=" ++ toDateAsString ++ "&page=" ++ show pageId
  request <- setRequestHeaders (httpHeadersWith apiToken) <$> parseRequest url
  response <- httpJSON request :: IO (Response VoucherList)
  return $ getResponseBody response

getInvoice :: ByteString -> String -> IO Invoice
getInvoice apiToken invoiceId = do
  let url = invoiceUrl ++ invoiceId
  request <- setRequestHeaders (httpHeadersWith apiToken) <$> parseRequest url
  response <- httpJSON request :: IO (Response Invoice)
  return $ getResponseBody response

voucherListUrl :: String
voucherListUrl = "https://api.lexoffice.io/v1/voucherlist?voucherType=invoice&voucherStatus=any&voucherDateFrom="

invoiceUrl :: String
invoiceUrl = "https://api.lexoffice.io/v1/invoices/"

httpHeadersWith :: ByteString -> RequestHeaders
httpHeadersWith apiToken =
  [ ("Authorization", "Bearer " <> apiToken),
    ("Accept", "application/json"),
    ("Content-Type", "application/json")
  ]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Data.Default
import Data.Maybe ( fromMaybe, isJust )
import Data.Text (Text)
import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Data.Time.Calendar ( addDays )

import System.Directory ( getHomeDirectory )
import qualified Data.ByteString.UTF8 as BSU

import qualified Data.Text as T
import DomainModel
import UiModel
import DomainLogic
import Monomer

import qualified Monomer.Lens as L

type InvoiceWenv = WidgetEnv InvoiceModel InvoiceEvt 
type InvoiceNode = WidgetNode InvoiceModel InvoiceEvt

invoiceRow :: InvoiceWenv -> Voucher -> InvoiceNode
invoiceRow wenv voucher = row where
  rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
  
  rowContent voucher = 
    hstack [
      vstack [ label_ (voucher ^. contactName) [resizeFactor 1]
            `styleBasic` [textFont "Medium", textSize 16],
        spacer,
        hstack [
          label_ (T.pack $ take 10 $  T.unpack (voucher ^. voucherDate)) [resizeFactor 1]
            `styleBasic` [textSize 14],
          spacer,
          label_ (voucher ^. voucherNumber) [resizeFactor 1]
            `styleBasic` [textFont "Medium", textSize 14]
        ]
      ],
      filler,
      vstack [
        label_ (T.pack $ show (voucher ^. totalAmount) ++ " €")  [resizeFactor 1]
          `styleBasic` [textSize 14],
        spacer
      ]
    ]

  row = box_ cfg cont `styleBasic` [padding 10, paddingT 0] where
    cfg = [expandContent, onClick (InvoiceShowDetails voucher)]
    cont = rowContent voucher
      `styleBasic` [height 70, padding 10, radius 5]
      `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

buildUI
  :: WidgetEnv InvoiceModel InvoiceEvt
  -> InvoiceModel
  -> WidgetNode InvoiceModel InvoiceEvt
buildUI wenv model = widgetTree where
  sectionBgColor = wenv ^. L.theme . L.sectionColor

  errorOverlay = alertMsg msg InvoiceCloseError where
    msg = fromMaybe "" (model ^. errorMsg)

  searchOverlay = box content `styleBasic` [bgColor (darkGray & L.a .~ 0.8)] where
    content = label "Daten werden geladen..." `styleBasic` [textSize 20, textColor black]

  searchForm = keystroke [("Enter", InvoiceSearch)] $ vstack [
      hstack [
        label "Von:", spacer, dateField queryFrom `nodeKey` "queryFrom",
        spacer,
        label "Bis:", spacer, dateField queryTo `nodeKey` "queryTo",
        spacer,
        label "Kunde:", spacer, textField queryFilter `nodeKey` "queryFilter",
        spacer,
        mainButton "Daten abrufen" InvoiceSearch,
        spacer,
        button "Alle Rechnungen exportieren" InvoiceSaveAll,
        spacer
      ] `styleBasic` [bgColor sectionBgColor, padding 25]
    ]

  countLabel = label caption `styleBasic` [padding 10] where
    caption = "Rechnungen gefunden(" <> showt (length $ model ^. vouchers) <> ")"

  invoicesChanged wenv old new = old ^. vouchers /= new ^. vouchers

  widgetTree = zstack [
      vstack [
        searchForm,
        countLabel,
        box_ [mergeRequired invoicesChanged] $
          vscroll (vstack (invoiceRow wenv <$> model ^. vouchers)) `nodeKey` "mainScroll"
      ],
      errorOverlay `nodeVisible` isJust (model ^. errorMsg),
      searchOverlay `nodeVisible` model ^. searching
    ]

showt = T.pack . show

handleEvent
  :: WidgetEnv InvoiceModel InvoiceEvt
  -> WidgetNode InvoiceModel InvoiceEvt
  -> InvoiceModel
  -> InvoiceEvt
  -> [AppEventResponse InvoiceModel InvoiceEvt]
handleEvent wenv node model evt = case evt of
  InvoiceInit -> [
    Task $ toInVoiceSearchResult $ searchVouchers (model ^. queryFrom) (model ^. queryTo) (model ^. queryFilter) (model ^. apiToken),
    SetFocusOnKey "queryFrom"]
  InvoiceSearch -> [
    Model $ model & searching .~ True,
    Task $ toInVoiceSearchResult $ searchVouchers (model ^. queryFrom) (model ^. queryTo) (model ^. queryFilter) (model ^. apiToken)
    ]
  InvoiceSearchResult resp -> [
    Message "mainScroll" ScrollReset,
    Model $ model
      & searching .~ False
      & errorMsg .~ Nothing
      & vouchers .~ resp ^. content 
    ]
  InvoiceSearchError msg -> [
    Model $ model
      & searching .~ False
      & errorMsg ?~ msg
      & vouchers .~ []
    ]
  InvoiceSaveAll -> [
    Model $ model & searching .~ True,
    Task $ toInvoiceCloseDetails $ saveAllVouchers (model ^. vouchers) (model ^. apiToken)
    ]
  InvoiceShowDetails voucher -> [
    Model $ model & searching .~ True,
    Task $ toInvoiceCloseDetails $ saveVoucher voucher (model ^. apiToken)
    ]
  InvoiceCloseDetails -> [
    Model $ model 
      & searching .~ False
      & selected .~ Nothing]
  InvoiceCloseError -> [Model $ model & errorMsg .~ Nothing]

toInvoiceCloseDetails :: IO () -> IO InvoiceEvt
toInvoiceCloseDetails action = action >> pure InvoiceCloseDetails

toInVoiceSearchResult :: IO VoucherList -> IO InvoiceEvt
toInVoiceSearchResult action = InvoiceSearchResult <$> action

main :: IO ()
main = do
  now <- getCurrentTime
  homeDir <- getHomeDirectory
  apiToken <- BSU.fromString <$> readFile (homeDir ++ "/loexporter/assets/apitoken.txt")
  let today = utctDay now
      initModel = InvoiceModel (addDays (-7) today) today "" apiToken False Nothing [] Nothing
      config = [
        appWindowState $ MainWindowNormal (1024, 800),
        appWindowTitle "Lexoffice Exporter",
        appWindowIcon $ T.pack $ homeDir ++ "/loexporter/assets/images/icon.png",
        appTheme customLightTheme,
        appFontDef "Regular" (T.pack $ homeDir ++ "/loexporter/assets/fonts/Roboto-Regular.ttf"),
        appFontDef "Medium"  (T.pack $ homeDir ++ "/loexporter/assets/fonts/Roboto-Medium.ttf"),
        appInitEvent InvoiceInit
        ]
  startApp initModel handleEvent buildUI config
    

customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

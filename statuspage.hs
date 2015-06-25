{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BasicPrelude
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import           Filesystem.Path.CurrentOS     (decodeString)
import           Options.Applicative           hiding (Failure, Success)
import qualified Prelude                       as P
import           System.Directory
import qualified System.FilePath               as FP
import           System.IO                     (stdout)
import           Text.Blaze                    (text)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5              as H hiding (map)

data ServiceInfo = ServiceInfo { sName        :: Text
                               , sStatus      :: Status
                               , sInformation :: Text } deriving (Show)

data Status = Success | Failure deriving (Show, Eq, Enum)


main :: IO ()
main = do
  out <- execParser $ info (helper <*> argument str (metavar "<outfolder>"))
         (fullDesc <> progDesc "Outputs a html statuspage from the service-checker output to stdout.")
  files <- getDirectoryContents out >>=
             return . map (out FP.</>) . filter (`notElem` [".", ".."])
  n <- forM files $
          \file -> ServiceInfo
                   <$> pure (T.pack $ FP.takeFileName file)
                   <*> (readFile (file </> "status")
                     >>= \x -> case x of
                       "0" -> return Success
                       "1" -> return Failure
                       otherwise -> error . T.unpack $
                         show file ++ " status has bad contents (" ++ x ++ ")")
                   <*> readFile (file </> "information")
  BL.hPutStr stdout . renderHtml $ statusTable n


statusTable :: [ServiceInfo] -> Html
statusTable sis = docTypeHtml $ table $ foldl1' (<>) $ map serviceInfoRow sis

serviceInfoRow :: ServiceInfo -> Html
serviceInfoRow si = tr $ do
    th.text $ sName si
    td.text.show $ sStatus si
    td.text $ sInformation si

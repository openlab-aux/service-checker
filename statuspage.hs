{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BasicPrelude
import           Data.Aeson                    (object, encode, toJSON, ToJSON, (.=))
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
import           Text.Blaze.Html5              as H hiding (map, object)

data ServiceInfo = ServiceInfo { sName        :: Text
                               , sStatus      :: Status
                               , sInformation :: Text } deriving (Show)

data Status = Success | Failure deriving (Show, Eq, Enum)

instance ToJSON ServiceInfo where
  toJSON (ServiceInfo name status info) = object [ "name" .= name
                                                 , "status" .= fromEnum status
                                                 , "information" .= info
                                                 ]

main :: IO ()
main = do
  (outFolder, pageFolder) <- execParser $ info (helper <*> optParser)
    (fullDesc <> progDesc "Outputs a html statuspage from the service-checker output to <pagefolder>.")
  failIfDoesNotExist outFolder
  failIfDoesNotExist pageFolder

  files <- getDirectoryContents outFolder >>=
             return . map (outFolder FP.</>) . filter (`notElem` [".", ".."])
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
  BL.writeFile (pageFolder </> "index.html") . renderHtml $ statusTable n
  BL.writeFile (pageFolder </> "status.json") $ encode n
  where optParser = (,)
          <$> argument str (metavar "<outfolder>")
          <*> argument str (metavar "<pagefolder>")


statusTable :: [ServiceInfo] -> Html
statusTable sis = docTypeHtml $ table $ foldl1' (<>) $ map serviceInfoRow sis

serviceInfoRow :: ServiceInfo -> Html
serviceInfoRow si = tr $ do
    th.text $ sName si
    td.text.show $ sStatus si
    td.text $ sInformation si

failIfDoesNotExist :: FilePath -> IO ()
failIfDoesNotExist fp = do
  file <- doesFileExist fp
  directory <- doesDirectoryExist fp
  when (not $ file || directory) $ error $ "'" ++ fp ++ "' does not exist."
  return ()

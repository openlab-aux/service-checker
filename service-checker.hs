{-# LANGUAGE OverloadedStrings #-}
#!/usr/bin/env runhaskell
module Main where

import BasicPrelude
import qualified Prelude as P
import Turtle
import Options.Applicative
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.Text as Text

import qualified System.Process as Process
import Control.Concurrent.Async (withAsync, concurrently)
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  (scripts, out) <-
    execParser $ info (helper <*> optParser)
      (fullDesc <> progDesc "Checks if services still work.")
  sh $ execScripts (fromString scripts) (fromString out)
    where
      optParser = (,)
          <$> argument str (metavar "<scriptfolder>")
          <*> argument str (metavar "<outfolder>")


execScripts :: FilePath -> FilePath -> Shell ()
execScripts scriptf outf = do
  script <- ls scriptf
  liftIO $
    let sof = outf </> filename script
    in do
      mktree sof
      (exCode, inf) <- procCollect (Text.pack $ encodeString script) [] mempty
      writeFile (sof </> "information") inf
      writeFile (sof </> "status") $ show.toNbr $ exCode
        where toNbr (ExitSuccess)   = 0
              toNbr (ExitFailure i) = i


{-| Run a command using @execvp@, retrieving the exit code and stdout as a
    non-lazy blob of Text

    The command inherits @stderr@ for the current process
-}
procCollect
    :: Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Text
    -- ^ Lines of standard input
    -> IO (ExitCode, Text)
    -- ^ Exit code and stdout
procCollect cmd args = system' (Process.proc (Text.unpack cmd) (map Text.unpack args))

system'
    :: Process.CreateProcess
    -- ^ Command
    -> Shell Text
    -- ^ Lines of standard input
    -> IO (ExitCode, Text)
    -- ^ Exit code and stdout
system' p s = do
    let p' = p
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.Inherit
            }
    (Just hIn, Just hOut, Nothing, ph) <- liftIO (Process.createProcess p')
    let feedIn = sh (do
            txt <- s
            liftIO (Text.hPutStrLn hIn txt) )
    concurrently
      (withAsync feedIn (\_ -> liftIO (Process.waitForProcess ph) ))
      (Text.hGetContents hOut)

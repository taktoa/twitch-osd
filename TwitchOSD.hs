{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Lens              as Lens

import qualified Libnotify                 as LN

import qualified Network.WebSockets        as WS
import qualified Wuss                      as WSS

import           Network.Wreq              (FormParam ((:=)))
import qualified Network.Wreq              as Wreq

import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS

import           Control.Concurrent        (forkIO)
import           Control.Monad
import           Control.Monad.Extra       (whileM)

import qualified Filesystem.Path           as FP
import qualified Filesystem.Path.CurrentOS as FP

import qualified Turtle

import           Flow                      ((.>), (|>))

import qualified Control.Foldl             as Foldl

import           Control.Monad.IO.Class    (MonadIO (liftIO))

import           Data.Aeson                as Aeson

import           Data.Maybe
import           Data.Monoid

import           System.Exit               (ExitCode (ExitFailure, ExitSuccess))

-- Send PING every 5 minute
-- If we don't receive a PONG within 10 seconds, we should reconnect
-- LISTEN

gpgDecrypt :: FP.FilePath -> IO ByteString
gpgDecrypt path = do
  pathText <- FP.toText path
              |> either (show .> fail) pure
  (exitCode, stdout, stderr) <- do
    Turtle.procStrictWithErr "gpg2" ["-d", pathText] (Turtle.select [])
  case exitCode of
    ExitSuccess      -> pure (Text.encodeUtf8 stdout)
    ExitFailure code -> [ "GPG failed with exit code ", show code, "\n"
                        , "It produced the following text on stderr:\n"
                        , Text.unpack stderr
                        ] |> mconcat |> fail

data APIKey
  = APIKey
    { apiKeyClientID :: !Text
    , apiKeySecret   :: !Text
    }
  deriving ()

instance FromJSON APIKey where
  parseJSON = withObject "APIKey" $ \o -> do
    apiKeyClientID <- o .: "id"
    apiKeySecret   <- o .: "secret"
    pure (APIKey {..})

getAPIKey :: IO APIKey
getAPIKey = do
  home <- Turtle.need "HOME"
          >>= maybe (fail "HOME is not set!") pure
  stdout <- gpgDecrypt (FP.fromText (home <> "/.config/twitch/api-key.gpg"))
  Aeson.decodeStrict stdout
    |> maybe (fail "Failed to decode JSON in api-key.gpg") pure

data AppAccessToken
  = AppAccessToken
    { appAccessTokenAccessToken  :: !Text
    , appAccessTokenRefreshToken :: !Text
    , appAccessTokenExpiresIn    :: !Int
    , appAccessTokenScope        :: ![Text]
    }
  deriving ()

instance FromJSON AppAccessToken where
  parseJSON = withObject "AppAccessToken" $ \o -> do
    appAccessTokenAccessToken  <- o .: "access_token"
    appAccessTokenRefreshToken <- o .: "refresh_token"
    appAccessTokenExpiresIn    <- o .: "expires_in"
    appAccessTokenScope        <- o .: "scope"
    pure (AppAccessToken {..})

-- POST https://api.twitch.tv/kraken/oauth2/token
--          ?client_id=...
--          &client_secret=...
--          &grant_type=client_credentials

twitchAuthenticate :: APIKey -> IO AppAccessToken
twitchAuthenticate apiKey = do
  let opts = Wreq.defaults
             |> Lens.set (Wreq.param "client_id")     [apiKeyClientID apiKey]
             |> Lens.set (Wreq.param "client_secret") [apiKeySecret apiKey]
             |> Lens.set (Wreq.param "grant_type")    ["client_credentials"]
  let body = "" :: Text
  -- r <- Wreq.postWith opts "https://api.twitch.tv/kraken/oauth2/token" body
  pure _

ws :: APIKey -> WS.Connection -> IO ()
ws apiKey connection = do
  putStrLn "Connected!"

  void $ forkIO $ forever $ do
    message <- WS.receiveData connection
    print (message :: Text)

  whileM $ do
    line <- getLine
    unless (null line) $ do
      WS.sendTextData connection (Text.pack line)
    pure (not (null line))

  WS.sendClose connection (Text.pack "Bye!")

main :: IO ()
main = do
  apiKey <- getAPIKey
  WSS.runSecureClient "pubsub-edge.twitch.tv" 443 "/" (ws apiKey)

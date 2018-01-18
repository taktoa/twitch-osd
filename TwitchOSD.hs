--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

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

import           Control.Exception         (assert)

import           System.Exit               (ExitCode (ExitFailure, ExitSuccess))

import           TwitchOSD.Scope           (Scope)
import qualified TwitchOSD.Scope           as Scope

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data AppAccessToken
  = AppAccessToken
    { appAccessTokenAccessToken  :: !Text
    , appAccessTokenRefreshToken :: !Text
    , appAccessTokenExpiresIn    :: !Int
    , appAccessTokenScopes       :: ![Scope]
    }
  deriving ()

instance FromJSON AppAccessToken where
  parseJSON = withObject "AppAccessToken" $ \o -> do
    appAccessTokenAccessToken  <- o .: "access_token"
    appAccessTokenRefreshToken <- o .: "refresh_token"
    appAccessTokenExpiresIn    <- o .: "expires_in"
    appAccessTokenScopes       <- o .: "scope"
    pure (AppAccessToken {..})

twitchAuthenticate :: APIKey -> [Scope] -> IO AppAccessToken
twitchAuthenticate apiKey scopes = do
  let scopeText = Text.intercalate " " (map Scope.scopeToText scopes)
  let opts = Wreq.defaults
             |> Lens.set (Wreq.param "client_id")     [apiKeyClientID apiKey]
             |> Lens.set (Wreq.param "client_secret") [apiKeySecret apiKey]
             |> Lens.set (Wreq.param "grant_type")    ["client_credentials"]
             |> Lens.set (Wreq.param "scope")         [scopeText]
  let body = "" :: ByteString
  r <- Wreq.postWith opts "https://api.twitch.tv/kraken/oauth2/token" body
  result <- Aeson.decode (Lens.view Wreq.responseBody r)
            |> maybe (fail "Failed to decode response JSON") pure
  assert (appAccessTokenScopes result == scopes) (pure result)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  apiKey <- getAPIKey
  appAccessToken <- twitchAuthenticate apiKey [] -- FIXME: pick scopes
  undefined

--------------------------------------------------------------------------------

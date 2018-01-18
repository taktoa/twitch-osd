--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module TwitchOSD.Scope where

--------------------------------------------------------------------------------

import qualified Data.Aeson as Aeson

import           Data.Text  (Text)

import           Flow

--------------------------------------------------------------------------------

-- | Twitch OAuth scopes
data Scope
  = ClipsEdit                -- ^ @clips:edit@
  | UserEdit                 -- ^ @user:edit@
  | UserReadEmail            -- ^ @user:read:email@
  | ChannelCheckSubscription -- ^ @channel_check_subscription@
  | ChannelCommercial        -- ^ @channel_commercial@
  | ChannelEditor            -- ^ @channel_editor@
  | ChannelFeedEdit          -- ^ @channel_feed_edit@
  | ChannelFeedRead          -- ^ @channel_feed_read@
  | ChannelRead              -- ^ @channel_read@
  | ChannelStream            -- ^ @channel_stream@
  | ChannelSubscriptions     -- ^ @channel_subscriptions@
  | ChatLogin                -- ^ @chat_login@
  | CollectionsEdit          -- ^ @collections_edit@
  | CommunitiesEdit          -- ^ @communities_edit@
  | CommunitiesModerate      -- ^ @communities_moderate@
  | OpenID                   -- ^ @openid@
  | UserBlocksEdit           -- ^ @user_blocks_edit@
  | UserBlocksRead           -- ^ @user_blocks_read@
  | UserFollowsEdit          -- ^ @user_follows_edit@
  | UserRead                 -- ^ @user_read@
  | UserSubscriptions        -- ^ @user_subscriptions@
  | ViewingActivityRead      -- ^ @viewing_activity_read@
  deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------

scopeToText :: Scope -> Text
scopeToText ClipsEdit                = "clips:edit"
scopeToText UserEdit                 = "user:edit"
scopeToText UserReadEmail            = "user:read:email"
scopeToText ChannelCheckSubscription = "channel_check_subscription"
scopeToText ChannelCommercial        = "channel_commercial"
scopeToText ChannelEditor            = "channel_editor"
scopeToText ChannelFeedEdit          = "channel_feed_edit"
scopeToText ChannelFeedRead          = "channel_feed_read"
scopeToText ChannelRead              = "channel_read"
scopeToText ChannelStream            = "channel_stream"
scopeToText ChannelSubscriptions     = "channel_subscriptions"
scopeToText ChatLogin                = "chat_login"
scopeToText CollectionsEdit          = "collections_edit"
scopeToText CommunitiesEdit          = "communities_edit"
scopeToText CommunitiesModerate      = "communities_moderate"
scopeToText OpenID                   = "openid"
scopeToText UserBlocksEdit           = "user_blocks_edit"
scopeToText UserBlocksRead           = "user_blocks_read"
scopeToText UserFollowsEdit          = "user_follows_edit"
scopeToText UserRead                 = "user_read"
scopeToText UserSubscriptions        = "user_subscriptions"
scopeToText ViewingActivityRead      = "viewing_activity_read"

--------------------------------------------------------------------------------

scopeFromText :: Text -> Maybe Scope
scopeFromText "clips:edit"                 = Just ClipsEdit
scopeFromText "user:edit"                  = Just UserEdit
scopeFromText "user:read:email"            = Just UserReadEmail
scopeFromText "channel_check_subscription" = Just ChannelCheckSubscription
scopeFromText "channel_commercial"         = Just ChannelCommercial
scopeFromText "channel_editor"             = Just ChannelEditor
scopeFromText "channel_feed_edit"          = Just ChannelFeedEdit
scopeFromText "channel_feed_read"          = Just ChannelFeedRead
scopeFromText "channel_read"               = Just ChannelRead
scopeFromText "channel_stream"             = Just ChannelStream
scopeFromText "channel_subscriptions"      = Just ChannelSubscriptions
scopeFromText "chat_login"                 = Just ChatLogin
scopeFromText "collections_edit"           = Just CollectionsEdit
scopeFromText "communities_edit"           = Just CommunitiesEdit
scopeFromText "communities_moderate"       = Just CommunitiesModerate
scopeFromText "openid"                     = Just OpenID
scopeFromText "user_blocks_edit"           = Just UserBlocksEdit
scopeFromText "user_blocks_read"           = Just UserBlocksRead
scopeFromText "user_follows_edit"          = Just UserFollowsEdit
scopeFromText "user_read"                  = Just UserRead
scopeFromText "user_subscriptions"         = Just UserSubscriptions
scopeFromText "viewing_activity_read"      = Just ViewingActivityRead
scopeFromText _                            = Nothing

--------------------------------------------------------------------------------

instance Aeson.ToJSON Scope where
  toJSON = scopeToText .> Aeson.toJSON

instance Aeson.FromJSON Scope where
  parseJSON = Aeson.withText "Scope" $ \t -> do
    scopeFromText t
      |> maybe (fail ("Failed to decode Scope: " ++ show t)) pure

--------------------------------------------------------------------------------

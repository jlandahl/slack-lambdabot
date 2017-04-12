{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens (use)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum)
import Data.Functor (void)
import Data.Monoid ((<>))
import Data.Text (Text, dropWhile, pack, stripPrefix, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Lambdabot.Main hiding (commandPrefixes)
import Lambdabot.Plugin.Haskell (languageExts, trustedPackages)
import Modules (modulesInfo)
import Prelude hiding (dropWhile, filter)
import System.IO.Silently (capture)
import Web.Slack (Event(Message), SlackBot, getId, runBot, selfUserId, session, slackSelf)
import Web.Slack.Message (sendMessage)

import Config ( BotConfig (BotConfig)
              , HaskellConfig (HaskellConfig)
              , _commandPrefix
              , _evalPrefix
              , _haskellConfig
              , _languageExtensions
              , _outputDirectory
              , _slackConfig
              , _trustedPackages
              , loadConfig)

-------------------------------------------------------------------------------
-- Lambdabot
-------------------------------------------------------------------------------

-- | Run one or more commands against Lambdabot and capture the response.
lambdabot :: BotConfig -> Text -> IO String
lambdabot BotConfig { _haskellConfig = HaskellConfig {..}, ..} command = do
  let config = [ outputDir ==> _outputDirectory
               , languageExts ==> _languageExtensions
               , trustedPackages ==> _trustedPackages
               , onStartupCmds ==> [unpack command]
               ]
      request = void $ lambdabotMain modulesInfo config
  (response, _) <- capture request
  return response

-------------------------------------------------------------------------------
-- Slack
-------------------------------------------------------------------------------

-- | Construct a @SlackBot@ from a name. This bot will pass messages addressed
-- to it to 'lambdabot' and relay 'lambdabot''s response.
slackBot :: BotConfig -> SlackBot a
slackBot botConfig@BotConfig { _haskellConfig = haskellConfig
                             , _commandPrefix = commandPrefix }
         (Message cid _ message _ _ _) = do
  myId <- use $ session . slackSelf . selfUserId . getId
  let idPrefix = "<@" <> myId <> ">"
      evalPrefix = pack $ _evalPrefix haskellConfig
      message' = case stripPrefix evalPrefix message of
                   Just m -> Just $ "run " <> m
                   Nothing -> msum $ flip stripPrefix message
                                  <$> [idPrefix, pack commandPrefix]
  case dropWhile (== ' ') <$> message' of
    Just rawMessage -> do
      let decodedMessage = decodeHtml rawMessage
      liftIO $ print decodedMessage
      rawResponse <- liftIO $ pack <$> lambdabot botConfig decodedMessage
      let response = "```\n" <> rawResponse <> "```"
      liftIO $ print response
      sendMessage cid response
    _ ->
      return ()
slackBot _ _ = return ()

decodeHtml :: Text -> Text
decodeHtml = toStrict . toLazyText . htmlEncodedText

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  config <- loadConfig "slack-lambdabot.conf"
  runBot (_slackConfig config) (slackBot config) ()

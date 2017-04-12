{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Configurator (Worth (Required), load, require, subconfig)
import Data.Configurator.Types (Config)
import Web.Slack (SlackConfig (SlackConfig))

data BotConfig = BotConfig
  { _outputDirectory :: String
  , _commandPrefix :: String
  , _slackConfig :: SlackConfig
  , _haskellConfig :: HaskellConfig
  }

data HaskellConfig = HaskellConfig
  { _evalPrefix :: String
  , _languageExtensions :: [String]
  , _trustedPackages :: [String]
  }

loadConfig :: FilePath -> IO BotConfig
loadConfig path = load [Required path] >>= loadConfig'

loadConfig' :: Config -> IO BotConfig
loadConfig' config = do
  outputDirectory <- require config "outputDirectory"
  commandPrefix <- require config "commandPrefix"
  slackConfig <- loadSlackConfig $ subconfig "slack" config
  haskellConfig <- loadHaskellConfig $ subconfig "haskell" config
  return BotConfig { _outputDirectory = outputDirectory
                   , _commandPrefix = commandPrefix
                   , _slackConfig = slackConfig
                   , _haskellConfig = haskellConfig
                   }

loadSlackConfig :: Config -> IO SlackConfig
loadSlackConfig config = SlackConfig <$> require config "apiToken"

loadHaskellConfig :: Config -> IO HaskellConfig
loadHaskellConfig config = do
  evalPrefix <- require config "evalPrefix"
  languageExtensions <- require config "languageExtensions"
  trustedPackages <- require config "trustedPackages"
  return HaskellConfig { _evalPrefix = evalPrefix
                       , _languageExtensions = languageExtensions
                       , _trustedPackages = trustedPackages }
